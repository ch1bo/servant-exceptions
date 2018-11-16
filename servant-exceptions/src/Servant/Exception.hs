{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RecordWildCards           #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE UndecidableInstances      #-}

module Servant.Exception
  ( Throws
  , ToServantErr(..)
  , ServantException
  , toServantException
  , fromServantException
  , Exception(..)
  , mapException
  ) where

import Control.Monad.Catch                        (Exception (..), MonadCatch, SomeException, catch,
                                                   throwM)
import Control.Monad.Error.Class                  (MonadError (..))
import Data.Aeson                                 (ToJSON (..), encode, object, (.=))
import Data.Maybe                                 (fromMaybe)
import Data.Monoid                                ((<>))
import Data.Proxy                                 (Proxy (..))
import Data.String                                (fromString)
import Data.Text                                  (Text)
import Data.Typeable                              (Typeable, cast, typeOf)
import GHC.TypeLits                               (Nat)
import Network.HTTP.Media                         (mapAcceptMedia)
import Network.HTTP.Types                         (Header, Status (..), hAccept, hContentType)
import Network.Wai                                (requestHeaders)
import Servant                                    hiding (Header)
import Servant.API.ContentTypes                   (AllMimeRender (..), JSON, MimeRender (..),
                                                   PlainText)
import Servant.Server.Internal.RoutingApplication (Delayed (..))

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

-- * Conversion type class

class (Typeable e, Show e) => ToServantErr e where
  status :: e -> Status

  message :: e -> Text
  message = fromString . show

  headers :: e -> [Header]
  headers _ = []

-- * Type level annotated exception handling

data Throws (e :: *)

-- | Main @HasServer@ instance for @Throws e@. Catches exceptions of type @e@ in
-- the upstream server and encodes them using @ToServantErr@ and @MimeRender@.
instance ( Exception e
         , ToServantErr e
         , AllMimeRender ct e
         , HasServer (Verb mt st ct a) context
         ) => HasServer (Throws e :> Verb (mt :: k) (st :: Nat) (ct :: [*]) (a :: *)) context where

  type ServerT (Throws e :> Verb mt st ct a) m =
       ServerT (Verb mt st ct a) m

  route _ ctx del = route (Proxy :: Proxy (Verb mt st ct a)) ctx $ extendServer del
   where
    extendServer Delayed{..} =
      Delayed { serverD = \c p h a b req -> do
                  let accH = fromMaybe ("*" <> "/" <> "*") . lookup hAccept $ requestHeaders req
                  handleException (Proxy :: Proxy ct) accH <$> serverD c p h a b req
              , ..
              }

    handleException ct h a = a `catch` \(e :: e) -> do
      let body = mapAcceptMedia (allMimeRender ct e) h
      throwError ServantErr { errHTTPCode = statusCode $ status e
                            , errReasonPhrase = Text.unpack . Text.decodeUtf8 . statusMessage $ status e
                            , errBody = fromMaybe "" body -- AllMimeRender should prevent @Nothing@
                            , errHeaders = (hContentType, h) : headers e
                            }

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy (Verb mt st ct a))
#endif

-- | Push @Throws@ further "upstream".
instance HasServer (api :> Throws e :> upstream) context =>
         HasServer (Throws e :> api :> upstream) context where

  type ServerT (Throws e :> api :> upstream) m =
       ServerT (api :> Throws e :> upstream) m

  route _ = route (Proxy :: Proxy (api :> Throws e :> upstream))

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy (api :> Throws e :> upstream))
#endif

-- | Transitive application of @Throws@ on @(:<|>)@.
instance HasServer (Throws e :> api1 :<|> Throws e :> api2) context =>
         HasServer (Throws e :> (api1 :<|> api2)) context where

  type ServerT (Throws e :> (api1 :<|> api2)) m =
       ServerT (Throws e :> api1 :<|> Throws e :> api2) m

  route _ = route (Proxy :: Proxy (Throws e :> api1 :<|> Throws e :> api2))

#if MIN_VERSION_servant_server(0,12,0)
  hoistServerWithContext _ = hoistServerWithContext (Proxy :: Proxy (Throws e :> api1 :<|> Throws e :> api2))
#endif

-- * Exception utilities

-- | A root exception type (see "Control.Exception") to provide a common
-- rendering format via @MimeRender@ for builtin content types @JSON@ and
-- @PlainText@.
data ServantException = forall e. (Exception e, ToJSON e, ToServantErr e) => ServantException e
                      deriving (Typeable)

instance Show ServantException where
  show (ServantException e) = show e

instance Exception ServantException

instance MimeRender JSON ServantException where
  mimeRender _ (ServantException e) = encode $ object [ "type" .= errorType
                                                      , "message" .= message e
                                                      , "error" .= toJSON e
                                                      ]
   where
    errorType = show $ typeOf e

instance MimeRender PlainText ServantException where
  mimeRender ct = mimeRender ct . displayException

instance ToServantErr ServantException where
  status (ServantException e) = status e
  message (ServantException e) = message e

toServantException :: (Exception e, ToJSON e, ToServantErr e) => e -> SomeException
toServantException = toException . ServantException

fromServantException :: Exception e => SomeException -> Maybe e
fromServantException x = fromException x >>= \(ServantException e) -> cast e

-- | Catch and rethrow using mapping function @f@.
mapException :: (Exception e1, Exception e2, MonadCatch m) => (e1 -> e2) -> m a -> m a
mapException f a = a `catch` (throwM . f)
