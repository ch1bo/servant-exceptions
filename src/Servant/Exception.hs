{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeOperators             #-}

module Servant.Exception
  ( Throws
  , ToServantErr(..)
  , toServantErr
  , toServantErrJSON
  , toServantErrPlain
  , ServantException
  , toServantException
  , fromServantException
  , handleServantExceptions
  , handleServantExceptionsWith
  , Exception(..)
  , mapException
  ) where

import Control.Monad.Catch       (Exception (..), MonadCatch, SomeException, catch, throwM)
import Control.Monad.Error.Class (MonadError (..))
import Data.Aeson                (ToJSON (..), encode, object, (.=))
import Data.Proxy                (Proxy (..))
import Data.String               (fromString)
import Data.Text                 (Text)
import Data.Typeable             (Typeable, cast, typeOf)
import GHC.TypeLits              (Nat)
import Network.HTTP.Types        (Header, Status (..))
import Servant                   hiding (Header)
import Servant.API.ContentTypes  (JSON, MimeRender (..), PlainText)

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

-- * Type level annotated exception handling

data Throws

instance HasServer (Verb mt st ct a) context =>
         HasServer (Throws :> Verb (mt :: k) (st :: Nat) (ct :: [*]) (a :: *)) context where
  type ServerT (Throws :> Verb mt st ct a) m =
       ServerT (Verb mt st ct a) m

  route _ ctx del = route (Proxy :: Proxy (Verb mt st ct a)) ctx (handleServantExceptions <$> del)

-- * Content-type aware servant error encoding

class (Typeable e, Show e) => ToServantErr e where
  status :: e -> Status

  message :: e -> Text
  message = fromString . show

  headers :: e -> [Header]
  headers _ = []

toServantErr :: (MimeRender ct e, ToServantErr e) => Proxy ct -> e -> ServantErr
toServantErr ct e = ServantErr { errHTTPCode = statusCode $ status e
                               , errReasonPhrase = reasonPhrase $ status e
                               , errBody = mimeRender ct e
                               , errHeaders = headers e
                               }
 where
  reasonPhrase = Text.unpack . Text.decodeUtf8 . statusMessage

toServantErrJSON :: (MimeRender JSON e, ToServantErr e) => e -> ServantErr
toServantErrJSON = toServantErr (Proxy :: Proxy JSON)

toServantErrPlain :: (MimeRender PlainText e, ToServantErr e) => e -> ServantErr
toServantErrPlain = toServantErr (Proxy :: Proxy PlainText)

-- * Servant exception handling

-- | A root exception type specifically targeted for servant as it can be
-- converted to a @ServantErr@ with content-type @JSON@ or @PlainText@.
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

-- | Catch all @ServantException@ and convert them to @ServantErr@ using @toServantErrJSON@.
handleServantExceptions :: (MonadCatch m, MonadError ServantErr m) => m a -> m a
handleServantExceptions = handleServantExceptionsWith toServantErrJSON

-- | Catch all @ServantException@ and convert them to @ServantErr@ using given function.
handleServantExceptionsWith :: (MonadCatch m, MonadError ServantErr m)
                            => (ServantException -> ServantErr) -> m a -> m a
handleServantExceptionsWith f a = a `catch` \(e :: ServantException) -> throwError $ f e

-- * Exception utilities

-- | Catch and rethrow using mapping function @f@.
mapException :: (Exception e1, Exception e2, MonadCatch m) => (e1 -> e2) -> m a -> m a
mapException f a = a `catch` (throwM . f)
