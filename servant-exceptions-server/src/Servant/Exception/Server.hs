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

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Exception.Server
  ( Throws
  , ToServantErr(..)
  , ServantException
  , toServantException
  , fromServantException
  , Exception(..)
  , mapException
  ) where

import Servant.Exception                          (ToServantErr(..), Throws, ServantException, toServantException, fromServantException, mapException)

import Control.Monad.Catch                        (Exception (..), catch)
import Control.Monad.Error.Class                  (MonadError (..))
import Data.Maybe                                 (fromMaybe)
import Data.Monoid                                ((<>))
import Data.Proxy                                 (Proxy (..))
import GHC.TypeLits                               (Nat)
import Network.HTTP.Media                         (mapAcceptMedia)
import Network.HTTP.Types                         (Status (..), hAccept, hContentType)
import Network.Wai                                (requestHeaders)
import Servant                                    hiding (Header)
import Servant.API.ContentTypes                   (AllMimeRender (..))
import Servant.Server.Internal.RoutingApplication (Delayed (..))

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

-- * Type level annotated exception handling

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
