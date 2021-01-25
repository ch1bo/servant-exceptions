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

module Servant.Exception.Client where

import Control.Exception        (Exception)
import Data.Kind                (Type)
import Data.Proxy               (Proxy (..))
import Data.Sequence            (fromList)
import GHC.TypeLits             (Nat)
import Network.HTTP.Media       (MediaType)
import Servant.API              hiding (Header)
import Servant.API.ContentTypes (AllMimeUnrender)
import Servant.Client.Core
import Servant.Exception        (Throws, ToServantErr (..))

import           Control.Monad                 (unless)
import           Data.Foldable                 (Foldable (toList))
import qualified Data.Text                     as Text
import           Debug.Trace                   (trace)
import           Network.HTTP.Media.Accept     (Accept (matches, parseAccept))
import           Network.HTTP.Media.MediaType  ((//))
import           Servant.Client.Core.RunClient (runRequest)

-- * Type level annotated exception handling

instance ( Exception e
         , ToServantErr e
         , MimeUnrender ct a
         , cts ~ (ct ': cts') -- REVIEW understand this fully. use first?
         , ReflectMethod mt
         , RunClient m
         , HasClient m (Verb mt st cts a)
         ) => HasClient m (Throws e :> Verb (mt :: k) (st :: Nat) (cts :: [Type]) (a :: Type)) where

  type Client m (Throws e :> Verb mt st cts a) = m a

  clientWithRoute pm Proxy req = do
    let req' = req { requestAccept = fromList $ toList accept
                   , requestMethod = method
                   }
    response <- trace ("accept: " <> show (requestAccept req')) runRequest req'
    trace ("headers: " <> show (responseHeaders response))
      trace ("body: " <> show (responseBody response))
        response `decodedAs` (Proxy :: Proxy ct)
   where
    accept = contentTypes (Proxy :: Proxy ct)
    method = reflectMethod (Proxy :: Proxy mt)

  -- No change in client monad
  hoistClientMonad _ Proxy f ma = f ma

checkContentTypeHeader :: RunClient m => Response -> m MediaType
checkContentTypeHeader response =
  case lookup "Content-Type" $ toList $ responseHeaders response of
    Nothing -> return $ "application"//"octet-stream"
    Just t -> case parseAccept t of
      Nothing -> throwClientError $ InvalidContentTypeHeader response
      Just t' -> return t'

decodedAs :: forall ct a m. (MimeUnrender ct a, RunClient m)
  => Response -> Proxy ct -> m a
decodedAs response ct = do
  responseContentType <- checkContentTypeHeader response
  unless (any (matches responseContentType) accept) $
    throwClientError $ UnsupportedContentType responseContentType response
  case mimeUnrender ct $ responseBody response of
    Left err -> throwClientError $ DecodeFailure (Text.pack err) response
    Right val -> return val
  where
    accept = toList $ contentTypes ct

-- | Push @Throws@ further "upstream".
instance ( RunClient m
         , HasClient m next
         , next ~ (api :> Throws e :> upstream)
         ) => HasClient m (Throws e :> api :> upstream) where

  type Client m (Throws e :> api :> upstream) = Client m (api :> Throws e :> upstream)

  clientWithRoute pm Proxy req = clientWithRoute pm (Proxy :: Proxy next) req

  hoistClientMonad pm Proxy f cm = hoistClientMonad pm (Proxy :: Proxy next) f cm


-- | Transitive application of @Throws@ on @(:<|>)@.
instance ( RunClient m
         , HasClient m next
         , next ~ (Throws e :> api1 :<|> Throws e :> api2)
         ) => HasClient m (Throws e :> (api1 :<|> api2)) where

  type Client m (Throws e :> (api1 :<|> api2)) = Client m (Throws e :> api1 :<|> Throws e :> api2)

  clientWithRoute pm Proxy req = clientWithRoute pm (Proxy :: Proxy next) req

  hoistClientMonad pm Proxy f cm = hoistClientMonad pm (Proxy :: Proxy next) f cm
