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

data Throws (e :: *)

instance ( Exception e
         , ToServantErr e
         , ToJSON e
         , HasServer (Verb mt st ct a) context
         ) => HasServer (Throws e :> Verb (mt :: k) (st :: Nat) (ct :: [*]) (a :: *)) context where
  type ServerT (Throws e :> Verb mt st ct a) m =
       ServerT (Verb mt st ct a) m

  route _ ctx del = route (Proxy :: Proxy (Verb mt st ct a)) ctx (handleException <$> del)
   where
    handleException a = a `catch` \(e :: e) -> throwError $ toServantErrJSON e

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

-- * Exception utilities

-- | Catch and rethrow using mapping function @f@.
mapException :: (Exception e1, Exception e2, MonadCatch m) => (e1 -> e2) -> m a -> m a
mapException f a = a `catch` (throwM . f)
