{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE UndecidableInstances      #-}

module Servant.Exception
  ( Throws,
    ToServantErr (..),
    ServantException,
    toServantException,
    fromServantException,
    Exception (..),
    mapException
  )
where

import Control.Monad.Catch      (Exception (..), MonadCatch, SomeException, catch, throwM)
import Data.Aeson               (ToJSON (..), encode, object, (.=))
import Data.Kind                (Type)
import Data.String              (fromString)
import Data.Text                (Text)
import Data.Typeable            (Typeable, cast, typeOf)
import Network.HTTP.Types       (Header, Status (..))
import Servant.API.ContentTypes (JSON, MimeRender (..), PlainText)

-- * Conversion type class

class (Typeable e, Show e) => ToServantErr e where
  status :: e -> Status

  message :: e -> Text
  message = fromString . show

  headers :: e -> [Header]
  headers _ = []

-- * Type level annotated exception handling

data Throws (e :: Type)

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
  mimeRender _ (ServantException e) =
    encode $
      object
        [ "type" .= errorType,
          "message" .= message e,
          "error" .= toJSON e
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
