{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Servant.Exception
  ( ToServantErr(..)
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
import Data.String               (fromString)
import Data.Text                 (Text)
import Data.Typeable             (Typeable, cast, typeOf)
import Network.HTTP.Types.Status (Status (..))
import Servant                   (ServantErr (..))

import qualified Data.Text          as Text
import qualified Data.Text.Encoding as Text

-- * A generic servant error encoding

-- | Class for easy exception handling in route handlers.
class (Typeable e, Show e, ToJSON e) => ToServantErr e where
  status :: e -> Status

  message :: e -> Text
  message = fromString . show

  -- TODO(SN): how to incorporate content-type?
  -- mimeRender (Proxy :: Proxy JSON) e
  toServantErr :: e -> ServantErr
  toServantErr e = ServantErr { errHTTPCode = statusCode $ status e
                              , errReasonPhrase = reasonPhrase $ status e
                              , errBody = encode $ object [ "type" .= errorType
                                                          , "error" .= toJSON e
                                                          , "message" .= message e
                                                          ]
                              , errHeaders = []
                              }
   where
    reasonPhrase = Text.unpack . Text.decodeUtf8 . statusMessage
    errorType = show $ typeOf e

-- * Servant exception handling

data ServantException = forall e. (Exception e, ToServantErr e) => ServantException e
                      deriving (Typeable)

instance Show ServantException where
  show (ServantException e) = show e

instance Exception ServantException

instance ToJSON ServantException where
  toJSON (ServantException e) = toJSON e

instance ToServantErr ServantException where
  status (ServantException e) = status e
  toServantErr (ServantException e) = toServantErr e

toServantException :: (Exception e, ToServantErr e) => e -> SomeException
toServantException = toException . ServantException

fromServantException :: Exception e => SomeException -> Maybe e
fromServantException x = fromException x >>= \(ServantException e) -> cast e

-- * Catch all @ServantException@ and convert them to @ServantErr@ using @ToServantErr@.
handleServantExceptions :: (MonadCatch m, MonadError ServantErr m) => m a -> m a
handleServantExceptions = handleServantExceptionsWith toServantErr

-- * Catch all @ServantException@ and convert them to @ServantErr@ using given function.
handleServantExceptionsWith :: (MonadCatch m, MonadError ServantErr m)
                            => (ServantException -> ServantErr) -> m a -> m a
handleServantExceptionsWith f a = a `catch` \(e :: ServantException) -> throwError $ f e

-- * Exception utilities

-- | Catch and rethrow using mapping function @f@.
mapException :: (Exception e1, Exception e2, MonadCatch m) => (e1 -> e2) -> m a -> m a
mapException f a = a `catch` (throwM . f)
