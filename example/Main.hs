{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeOperators         #-}

module Main where

import Control.Concurrent
import Control.Monad             ((<=<))
import Control.Monad.Catch       (MonadCatch, MonadThrow (..))
import Control.Monad.IO.Class    (liftIO)
import Data.Aeson
import Data.Bifunctor            (bimap)
import Data.Text                 (Text)
import Data.Text.Encoding
import Data.Typeable             (typeOf)
import GHC.Generics
import Network.HTTP.Client       (newManager, defaultManagerSettings)
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Servant
import Servant.Client            (Client, ClientM, client, runClientM, mkClientEnv, BaseUrl(..), Scheme(..))
import Servant.Exception         (Exception (..), Throws, ToServantErr (..), mapException)
import Servant.Exception.Client  ()
import Servant.Exception.Server  (mapException)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as Text

-- * Example types

type API = "api" :> "users" :> UsersAPI

type UsersAPI = Throws UsersError :> (
  Get '[JSON] [User]
  :<|> Capture "name" Text :> Get '[PlainText, JSON] User
  :<|> ReqBody '[JSON] User :> Post '[JSON] ()
  )

newtype User = User Text
             deriving (Eq, Show, Generic)

instance FromJSON User

instance ToJSON User

instance MimeRender PlainText User where
  mimeRender ct = mimeRender ct . show

instance MimeUnrender PlainText User where
  mimeUnrender ct = bimap show User . decodeUtf8' . BSL.toStrict

-- | Erros occurring at the @UsersAPI@, which can be converted to @ServantErr@
-- via @ToServantErr@.
data UsersError = UserNotFound
                | UserAlreadyExists
                | BadUser
                | InternalError
                deriving (Show)

instance Exception UsersError

-- | Provide a conversion to servant's error type @ServantErr@.
instance ToServantErr UsersError where
  status UserNotFound = status404
  status UserAlreadyExists = status409
  status BadUser = status400
  status InternalError = status500

  -- TODO(SN): not used right now, default MimeRender?
  message InternalError = "Something bad happened internally"
  message e = Text.pack $ show e

instance ToJSON UsersError where
  toJSON e = object [ "type" .= show (typeOf e)
                    , "message" .= message e
                    ]

instance MimeRender PlainText UsersError where
  mimeRender ct = mimeRender ct . show

instance MimeUnrender PlainText UsersError where
  mimeUnrender ct "UserNotFound"      = Right UserNotFound
  mimeUnrender ct "UserAlreadyExists" = Right UserAlreadyExists
  mimeUnrender ct "BadUser"           = Right BadUser
  mimeUnrender ct "InternalError"     = Right InternalError
  mimeUnrender ct bs                  = Left . show $ bs

-- | An example backend error type, which knows nothing about HTTP status codes
-- or content type encodings.
data DatabaseError = QueryError
                   | ConnectionFailure
                   deriving (Show)

instance Exception DatabaseError

-- * Example server

server :: MonadCatch m => ServerT API m
server = getUsers
         :<|> getUser
         :<|> postUser

clientGetUsers :: ClientM [User]
clientGetUser :: Text -> ClientM User
clientPostUser :: User -> ClientM ()
clientGetUsers :<|> clientGetUser :<|> clientPostUser = client (Proxy :: Proxy API)

getUsers :: Monad m => m [User]
getUsers = return [User "foo"]

getUser :: MonadThrow m => Text -> m User
getUser n
  | n == "foo" = return $ User "foo"
  | n == "bar" = throwM ConnectionFailure
  | otherwise = throwM UserNotFound

postUser :: MonadCatch m => User -> m ()
postUser (User n)
  | Text.length n < 3 = throwM BadUser
  | otherwise = throwM QueryError

nt :: IO a -> Handler a
nt = mapException databaseErrors . liftIO
 where
  databaseErrors :: DatabaseError -> UsersError
  databaseErrors _ = InternalError

main :: IO ()
main = do
  forkIO $ do
    run 8000 . serve (Proxy :: Proxy API)
#if MIN_VERSION_servant_server(0,12,0)
      $ hoistServer (Proxy :: Proxy API) nt server
#else
      $ enter (NT nt) server
#endif

  manager' <- newManager defaultManagerSettings

  res <- runClientM clientGetUsers (mkClientEnv manager' (BaseUrl Http "localhost" 8000 ""))

  case res of
    Left err -> putStrLn $ "Error: " ++ show err
    Right users -> print users
