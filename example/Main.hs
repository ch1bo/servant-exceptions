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
import Network.HTTP.Client       (defaultManagerSettings, newManager)
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Servant
import Servant.Client            (BaseUrl (..), Client, ClientM, Scheme (..), client, mkClientEnv,
                                  runClientM)
import Servant.Exception         (Exception (..), Throws, ToServantErr (..), mapException)
import Servant.Exception.Client  ()
import Servant.Exception.Server  ()

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text            as Text
import qualified Data.Text.Encoding   as Text

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

-- | Errors occurring at the 'UsersAPI'
data UsersError = UserNotFound
                | UserAlreadyExists
                | BadUser
                | InternalError
                deriving (Show)

-- | Required to be able to 'throwM' and 'catch' 'UsersError' errors.
instance Exception UsersError

-- | Provide means to convert 'UsersError' to servant's error types.
instance ToServantErr UsersError where
  status UserNotFound = status404
  status UserAlreadyExists = status409
  status BadUser = status400
  status InternalError = status500

  message InternalError = "Something bad happened internally"
  message e = Text.pack $ show e

  headers e = [("X-Reason", Text.encodeUtf8 $ message e)]

-- | There is a builtin 'MimeRender JSON' instance which uses 'ToJSON' to create
-- the actual error response payload. If we only use 'ToServantErr' functions,
-- we could re-use this implementation easily 'forall e. ToServantErr e'.
instance ToJSON UsersError where
  toJSON e = object [ "type" .= show (typeOf e)
                    , "message" .= message e
                    ]

-- | For 'PlainText' we
instance MimeRender PlainText UsersError where
  mimeRender ct = mimeRender ct . message

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

postUser :: MonadThrow m => User -> m ()
postUser (User n)
  | Text.length n < 3 = throwM BadUser
  | otherwise = throwM QueryError

-- | This natural transformation strips off 'ExceptT ServerError' from 'Handler'
-- and leaves us with plain 'IO' for our handlers.
--
-- The 'mapException databaseErrors' shows how we could generally catch and
-- rethrow "backend" errors into our api error type 'UsersError'.
nt :: IO a -> Handler a
nt = mapException databaseErrors . liftIO
 where
  databaseErrors :: DatabaseError -> UsersError
  databaseErrors _ = InternalError

-- | A natural transformation like this one, can be used to only "map"
-- exceptions in parts of the API using 'hoistServer pi ntMapDatabaseErors'.
ntMapDatabaseErrors :: MonadCatch m => m a -> m a
ntMapDatabaseErrors = mapException databaseErrors
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
