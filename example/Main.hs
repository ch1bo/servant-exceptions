{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Control.Monad.Catch       (MonadThrow (..), MonadCatch)
import Control.Monad.IO.Class    (liftIO)
import Data.Aeson
import Data.Text                 (Text)
import GHC.Generics
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Servant
import Servant.Exception         (Exception (..), Throws, ToServantErr (..), mapException)

import qualified Data.Text as Text

type API = "api" :> "users" :> UsersAPI

-- TODO(SN): relocatable Throws (:> api), (:<|>)
type UsersAPI = Get '[JSON] [User]
                :<|> Capture "name" Text :> Throws UsersError :> Get '[JSON] User
                :<|> ReqBody '[JSON] User :> Throws UsersError :> Post '[JSON] ()

newtype User = User Text
             deriving (Eq, Show, Generic)

instance FromJSON User
instance ToJSON User

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

instance ToJSON UsersError where
  toJSON = toJSON . show

-- | An example backend error type, which knows nothing about HTTP status codes
-- or content type encodings.
data DatabaseError = QueryError
                   | ConnectionFailure
                   deriving (Show)

instance Exception DatabaseError

server :: MonadCatch m => ServerT API m
server = getUsers
         :<|> getUser
         :<|> postUser

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

nt :: IO :~> Handler
nt = NT (mapException databaseErrors . liftIO)
 where
  databaseErrors :: DatabaseError -> UsersError
  databaseErrors _ = InternalError

main :: IO ()
main =
  run 8000
  . serve (Proxy :: Proxy API)
  $ enter nt server
