{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where

import Servant.Exception (Exception (..), ToServantErr (..), fromServantException,
                          handleServantExceptions, toServantException)

import Control.Monad.Catch       (MonadThrow (..))
import Control.Monad.IO.Class    (liftIO)
import Data.Aeson
import Data.Text
import GHC.Generics
import Network.HTTP.Types.Status
import Network.Wai.Handler.Warp
import Servant

data User = User String
          deriving (Eq, Show, Generic)

instance ToJSON User

data UsersError = UserNotFound
                | DatabaseError
                deriving (Show)

instance Exception UsersError where
  toException = toServantException
  fromException = fromServantException

instance ToJSON UsersError where
  toJSON = toJSON . show

instance ToServantErr UsersError where
  status UserNotFound = status404
  status DatabaseError = status500

type API = "api" :> "users" :> UsersAPI

type UsersAPI = Get '[JSON] [User]
                :<|> Capture "name" Text :> Get '[JSON] User

server :: MonadThrow m => ServerT API m
server = getUsers
         :<|> getUser

getUsers :: Monad m => m [User]
getUsers = return [User "foo"]

getUser :: MonadThrow m => Text -> m User
getUser n
  | n == "foo" = return $ User "foo"
  | n == "bar" = throwM DatabaseError
  | otherwise = throwM UserNotFound

nt :: IO :~> Handler
nt = NT (handleServantExceptions . liftIO)

main :: IO ()
main =
  run 8000
  . serve (Proxy :: Proxy API)
  $ enter nt server
