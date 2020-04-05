{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module UserAPI where

import Servant
import Data.Time
import Data.Text
import Data.Aeson
import GHC.Generics
import Network.Wai
import Network.Wai.Handler.Warp

data Address =
    Address{
        city :: Text,
        state :: Text,
        country :: Text,
        pin :: Text
    } deriving (Show, Eq, Generic, ToJSON , FromJSON)

data AddUserRequest =
    AddUserRequest{
        name :: Text,
        mobile :: Text,
        address :: Address
    } deriving (Show, Eq , Generic , FromJSON , ToJSON)

data AddUserResponse =
    AddUserResponse{
        userName :: Text,
        userId   :: Text,
        description :: Text
    } deriving (Show, Eq, Generic, ToJSON , FromJSON)

type UserDetailResponse = AddUserResponse


type UserAPI2 =
    "users" :> "add" :> ReqBody '[JSON] AddUserRequest :> Post '[JSON] AddUserResponse
    :<|> "users" :> Capture "id" Text :> Get '[JSON] UserDetailResponse

type UserAPI1 = "users" :> Get '[JSON] UserDetailResponse

type UserAPI3 =
    "users" :> "user" :> Get '[JSON] UserDetailResponse
    :<|> "users" :> "albert" :> Get '[JSON] UserDetailResponse
    :<|> "users" :> "einstine" :> Get '[JSON] UserDetailResponse

--Let's return this user
user1 :: UserDetailResponse
user1 = AddUserResponse{
    userName = pack $ "Shashi",
    userId  =  pack $ "userId1",
    description = pack $ "This is a good user"
}

albert :: UserDetailResponse
albert = AddUserResponse{
    userName = pack $ "Albert",
    userId  =  pack $ "userId1",
    description = pack $ "This is a good user"
}

einstein :: UserDetailResponse
einstein = AddUserResponse{
    userName = pack $ "Einstein",
    userId  =  pack $ "userId1",
    description = pack $ "This is a good user"
}

server1 :: Server UserAPI3
server1 = return user1
    :<|> return albert
    :<|> return einstein

userAPI :: Proxy UserAPI3
userAPI = Proxy

app1 :: Application
app1 = serve userAPI server1

runUserAPI :: IO ()
runUserAPI = putStrLn ("Starting userAPI server at port 8011 ") *> (run 8011 app1)
