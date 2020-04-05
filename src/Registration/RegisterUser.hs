{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}


module Registration.RegisterUser where

import Data.Text
import Network.Wai
import Network.Wai.Handler.Warp
import Control.Lens
import Database.Beam
import Database.Beam.Postgres
import Database.PostgreSQL.Simple
import Servant
import qualified Registration.RegisterUserType as RU
import Crypto.Hash (hash , SHA256 (..) , Digest , hashWith)
import qualified Data.ByteString.UTF8 as Utf8
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Base64 as Base64

type RegisterUserHeaders = Headers '[Header "sessiod-id" Text, Header "checksum" Text , Header "authentication-code" Text]

type RegisterUserAPI =
    "user" :> "register" :> ReqBody '[JSON] RU.RegisterUserRequest :> Post '[JSON] (RegisterUserHeaders RU.RegisterUserResponse)

registerUserAPIProxy :: Proxy RegisterUserAPI
registerUserAPIProxy = Proxy

registerUserServer :: Connection -> Server RegisterUserAPI
registerUserServer conn = registerUser conn

registerUser :: Connection -> RU.RegisterUserRequest -> Handler (RegisterUserHeaders  RU.RegisterUserResponse)
registerUser conn user = do
    liftIO $ putStrLn ("RegisterUserAPI Invoked")
    return
        $ addHeader "sessionId"
        $ addHeader "checksum"
        $ addHeader "authentication code"
            RU.RegisterUserResponse
            { RU._userRegistrationToken = getRegistrationTokenFromUserRequest user
            , RU._userId = user ^. RU.userName
            }

getRegistrationTokenFromUserRequest :: RU.RegisterUserRequest -> Text
getRegistrationTokenFromUserRequest user = do
    let userName = user ^. RU.userName
        userMobile = user ^. RU.userMobile
        userEmail = user ^. RU.userEmail
        userString = userName <> userMobile <> userEmail
        registrationToken = hashWith SHA256 (Utf8.fromString $ unpack userString)
    pack $ show  registrationToken

startRegisterUserApp :: Connection -> IO ()
startRegisterUserApp conn = do
    putStrLn ("Starting App on Port 8013")
    run 8013 (runRegisterUserApp conn)

runRegisterUserApp :: Connection -> Application
runRegisterUserApp conn = serve registerUserAPIProxy (registerUserServer conn)