{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import UserAPI
import Calculator
import Network.Wai.Handler.Warp
import Database.PostgreSQL.Simple

main :: IO ()
main = putStrLn ("This is where the execution starts") *> setupApp *> runCalculator

setupApp :: IO ()
setupApp = do
    _ <- makeConnectionToRanaPaymentDb
    return ()

ranaPaymentDb :: ConnectInfo
ranaPaymentDb =
    ConnectInfo
    { connectHost = "localhost"
    , connectDatabase = "rana_payment"
    , connectUser     = "user_rana_payemnt"
    , connectPassword = "user_rana_payment"
    , connectPort     = 5432
    }

makeConnectionToRanaPaymentDb :: IO ()
makeConnectionToRanaPaymentDb = do
    putStrLn ("Connecting to Rana Payment db... ")
    conn <- connect ranaPaymentDb
    mapM_ print =<< (query_ conn "SELECT 1+1" :: IO [Only Int])

makeDBConnection :: IO()
makeDBConnection = do
    putStrLn ("Connecting the PostgreSQL... ")
    conn <-
        connect
            defaultConnectInfo
            { connectHost = "localhost"
            , connectDatabase = "rana_payment"
            , connectUser     = "user_rana_payemnt"
            , connectPassword = "user_rana_payment"
            }
    mapM_ print =<< (query_ conn "SELECT 1+1" :: IO [Only Int])
