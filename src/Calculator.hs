{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}


module Calculator where

import Servant
import Data.Time
import Data.Text
import Data.Aeson
import GHC.Generics
import Network.Wai
import Data.Maybe (fromJust)
import Network.Wai.Handler.Warp
import Control.Monad.IO.Class

data Sub =
    Sub{
        x :: Integer,
        y :: Integer
    } deriving (Show,Eq,Generic,ToJSON,FromJSON)

type AddSubResponseHeader = Headers '[Header "add-result" Integer , Header "add-message" Text]
type CalculatorAPI =
         "cal" :> "add" :> QueryParam "x" Int :> QueryParam "y" Int :> Get '[JSON] Int --cal/add?x=12&&y=13
    :<|> "cal" :> "mul" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Int --cal/mul/12/13
    :<|> "cal" :> "sub" :> ReqBody '[JSON] Sub :> Post '[JSON] Sub --cal/sub
    :<|> "cal" :> "div" :> Header "value-x" Integer :> Header "value-y" Integer :> Get '[JSON] Integer
    :<|> "cal" :> "addsub" :> Capture "x" Integer :> QueryParam "y" Integer :> Post '[JSON] (AddSubResponseHeader Integer)

calculatorApp :: Server CalculatorAPI
calculatorApp =
         add
    :<|> mul
    :<|> sub
    :<|> divide
    :<|> addSub
    where
        add :: Maybe Int -> Maybe Int -> Handler Int
        add x y = do
            liftIO $ putStrLn ("API Add Invoked")
            return $ (fromJust x) + (fromJust y)
        mul :: Int -> Int -> Handler Int
        mul x y = do
            liftIO $ putStrLn ("API Mul Invoked")
            return $ x * y
        sub :: Sub -> Handler Sub
        sub (Sub x y) = do
            liftIO $ putStrLn ("API Sub Invoked")
            return $ Sub {x = x-y ,  y = y-x }
        divide :: Maybe Integer -> Maybe Integer -> Handler Integer
        divide x y = do
            liftIO $ putStrLn ("API Divide Invoked")
            return $ div (fromJust x) (fromJust y)
        addSub :: Integer -> Maybe Integer -> Handler (AddSubResponseHeader Integer)
        addSub x Nothing = do
            liftIO $ putStrLn ("AddSubCalled with invalid Arguments")
            throwError err400 { errBody = "Invalid divider"}
        addSub x (Just y) = do
            liftIO $ putStrLn ("API AddSubCalled")
            return $ addHeader (x + y) $ addHeader "Addition Result" (x-y)

calculatorAPIProxy :: Proxy CalculatorAPI
calculatorAPIProxy = Proxy

app2 :: Application
app2 = serve calculatorAPIProxy calculatorApp

runCalculator :: IO ()
runCalculator = putStrLn ("Starting the calculator Application on Port 8012") *> run 8012 app2