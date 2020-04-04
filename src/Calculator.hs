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

data Sub =
    Sub{
        x :: Integer,
        y :: Integer
    } deriving (Show,Eq,Generic,ToJSON,FromJSON)

type CalculatorAPI =
         "cal" :> "add" :> QueryParam "x" Int :> QueryParam "y" Int :> Get '[JSON] Int --cal/add?x=12&&y=13
    :<|> "cal" :> "mul" :> Capture "x" Int :> Capture "y" Int :> Get '[JSON] Int --cal/mul/12/13
    :<|> "cal" :> "sub" :> ReqBody '[JSON] Sub :> Post '[JSON] Sub --cal/sub
    :<|> "cal" :> "div" :> Header "value-x" Integer :> Header "value-y" Integer :> Get '[JSON] Integer


calculatorApp :: Server CalculatorAPI
calculatorApp =
         add
    :<|> mul
    :<|> sub
    :<|> divide
    where
        add :: Maybe Int -> Maybe Int -> Handler Int
        add x y = return $ (fromJust x) + (fromJust y)
        mul :: Int -> Int -> Handler Int
        mul x y = return $ x * y
        sub :: Sub -> Handler Sub
        sub (Sub x y) = return $ Sub {x = x-y ,  y = y-x }
        divide :: Maybe Integer -> Maybe Integer -> Handler Integer
        divide x y = return $ div (fromJust x) (fromJust y)  -- addHeader   pack "Division is sent in header"

calculatorAPIProxy :: Proxy CalculatorAPI
calculatorAPIProxy = Proxy


app2 :: Application
app2 = serve calculatorAPIProxy calculatorApp

runCalculator :: IO ()
runCalculator = putStrLn ("Starting the calculator Application on Port 8012") *> run 8012 app2