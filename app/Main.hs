{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Main where

import UserAPI
import Calculator
import Network.Wai.Handler.Warp

main :: IO ()
main = putStrLn ("This is where the execution starts") *> runCalculator

