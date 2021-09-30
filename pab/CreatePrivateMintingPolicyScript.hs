{-# LANGUAGE OverloadedStrings #-}

import Cardano.Api
import Data.String
import Ledger
-- import Ledger.Crypto
import OnChain.PrivateToken (serialisedScript)
import System.Environment (getArgs)
import Prelude

createPlutusScript :: PubKeyHash -> String -> IO ()
createPlutusScript owner filename = do
  putStrLn $ "owner => " ++ (show owner)
  result <- writeFileTextEnvelope filename Nothing (serialisedScript owner)
  case result of
    Left err -> putStrLn $ displayError err
    Right () -> do
      putStrLn $ "created the plutus script => " ++ filename
      return ()

main :: IO ()
main = do
  args <- getArgs
  let argsLen = length args
  let owner = if argsLen > 0 then head args else error "Please provide Public Key Hash of the Owner"
  let filename = if argsLen > 1 then args !! 1 else "minting-policy-" ++ owner ++ ".plutus"
  putStrLn "Arguments Check: Success"
  putStrLn "creating plutus script"
  createPlutusScript (fromString owner) filename