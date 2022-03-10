{-# LANGUAGE OverloadedStrings #-}

import Cardano.Api
import Data.String
import Ledger
-- import Ledger.Crypto
import OnChain.AGIXMultiOwnerMintingPolicy (serialisedScript)
import System.Environment (getArgs)
import Prelude

createPlutusScript :: PubKeyHash -> PubKeyHash -> TokenName -> String -> IO ()
createPlutusScript owner1 owner2 tName filename = do
  putStrLn $ "Creating script for " <> show tName <> "..."
  result <- writeFileTextEnvelope filename Nothing $ serialisedScript owner1 owner2 tName
  case result of
    Left err -> putStrLn $ displayError err
    Right () -> do
      putStrLn $ "created the plutus script => " ++ filename
      return ()

main :: IO ()
main = do
  args <- getArgs
  let argsLen = length args
  let owner1 = if argsLen > 0 then head args else error "Please provide Public Key Hash of the Owner1"
  let owner2 = if argsLen > 1 then args !! 1  else error "Please provide Public Key Hash of the Owner2"
  let tName = if argsLen > 2 then args !! 2  else error "Please provide Token Name"
  let filename = if argsLen > 3 then args !! 3 else "minting-policy-" ++ owner1 ++ "-" ++ owner2 ++ ".plutus"
  putStrLn "Arguments Check: Success"
  putStrLn "creating plutus script"
  putStrLn $ "owner1 => " ++ show owner1
  putStrLn $ "owner2 => " ++ show owner2
  putStrLn $ "Token Name => " ++ show tName
  putStrLn $ "filename => " ++ show filename
  if owner1 /= owner2 then createPlutusScript (fromString owner1) (fromString owner2) (fromString tName) filename else error "Owner1 public hash can't be same as Owner2 public hash" 