{-# LANGUAGE OverloadedStrings #-}

import Cardano.Api
import Data.String
import Ledger
-- import Ledger.Crypto
import OnChain.AGIXMultiOwnerMintingPolicy (serialisedScript)
import System.Environment (getArgs)
import Prelude

createPlutusScript :: PubKeyHash -> PubKeyHash -> String -> IO ()
createPlutusScript owner1 owner2 filename = do
  putStrLn $ "owner1 => " ++ show owner1
  putStrLn $ "owner2 => " ++ show owner2
  result <- writeFileTextEnvelope filename Nothing (serialisedScript $ owner1 owner2)
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
  let owner2 = if argsLen > 1 then head args else error "Please provide Public Key Hash of the Owner2"
  let filename = if argsLen > 2 then args !! 1 else "minting-policy-" ++ owner ++ ".plutus"
  putStrLn "Arguments Check: Success"
  putStrLn "creating plutus script"
  createPlutusScript (fromString owner1) (fromString owner2) filename