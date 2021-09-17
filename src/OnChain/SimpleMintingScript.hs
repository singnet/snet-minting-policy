{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module OnChain.SimpleMintingScript (curSymbol, simpleMintingScript, mint, MintingSchema, MintParams (..), serialisedScript) where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
-- import Data.Text (Text)

-- import Plutus.Trace.Emulator as Emulator

-- import Text.Printf (printf)

import Codec.Serialise
import Control.Monad
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Data.Void
import Ledger hiding (mint, singleton)
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Playground.Contract
import Plutus.Contract as Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude hiding (($))

-- Minting Policy:
-- The currency symbol is the hash of this script.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: () -> ScriptContext -> Bool
mkPolicy () _ = True

-- TODO:
-- Pausable
-- Owner Contract - done

-- Compile the policy to plutus script
policy :: Scripts.MintingPolicy
policy =
  mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkPolicy||])

-- START : only for cli
policyValidator :: Validator
policyValidator = Validator $ unMintingPolicyScript policy

policyScriptAddress :: Address
policyScriptAddress = Ledger.scriptAddress policyValidator


scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise policyValidator

serialisedScript :: PlutusScript PlutusScriptV1
serialisedScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

-- END : only for cli

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- OFFCHAIN CODE

data MintParams = MintParams
  { mpTokenName :: TokenName,
    mpAmount :: Integer
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema)

type MintingSchema = Endpoint "mint" MintParams

-- mint :: MintParams -> Contract w MintingSchema Text ()
mint :: AsContractError e => Promise () MintingSchema e ()
mint = endpoint @"mint" @MintParams $ \(MintParams tknName amount) -> do
  let val = Value.singleton curSymbol tknName amount
      lookups = Constraints.mintingPolicy policy
      tx = Constraints.mustMintValue val
  --   The Below line will automatically find an input in the wallet
  --   to cover the fees.
  --   It will also, transfer the minted amount to the wallet if the
  --   value is positive
  ledgerTx <- submitTxConstraintsWith @Void lookups tx
  void $ awaitTxConfirmed $ txId ledgerTx
  logInfo @String $ "Script Address => " <> show policyScriptAddress <> "."
  logInfo @String $ "Currency Symbol => " <> show curSymbol <> "."
  logInfo @String $ "forged => " <> show val <> " " <> show tknName <> "."

-- mint2 :: MintParams -> Contract w MintingSchema Text ()
-- mint2 mp = do
--   let val = Value.singleton curSymbol (mpTokenName mp) (mpAmount mp)
--       lookups = Constraints.mintingPolicy policy
--       tx = Constraints.mustMintValue val
--   ledgerTx <- submitTxConstraintsWith @Void lookups tx
--   void $ awaitTxConfirmed $ txId ledgerTx
--   Contract.logInfo @String $ printf "forged %s" (show val)

-- endpoints :: Contract () MintingSchema Text ()
-- endpoints = mint' >> endpoints
--     where
--         mint' = endpoint @"mint" >>= mint

simpleMintingScript :: AsContractError e => Contract () MintingSchema e ()
simpleMintingScript = do
  logInfo @String $ "Currency Symbol => " <> show curSymbol <> "."
  logInfo @String $ "Script Address => " <> show policyScriptAddress <> "."
  logInfo @String "Initialized Simple Minting Script"
  selectList [mint] >> simpleMintingScript

toByteStr :: String -> TokenName
toByteStr = TokenName . toBuiltin . C.pack

-- endpoints :: Contract () MintingSchema Text ()
-- endpoints = mint' >> endpoints
--   where
--     mint' = endpoint @"mint" >>= mint2

-- test :: IO ()
-- test = runEmulatorTraceIO $ do
--   let tokenName = "AGIX"
--   w1 <- activateContractWallet (Wallet 1) simpleMintingScript
--   w2 <- activateContractWallet (Wallet 2) simpleMintingScript
--   callEndpoint @"mint" w1 $
--     MintParams
--       { mpTokenName = tokenName,
--         mpAmount = 100
--       }
--   callEndpoint @"mint" w2 $
--     MintParams
--       { mpTokenName = tokenName,
--         mpAmount = 200
--       }
--   void $ Emulator.waitNSlots 1
--   callEndpoint @"mint" w1 $
--     MintParams
--       { mpTokenName = tokenName,
--         mpAmount = -50
--       }