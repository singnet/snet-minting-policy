{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module OnChain.PrivateToken (privateToken, mint, MintingSchema, MintParams (..), curSymbol, serialisedScript) where

import Cardano.Api hiding (Address)
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise
import Control.Monad
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Data.Void
import Ledger
  ( Address,
    PubKeyHash (..),
    ScriptContext (scriptContextTxInfo),
    Validator (Validator),
    mkMintingPolicyScript,
    scriptAddress,
    scriptCurrencySymbol,
    txId,
    txSignedBy,
    unMintingPolicyScript,
  )
import qualified Ledger.Constraints as Constraints
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Playground.Contract
import Plutus.Contract as Contract
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import PlutusTx.Prelude as PlutusTx
import Prelude hiding (($), (<>))

-- Minting Policy:
-- The currency symbol is the hash of this script.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy pkh () ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: Scripts.MintingPolicy
policy =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` (PlutusTx.liftCode owner)

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

-- START : only for .plutus compilation
policyValidator :: Validator
policyValidator = Validator $ unMintingPolicyScript policy

policyScriptAddress :: Address
policyScriptAddress = Ledger.scriptAddress policyValidator

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise policyValidator

serialisedScript :: PlutusScript PlutusScriptV1
serialisedScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

-- END : only for .plutus compilation

data MintParams = MintParams
  { mpTokenName :: TokenName,
    mpAmount :: Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type MintingSchema = Endpoint "mint" MintParams

owner :: PubKeyHash
owner = "88d3ff2343305de1177081ce211fa7610c617f767b1f05f440dde1d2" :: PubKeyHash

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
  logInfo @String $ "forged => " <> show val <> " " <> show tknName <> "."

privateToken :: AsContractError e => Contract () MintingSchema e ()
privateToken = do
  logInfo @String $ "Owner Pub Key Hash=> " <> show owner <> "."
  logInfo @String $ "Currency Symbol => " <> show curSymbol <> "."
  logInfo @String "Initialized Private Script"
  selectList [mint] >> privateToken