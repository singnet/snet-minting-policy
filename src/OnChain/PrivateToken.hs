{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module OnChain.PrivateToken
  ( 
    -- privateToken,
    -- mint,
    MintingSchema,
    MintParams (..),
    curSymbol,
    serialisedScript,
  )
where

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

policy :: PubKeyHash -> Scripts.MintingPolicy
policy owner =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` (PlutusTx.liftCode owner)

curSymbol :: PubKeyHash -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

-- START : only for .plutus compilation
policyValidator :: PubKeyHash -> Validator
policyValidator owner = Validator $ unMintingPolicyScript $ policy owner

policyScriptAddress :: PubKeyHash -> Address
policyScriptAddress = Ledger.scriptAddress . policyValidator

scriptAsCbor :: PubKeyHash -> LB.ByteString
scriptAsCbor = serialise . policyValidator

serialisedScript :: PubKeyHash -> PlutusScript PlutusScriptV1
serialisedScript owner = PlutusScriptSerialised . SBS.toShort $ LB.toStrict $ scriptAsCbor owner

-- END : only for .plutus compilation

data MintParams = MintParams
  { mpTokenName :: TokenName,
    mpAmount :: Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type MintingSchema = Endpoint "mint" MintParams

-- owner :: PubKeyHash
-- owner = "b4ddb46e5c9eff21ecfbe426feec1ae54712f16f31e993742a804030"

-- mint :: AsContractError e => Promise () MintingSchema e ()
-- mint = endpoint @"mint" @MintParams $ \(MintParams tknName amount) -> do
--   let val = Value.singleton curSymbol tknName amount
--       lookups = Constraints.mintingPolicy policy
--       tx = Constraints.mustMintValue val
--   --   The Below line will automatically find an input in the wallet
--   --   to cover the fees.
--   --   It will also, transfer the minted amount to the wallet if the
--   --   value is positive
--   ledgerTx <- submitTxConstraintsWith @Void lookups tx
--   void $ awaitTxConfirmed $ txId ledgerTx
--   logInfo @String $ "forged => " <> show val <> " " <> show tknName <> "."

-- privateToken :: AsContractError e => Contract () MintingSchema e ()
-- privateToken = do
--   logInfo @String $ "Owner Pub Key Hash=> " <> show owner <> "."
--   logInfo @String $ "Currency Symbol => " <> show curSymbol <> "."
--   logInfo @String "Initialized Private Script"
--   selectList [mint] >> privateToken