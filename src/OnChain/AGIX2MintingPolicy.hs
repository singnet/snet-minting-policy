{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module OnChain.AGIX2MintingPolicy
  ( curSymbol,
    serialisedScript,
  )
where

import           Cardano.Api           hiding (Address)
import           Cardano.Api.Shelley   (PlutusScript (..))
import           Codec.Serialise
import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Ledger
import qualified Ledger.Typed.Scripts  as Scripts
import           Ledger.Value          as Value
import qualified PlutusTx
import           PlutusTx.Prelude      hiding (Semigroup (..), unless)
import           Prelude               hiding (($), (&&), (<), (<>), (==), (||))

-- Minting Policy:
-- The currency symbol is the hash of this script.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy owner () ctx = do isBurning || isSignedByOwner
    where
        isSignedByOwner :: Bool
        isSignedByOwner = txSignedBy (scriptContextTxInfo ctx) owner

        isBurning :: Bool
        isBurning = case flattenValue (txInfoMint (scriptContextTxInfo ctx)) of
            [(curSym', _, amt')] -> curSym' == ownCurrencySymbol ctx && amt' < 0
            _                    -> False

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
