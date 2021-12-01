{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeApplications  #-}

module OnChain.AGIXMultiOwnerMintingPolicy
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
import           Prelude               hiding (($), (&&), (<), (<>), (==), (||), (.))

-- Minting Policy:
-- The currency symbol is the hash of this script.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> PubKeyHash -> () -> ScriptContext -> Bool
mkPolicy owner1 owner2 () ctx = do isBurning || (isSignedByOwner1 && isSignedByOwner2)
    where
        isSignedByOwner1 :: Bool
        isSignedByOwner1 = txSignedBy (scriptContextTxInfo ctx) owner1

        isSignedByOwner2 :: Bool
        isSignedByOwner2 = txSignedBy (scriptContextTxInfo ctx) owner2

        isBurning :: Bool
        isBurning = case flattenValue (txInfoMint (scriptContextTxInfo ctx)) of
            [(curSym', _, amt')] -> curSym' == ownCurrencySymbol ctx && amt' < 0
            _                    -> False

policy :: PubKeyHash -> PubKeyHash -> Scripts.MintingPolicy
policy owner1 owner2 =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \owner1' owner2' -> Scripts.wrapMintingPolicy $ mkPolicy owner1' owner2'||])
      `PlutusTx.applyCode` PlutusTx.liftCode owner1
      `PlutusTx.applyCode` PlutusTx.liftCode owner2

curSymbol :: PubKeyHash -> PubKeyHash -> CurrencySymbol
curSymbol owner1 owner2 = scriptCurrencySymbol $ policy owner1 owner2

-- START : only for .plutus compilation
policyValidator :: PubKeyHash -> PubKeyHash -> Validator
policyValidator owner1 owner2 = Validator $ unMintingPolicyScript $ policy owner1 owner2

policyScriptAddress :: PubKeyHash -> PubKeyHash -> Address
policyScriptAddress owner1 owner2 = Ledger.scriptAddress $ policyValidator owner1 owner2

scriptAsCbor :: PubKeyHash -> PubKeyHash -> LB.ByteString
scriptAsCbor owner1 owner2= serialise $ policyValidator owner1 owner2

serialisedScript :: PubKeyHash -> PubKeyHash -> PlutusScript PlutusScriptV1
serialisedScript owner1 owner2 = PlutusScriptSerialised . SBS.toShort $ LB.toStrict $ scriptAsCbor owner1 owner2
