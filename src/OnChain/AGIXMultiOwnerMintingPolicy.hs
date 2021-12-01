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
import           Prelude               hiding (fst, snd, ($), (&&), (.), (<),
                                        (<>), (==), (||))

-- Cardano Team's Review
-- "As long as we sign, we can mint as many tokens as we want, and those tokens can have arbitrary token names.
-- Anybody else can burn our tokens, but only one token name per transaction."

-- Also, there is a tiny stylistic issue: the do keyword is strange and unusual - we advise you to skip it.


-- Takeaway
-- Set token name configurable in the Minting Policy
-- test burning tokens that are not owned by the user

-- Policy Characteristics
-- Signatories and Token Name will be configurable
-- We will use different signatories for different tokens
-- Only signatories should mint
-- Only token holders can burn their tokens


-- Minting Policy:
-- The currency symbol is the hash of this script.
{-# INLINEABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> PubKeyHash -> TokenName -> () -> ScriptContext -> Bool
mkPolicy owner1 owner2 name () ctx = isCorrectToken && (isBurning || (isSignedByOwner1 && isSignedByOwner2))
  where
    info :: TxInfo
    info = scriptContextTxInfo ctx

    nameAmount :: (TokenName, Integer)
    nameAmount = case flattenValue $ txInfoMint info of
        [(_, name', amt)] -> (name', amt)
        _                 -> traceError "expected only one minting policy"

    isCorrectToken :: Bool
    isCorrectToken = fst nameAmount == name

    isSignedByOwner1 :: Bool
    isSignedByOwner1 = txSignedBy info owner1

    isSignedByOwner2 :: Bool
    isSignedByOwner2 = txSignedBy info owner2

    isBurning :: Bool
    isBurning = snd nameAmount < 0

policy :: PubKeyHash -> PubKeyHash -> TokenName -> Scripts.MintingPolicy
policy owner1 owner2 name =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [|| \owner1' owner2' name' -> Scripts.wrapMintingPolicy $ mkPolicy owner1' owner2' name'||])
      `PlutusTx.applyCode` PlutusTx.liftCode owner1
      `PlutusTx.applyCode` PlutusTx.liftCode owner2
      `PlutusTx.applyCode` PlutusTx.liftCode name

curSymbol :: PubKeyHash -> PubKeyHash -> TokenName -> CurrencySymbol
curSymbol owner1 owner2 name = scriptCurrencySymbol $ policy owner1 owner2 name

-- START : only for .plutus compilation
policyValidator :: PubKeyHash -> PubKeyHash -> TokenName -> Validator
policyValidator owner1 owner2 name = Validator $ unMintingPolicyScript $ policy owner1 owner2 name

policyScriptAddress :: PubKeyHash -> PubKeyHash -> TokenName -> Address
policyScriptAddress owner1 owner2 name = Ledger.scriptAddress $ policyValidator owner1 owner2 name

scriptAsCbor :: PubKeyHash -> PubKeyHash -> TokenName -> LB.ByteString
scriptAsCbor owner1 owner2 name = serialise $ policyValidator owner1 owner2 name

serialisedScript :: PubKeyHash -> PubKeyHash -> TokenName -> PlutusScript PlutusScriptV1
serialisedScript owner1 owner2 name = PlutusScriptSerialised . SBS.toShort $ LB.toStrict $ scriptAsCbor owner1 owner2 name
