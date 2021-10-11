{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module OnChain.PrivateTokenWithCap
  ( -- privateToken,
    -- mint,
    -- MintingSchema,
    MintParams (..),
    curSymbol,
    -- serialisedScript,
  )
where

import Cardano.Api hiding (Address, Value)
import Cardano.Api.Shelley (PlutusScript (..))
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Data.Map (Map)
import Data.Map as Map hiding (foldr)
import Data.Maybe
import Data.Monoid
import Ledger
  ( Address,
    PubKeyHash (..),
    ScriptContext (scriptContextTxInfo),
    TxInInfo (TxInInfo),
    Validator (Validator),
    mkMintingPolicyScript,
    scriptAddress,
    scriptCurrencySymbol,
    txSignedBy,
    unMintingPolicyScript,
  )
import Ledger.Ada as Ada
import Ledger.Address
import Ledger.AddressMap
import Ledger.Contexts
import Ledger.Tx
  ( ChainIndexTxOut (..),
    Tx (txMint),
    TxOutTx (TxOutTx, txOutTxTx),
  )
import qualified Ledger.Typed.Scripts as Scripts
import Ledger.Value as Value
import Playground.Contract hiding (utxosAt)
import Plutus.Contract
import Plutus.Contract.Effects
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), mapMaybe, unless)
import Prelude hiding (foldr, map, ($), (&&), (<=), (<>), (==))

-- Minting Policy:
-- The currency symbol is the hash of this script.
-- {-# INLINEABLE mkPolicy #-}
-- mkPolicy :: MintParams -> BuiltinData -> ScriptContext -> Bool
-- mkPolicy pkh _redeemer ctx = txSignedBy (scriptContextTxInfo ctx) pkh

{-# INLINEABLE mkPolicy #-}
mkPolicy :: MintParams -> BuiltinData -> ScriptContext -> Bool
mkPolicy MintParams {owner, totalLimit, tknName} _redeemer ctx = do 
  signedByOwner && lessThanTheTotalLimit
  
  where
    -- findTotalMintedSoFar :: Value
    -- findTotalMintedSoFar = Ada.toValue 1

    -- inputTxns :: [TxInInfo]
    -- inputTxns = txInfoInputs $ scriptContextTxInfo ctx

    -- inputUtxoAccounts :: [PubKeyHash]
    -- inputUtxoAccounts = mapMaybe (txOutPubKey . txInInfoResolved) inputTxns

    ownerAddress :: Address
    ownerAddress = pubKeyHashAddress owner

    pastUtxos :: AsContractError e => Contract w s e (Map TxOutRef ChainIndexTxOut)
    pastUtxos = utxosAt ownerAddress

    mintedValues :: Map TxOutRef ChainIndexTxOut -> [Value]
    mintedValues = Map.elems . Map.map findMintVal

    findMintVal :: ChainIndexTxOut -> Value
    findMintVal co =  _ciTxOutValue co

    totalMintedVal :: [Value] -> Value
    totalMintedVal = foldr (\cur acc -> acc <> cur) (Ada.toValue 1)

    signedByOwner :: Bool
    signedByOwner = txSignedBy (scriptContextTxInfo ctx) owner

    lessThanTheTotalLimit :: Bool
    lessThanTheTotalLimit = do
      utxos <- pastUtxos
      case flattenValue (totalMintedVal $ mintedValues utxos) of
        [(curSym, tknName', amt)] -> curSym == ownCurrencySymbol ctx && tknName' == tknName && amt <= totalLimit
        _ -> False

policy :: MintParams -> Scripts.MintingPolicy
policy mp =
  mkMintingPolicyScript $
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy . mkPolicy||])
      `PlutusTx.applyCode` (PlutusTx.liftCode mp)

curSymbol :: MintParams -> CurrencySymbol
curSymbol = scriptCurrencySymbol . policy

data MintParams = MintParams
  { owner :: !PubKeyHash,
    totalLimit :: !Integer,
    tknName :: TokenName
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

PlutusTx.makeLift ''MintParams

-- -- START : only for .plutus compilation
-- -- UNCOMMENT when the policy is ready
-- policyValidator :: PubKeyHash -> Validator
-- policyValidator owner = Validator $ unMintingPolicyScript $ policy owner

-- policyScriptAddress :: PubKeyHash -> Address
-- policyScriptAddress = Ledger.scriptAddress . policyValidator

-- scriptAsCbor :: PubKeyHash -> LB.ByteString
-- scriptAsCbor = serialise . policyValidator

-- serialisedScript :: PubKeyHash -> PlutusScript PlutusScriptV1
-- serialisedScript owner = PlutusScriptSerialised . SBS.toShort $ LB.toStrict $ scriptAsCbor owner

-- -- END : only for .plutus compilation

-- data MintParams = MintParams
--   { mpTokenName :: TokenName,
--     mpAmount :: Integer
--   }
--   deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

-- type MintingSchema = Endpoint "mint" MintParams

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