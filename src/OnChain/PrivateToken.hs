{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module OnChain.PrivateToken (privateToken, mint, MintingSchema, MintParams (..), curSymbol, serialisedScript) where

-- import Wallet.Emulator.Wallet (walletAddress, walletPrivKey, walletPubKey)

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import Control.Monad
import Data.ByteString (ByteString)
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
import Plutus.V1.Ledger.Bytes (bytes, fromBytes)
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Wallet.Emulator.Wallet (walletPubKey)
import Prelude hiding (($))

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

data MintParams = MintParams
  { mpTokenName :: TokenName,
    mpAmount :: Integer
  }
  deriving (Show, Generic, FromJSON, ToJSON, ToSchema)

type MintingSchema = Endpoint "mint" MintParams

owner :: PubKeyHash
owner = pubKeyHash ownerPubKey

-- owner = PubKeyHash . toBuiltin . C.pack $ "ec8c7d111c04761ef362a0036d36893e7f04adde4afd5ea3e1e85914"
-- owner = PubKeyHash . toBuiltin . C.pack $ "acct_vk1tzpcn30n20dxmunenzuxfcg7m3p3ax3rvzfgefslhph8r5ppp0eqgee4vk"

ownerPubKey :: PubKey
ownerPubKey = PubKey . fromBytes . C.pack $ "acct_vk1tzpcn30n20dxmunenzuxfcg7m3p3ax3rvzfgefslhph8r5ppp0eqgee4vk"
-- ownerPubKey = walletPubKey (Wallet 1)

ownerString :: String
ownerString = C.unpack . bytes . getPubKey $ ownerPubKey

-- ownerPrivateKey :: PrivateKey
-- ownerPrivateKey = walletPrivKey (Wallet 1)

-- ownerAddress :: Address
-- ownerAddress = walletAddress (Wallet 1)

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
  logInfo @String $ "Owner => " <> show owner <> "."
  logInfo @String $ "Currency Symbol => " <> show curSymbol <> "."
  logInfo @String $ "forged => " <> show val <> " " <> show tknName <> "."

privateToken :: AsContractError e => Contract () MintingSchema e ()
privateToken = do
  logInfo @String $ "Owner Pub Key Hash=> " <> show owner <> "."
  logInfo @String $ "Owner Pub Key=> " <> show ownerPubKey <> "."
  -- logInfo @String $ "Owner Priv Key=> " <> show ownerPrivateKey <> "."
  -- logInfo @String $ "Owner Address => " <> show ownerAddress <> "."
  logInfo @String $ "Currency Symbol => " <> show curSymbol <> "."
  logInfo @String $ "Owner string => " <> show ownerString <> "."
  logInfo @String "Initialized Private Script"
  selectList [mint] >> privateToken