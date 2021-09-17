{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- TODO: Ledger currently always supplies a redeemer while the plutus-repo's
-- minting validator does not expect a redeemer. This needs to be rectified
-- before we can successfully use minting Plutus scripts.
-- TODO: We should potentially parameterize the script creation

module OnChain.MintingScriptParameterized
  ( apiExamplePlutusMintingScript
  , mintingScriptShortBs
  ) where

import           Prelude hiding (($))

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import           Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS

import           Ledger hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import           PlutusTx.Prelude hiding (Semigroup (..), unless)
import           Ledger.Value

{- HLINT ignore "Avoid lambda" -}


data MintParams = MintParams {
                    mpTokenName:: TokenName
                   }

PlutusTx.makeLift ''MintParams

{-# INLINABLE mkPolicy #-}
mkPolicy :: MintParams -> BuiltinData -> ScriptContext -> Bool
mkPolicy mp _redeemer _ctx = traceIfFalse "wrong token name" checkMinting
    where
        info :: TxInfo
        info = scriptContextTxInfo _ctx

        checkMinting :: Bool
        checkMinting = 


policy :: Scripts.MintingPolicy
policy mp = mkMintingPolicyScript
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode` 
        PlutusTx.makeLift ''mp


plutusScript :: Script
plutusScript  mp =
  unMintingPolicyScript (policy mp)
    where
        mp = MintParams {
                mpTokenName = "AGIX"
            }

validator :: Validator
validator =
  Validator $ plutusScript

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor