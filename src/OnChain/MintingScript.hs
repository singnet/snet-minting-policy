{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- TODO: Ledger currently always supplies a redeemer while the plutus-repo's
-- minting validator does not expect a redeemer. This needs to be rectified
-- before we can successfully use minting Plutus scripts.
-- TODO: We should potentially parameterize the script creation

module OnChain.MintingScript
  ( apiExamplePlutusMintingScript,
    mintingScriptShortBs,
    curSymbol,
    scrAddress,
  )
where

import Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)
import Codec.Serialise
import qualified Data.ByteString.Lazy as LB
import qualified Data.ByteString.Short as SBS
import Ledger hiding (singleton)
import qualified Ledger.Typed.Scripts as Scripts
import qualified PlutusTx
import PlutusTx.Prelude hiding (Semigroup (..), unless)
import Prelude hiding (($))

{- HLINT ignore "Avoid lambda" -}

{-# INLINEABLE mkPolicy #-}
mkPolicy :: BuiltinData -> ScriptContext -> Bool
mkPolicy _ _ = True

policy :: Scripts.MintingPolicy
policy =
  mkMintingPolicyScript
    $$(PlutusTx.compile [||Scripts.wrapMintingPolicy mkPolicy||])

curSymbol :: CurrencySymbol
curSymbol = scriptCurrencySymbol policy

scrAddress :: Address
scrAddress = Ledger.scriptAddress validator

plutusScript :: Script
plutusScript =
  unMintingPolicyScript policy

validator :: Validator
validator =
  Validator $ unMintingPolicyScript policy

scriptAsCbor :: LB.ByteString
scriptAsCbor = serialise validator

apiExamplePlutusMintingScript :: PlutusScript PlutusScriptV1
apiExamplePlutusMintingScript = PlutusScriptSerialised . SBS.toShort $ LB.toStrict scriptAsCbor

mintingScriptShortBs :: SBS.ShortByteString
mintingScriptShortBs = SBS.toShort . LB.toStrict $ scriptAsCbor