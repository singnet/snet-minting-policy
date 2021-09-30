{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- TODO: Ledger currently always supplies a redeemer while the plutus-repo's
-- minting validator does not expect a redeemer. This needs to be rectified
-- before we can successfully use minting Plutus scripts.
-- TODO: We should potentially parameterize the script creation

module OnChain.MintingTokenWithOwner
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


{- HLINT ignore "Avoid lambda" -}

-- data MintParams = MintParams
--     { pkeyHash :: !PubKeyHash
--     } deriving Show

-- PlutusTx.makeLift ''MintParams

keyHash1 :: PubKeyHash
keyHash1 = "b4ddb46e5c9eff21ecfbe426feec1ae54712f16f31e993742a804030"

{-# INLINABLE mkPolicy #-}
mkPolicy :: PubKeyHash -> BuiltinData -> ScriptContext -> Bool
mkPolicy pkh _redeemer ctx = txSignedBy (scriptContextTxInfo ctx) pkh

policy :: Scripts.MintingPolicy
policy = mkMintingPolicyScript $
    $$(PlutusTx.compile [|| Scripts.wrapMintingPolicy . mkPolicy ||])
    `PlutusTx.applyCode`
     PlutusTx.liftCode keyHash1

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
