{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}


module OnChain.NFT where

-- Read more about NFTs and
-- how to create one, here https://plutus-pioneer-program.readthedocs.io/en/latest/pioneer/week5.html#nfts

import Ledger
import PlutusTx
import Plutus.V1.Ledger.Value
import qualified Ledger.Typed.Scripts as Scripts

mkPolicy :: TxOutRef -> TokenName -> BuiltinData -> ScriptContext -> Bool 
mkPolicy txOutRef tknName redeemer ctx = hasUtxo && checkMintedAmount
    where
        info :: TxInfo
        info = scriptContextTxInfo ctx

        hasUtxo :: Bool
        hasUtxo = any (\i -> txInInfoOutRef i == txOutRef) $ txInfoInputs info 

        checkMintedAmount :: Bool
        -- txInfoForge has been renamed as txInfoMint. #ref https://cardanoupdates.com/commits/f031d541665a996ccc30122ed3636c66823d34a7
        checkMintedAmount = case flattenValue (txInfoMint info) of 
            [(curSym, tknName', amt)] -> curSym == ownCurrencySymbol ctx && tknName' == tknName && amt == 1
            _                         -> False


policy :: TxOutRef -> TokenName -> Scripts.MintingPolicy 
policy txOutRef tknName = 
    mkMintingPolicyScript $
        $$(PlutusTx.compile [|| \txOutRef' tknName' -> Scripts.wrapMintingPolicy $ mkPolicy txOutRef' tknName' ||])
        `PlutusTx.applyCode` 
        PlutusTx.liftCode txOutRef 
        `PlutusTx.applyCode` 
        PlutusTx.liftCode tknName
