{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE PartialTypeSignatures      #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}


module Registry (
    registry, 
    createorg, 
    listorg, 
    RegistrySchema,
    CreateOrgParams(..)) where

import           Control.Monad         (void)
import qualified PlutusTx
import           Playground.Contract
import           Plutus.Contract
import qualified Prelude               as Haskell
import qualified Ledger.Typed.Scripts  as Scripts
import qualified Data.ByteString.Char8 as C
import           PlutusTx.Prelude      hiding (pure, (<$>))
import           Ledger                (Address, Datum (Datum), ScriptContext, TxOutTx, Validator)
import qualified Ledger
import qualified Ledger.Constraints    as Constraints
import qualified Ledger.Ada            as Ada
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)
import qualified Prelude               as Haskell

type RegistrySchema = 
    Endpoint "listorg" ()
    .\/ Endpoint "createorg" CreateOrgParams

data CreateOrgParams = CreateOrgParams {
        orgId :: Haskell.String,
        orgName :: Haskell.String
    } 
    deriving (Haskell.Eq, Haskell.Show, Generic)
    deriving (FromJSON, ToJSON, ToSchema, ToArgument)


data Registry
instance Scripts.ValidatorTypes Registry where
    type instance RedeemerType Registry = () -- Empty Redeemer
    type instance DatumType Registry = HashedString


newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData, Haskell.Show)

PlutusTx.makeLift ''HashedString

newtype ClearString = ClearString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

PlutusTx.makeLift ''ClearString


registryInstance :: Scripts.TypedValidator Registry
registryInstance = Scripts.mkTypedValidator @Registry
        $$(PlutusTx.compile [|| validateOrgId ||])
        $$(PlutusTx.compile [|| wrap ||]) where
            wrap = Scripts.wrapValidator @HashedString @()



validateOrgId :: HashedString -> () -> ScriptContext -> Bool
validateOrgId (HashedString actual) _ _ = True -- TODO: Don't allow if the org Id is already published




listorg :: AsContractError e => Promise () RegistrySchema e ()
listorg = endpoint @"listorg" @() $ \_ -> do
    logInfo @Haskell.String "Listing Organization..."
    utxos <- utxoAt registryAddress
    -- let tx = collectFromScript utxos ()
    let orgId = fromMaybe ( hashString "Not Found") (findOrg utxos)
    logInfo  @Haskell.String $ "The orgId from on-chain is => " <> Haskell.show orgId <> "!!"
    -- let log         <- logOrgIds orgIdList
    let tx       = collectFromScript utxos ()
    void (submitTxConstraintsSpending registryInstance utxos tx)

    -- TODO: Return the list of organizations 




createorg :: AsContractError e => Promise () RegistrySchema e ()
createorg = endpoint @"createorg" @CreateOrgParams $ \(CreateOrgParams orgId orgName) -> do
    logInfo @Haskell.String $ "Creating Organization with id: " <> Haskell.show  orgId <> "and name: " <> Haskell.show orgName <> "."
    let tx = Constraints.mustPayToTheScript  (hashString orgId) (Ada.lovelaceValueOf 1)
    void (submitTxConstraints registryInstance tx)
    -- TODO: Create a new organization and add to the list


registry :: AsContractError e => Contract () RegistrySchema e ()
registry = do
    logInfo @Haskell.String "Intialized Registry Contract..."
    selectList [listorg, createorg] >> registry

-- UTILS

registryValidator :: Validator
registryValidator = Scripts.validatorScript registryInstance

registryAddress :: Address
registryAddress = Ledger.scriptAddress registryValidator

-- create a data script for the Registry by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . toBuiltin . C.pack


-- unhashString :: HashedString -> Haskell.String
-- unhashString = fromBuiltinData . C.unpack


findOrg :: UtxoMap -> Maybe HashedString
findOrg = listToMaybe . catMaybes . Map.elems . Map.map  orgIdValue

orgIdValue :: TxOutTx -> Maybe HashedString
orgIdValue o = do
  dh <- Ledger.txOutDatum $ Ledger.txOutTxOut o
  Datum d <- Map.lookup dh $ Ledger.txData $ Ledger.txOutTxTx o
  PlutusTx.fromBuiltinData d


logOrgIds :: [HashedString] -> [Contract w0 s0 e0 ()]
logOrgIds list = do
     map logInfo $ map (\el -> ""<>Haskell.show el <> "") list