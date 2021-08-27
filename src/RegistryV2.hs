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

module RegistryV2 (
    registryV2,
    listorg,
    createorg,
    toByteStr,
    OrganizationData (..),
    RegistryV2Schema
) where

import           Control.Monad         (void)
import qualified PlutusTx
import           Playground.Contract
import           Plutus.Contract
import qualified Prelude               as Haskell
import qualified Ledger.Typed.Scripts  as Scripts
import qualified Data.ByteString.Char8 as C
import           PlutusTx.Prelude      hiding (pure, (<$>))
import           Ledger                (Address, ScriptContext, TxOutTx, Validator)
import qualified Ledger
import qualified Ledger.Constraints    as Constraints
import qualified Ledger.Ada            as Ada
import qualified Data.Map              as Map
import           Data.Maybe            (catMaybes)

type RegistryV2Schema =
    Endpoint "createorg" OrganizationData
    .\/ Endpoint "listorg" ()

data RegistryV2
instance Scripts.ValidatorTypes RegistryV2 where
    type instance RedeemerType RegistryV2 = ()
    type instance DatumType RegistryV2 = OrganizationData


data OrganizationData = OrganizationData {
        orgId :: BuiltinByteString,
        orgName ::  BuiltinByteString,
        orgOwner :: Ledger.PubKeyHash
    }
    deriving (Haskell.Show, Haskell.Eq, Generic)
    deriving anyclass (FromJSON, ToJSON, ToSchema)

PlutusTx.unstableMakeIsData ''OrganizationData
PlutusTx.makeLift ''OrganizationData


toByteStr :: Haskell.String -> BuiltinByteString
toByteStr = toBuiltin . C.pack

validate :: OrganizationData -> () -> ScriptContext -> Bool
validate _ _ _ = True


registryV2Instance :: Scripts.TypedValidator RegistryV2
registryV2Instance =  Scripts.mkTypedValidator @RegistryV2
    $$(PlutusTx.compile [|| validate ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator @OrganizationData @()


registryV2 :: AsContractError e => Contract () RegistryV2Schema e ()
registryV2 = do
    logInfo @Haskell.String "Initialized V2 Registry."
    selectList [createorg, listorg] >> registryV2

registryV2Validator :: Validator
registryV2Validator = Scripts.validatorScript registryV2Instance

registryV2Address :: Address
registryV2Address = Ledger.scriptAddress registryV2Validator


listorg :: AsContractError e =>Promise () RegistryV2Schema e ()
listorg = endpoint @"listorg" @() $ \_ -> do
    logInfo @Haskell.String "Listing organization"
    utxos <- utxoAt registryV2Address
    logInfo @Haskell.String $ "Total number of utxos is " <> Haskell.show (Map.size utxos) <> "."
    logInfo $ intercalateOrgNames $ retrieveOrgData utxos
    logInfo @Haskell.String "List Complete"


createorg :: AsContractError e => Promise () RegistryV2Schema e ()
createorg = endpoint @"createorg" @OrganizationData $ \(OrganizationData orgId orgName orgOwner) -> do
    logInfo @Haskell.String $ "Creating organization \n \
        \ with Id: " <> Haskell.show orgId <> ". \n \
        \ Name: " <> Haskell.show orgName <>". \n \
        \ Owner: " <> Haskell.show orgOwner <> "."
    let tx = Constraints.mustPayToTheScript (OrganizationData orgId orgName orgOwner) (Ada.lovelaceValueOf 1)
    logInfo "Created Txn Contraints"
    void(submitTxConstraints registryV2Instance tx)

    -- let tx = Constraints.mustIncludeDatum $ Datum $ toBuiltin OrganizationData 


printOrgData :: OrganizationData -> Contract () RegistryV2Schema e ()
printOrgData org = logInfo $ orgDataToString org

-- @Haskell.String $ " \
--                                         \ orgId: " <> Haskell.show orgId <> ". \n \
--                                         \ Name: " <> Haskell.show orgName <>". \n \
--                                         \ Owner: " <> Haskell.show orgOwner <> "."

orgDataToString :: OrganizationData -> Haskell.String 
orgDataToString (OrganizationData orgId orgName orgOwner) = " \
                                        \ orgId: " <> Haskell.show orgId <> ". \n \
                                        \ Name: " <> Haskell.show orgName <>". \n \
                                        \ Owner: " <> Haskell.show orgOwner <> "."

intercalateOrgNames :: Foldable t => t OrganizationData -> Haskell.String
intercalateOrgNames = foldr (\org acc -> acc ++ orgDataToString org ++ "\nNext Org =>") "First Org =>"

retrieveOrgData :: UtxoMap -> [OrganizationData]
retrieveOrgData = catMaybes . Map.elems . Map.map datumToOrg

datumToOrg :: TxOutTx  -> Maybe OrganizationData
datumToOrg o = do
    dh <- Ledger.txOutDatumHash $ Ledger.txOutTxOut o
    datum <- Map.lookup dh $ Ledger.txData $ Ledger.txOutTxTx o
    PlutusTx.fromBuiltinData $ Ledger.getDatum datum

