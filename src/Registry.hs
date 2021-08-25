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
    createOrg, 
    listOrg, 
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
import           Ledger                (ScriptContext)
import qualified Ledger.Constraints    as Constraints
import qualified Ledger.Ada            as Ada

type RegistrySchema = 
    Endpoint "listOrg" ()
    .\/ Endpoint "createOrg" CreateOrgParams

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


newtype HashedString = HashedString BuiltinByteString deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)

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




listOrg :: AsContractError e => Promise () RegistrySchema e ()
listOrg = endpoint @"listOrg" @() $ \_ -> do
    logInfo @Haskell.String "Listing Organization"
    -- TODO: Return the list of organizations 


-- create a data script for the Registry by hashing the string
-- and lifting the hash to its on-chain representation
hashString :: Haskell.String -> HashedString
hashString = HashedString . sha2_256 . toBuiltin . C.pack

createOrg :: AsContractError e => Promise () RegistrySchema e ()
createOrg = endpoint @"createOrg" @CreateOrgParams $ \(CreateOrgParams orgId orgName) -> do
    logInfo @Haskell.String $ "Creating Organization with id: " <> Haskell.show  orgId <> "and name: " <> Haskell.show orgName <> "."
    let tx = Constraints.mustPayToTheScript  (hashString orgId) (Ada.lovelaceValueOf 1)
    void (submitTxConstraints registryInstance tx)
    -- TODO: Create a new organization and add to the list


registry :: AsContractError e => Contract () RegistrySchema e ()
registry = do
    logInfo @Haskell.String "Intialized Registry Contract..."
    selectList [listOrg, createOrg] >> registry


