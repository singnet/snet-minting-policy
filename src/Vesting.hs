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
{-# LANGUAGE NamedFieldPuns             #-}

module Vesting(VestingSchema, vesting) where

import           Control.Monad                     (void, when)
import qualified Data.Map                          as Map
import qualified Data.Text                         as T

import           Plutus.Contract                   hiding (when)
import qualified Plutus.Contract.Typed.Tx          as Typed
import qualified PlutusTx                          as PlutusTx
import           PlutusTx.Prelude                  hiding (Semigroup (..), fold)
import           Ledger                            (Address, POSIXTime, POSIXTimeRange, PubKeyHash, Slot (Slot), Validator, pubKeyHash)
import qualified Ledger.Ada                        as Ada
import           Ledger.Constraints                (TxConstraints, mustBeSignedBy, mustPayToTheScript, mustValidateIn)
import           Ledger.Contexts                   (
    TxInfo (..) , 
    ScriptContext (..)
    )
import qualified Ledger.Contexts                   as Validation
import qualified Ledger.Interval                   as Interval
import qualified Ledger.Slot                       as Slot
import qualified Ledger.Tx                         as Tx
import qualified Ledger.Typed.Scripts              as Scripts
import           Ledger.Value                      (Value)
import qualified Ledger.Value                      as Value
import           Playground.Contract
import           Wallet.Emulator.Types             (walletPubKey)           
import qualified Prelude                           as Haskell (Semigroup (..), show, Eq, Show)

type VestingSchema = 
    Endpoint "vest_funds" ()
    .\/ Endpoint "retrieve_funds" Value

data VestingTranche = VestingTranche {
    vestingTrancheDate :: POSIXTime, 
    vestingTrancheAmount ::  Value
    } 
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''VestingTranche

data VestingParams = VestingParams {
    vestingTranche1 :: VestingTranche,
    vestingTranche2 :: VestingTranche, 
    vestingOwner :: PubKeyHash
    } 
    deriving stock (Haskell.Eq, Haskell.Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

PlutusTx.makeLift ''VestingParams

{-# INLINABLE totalAmount #-}
totalAmount :: VestingParams -> Value
totalAmount VestingParams{vestingTranche1, vestingTranche2} = 
    vestingTrancheAmount vestingTranche1 + vestingTrancheAmount vestingTranche2

availableFrom :: VestingTranche -> POSIXTimeRange -> Value
availableFrom (VestingTranche d v) range =
    let validRange = Interval.from d
    in if validRange `Interval.contains` range then v else zero

availableAt :: VestingParams -> POSIXTime -> Value
availableAt VestingParams{vestingTranche1, vestingTranche2} sl =
    let f VestingTranche{vestingTrancheDate, vestingTrancheAmount} =
            if sl >= vestingTrancheDate then vestingTrancheAmount else mempty
    in foldMap f [vestingTranche1, vestingTranche2]

remainingFrom :: VestingTranche -> POSIXTimeRange -> Value
remainingFrom t@VestingTranche{vestingTrancheAmount} range = 
    vestingTrancheAmount - availableFrom t range


validate :: VestingParams -> () -> () -> ScriptContext -> Bool
validate VestingParams{vestingTranche1, vestingTranche2, vestingOwner} () () ctx@ScriptContext{scriptContextTxInfo=txInfo@TxInfo{txInfoValidRange}} = 
    let
        remainingActual = Validation.valueLockedBy txInfo (Validation.ownHash ctx)

        remainingExpected = 
            remainingFrom vestingTranche1 txInfoValidRange
            + remainingFrom vestingTranche2 txInfoValidRange

    in
        remainingActual `Value.geq` remainingExpected
        && Validation.txSignedBy txInfo vestingOwner


data Vesting
instance Scripts.ValidatorTypes Vesting where
    type instance RedeemerType Vesting = ()
    type instance DatumType Vesting = ()

vestingScript :: VestingParams -> Validator
vestingScript = Scripts.validatorScript . typedValidator

typedValidator :: VestingParams ->  Scripts.TypedValidator Vesting
typedValidator = Scripts.mkTypedValidatorParam @Vesting
    $$(PlutusTx.compile [|| validate ||])  
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.wrapValidator


contractAddress :: VestingParams -> Ledger.Address
contractAddress = Scripts.validatorAddress . typedValidator

vestingContract :: VestingParams -> Contract () VestingSchema T.Text ()
vestingContract vesting = selectList [vest, retrieve]
    where
        vest = endpoint @"vest_funds" $ \_ -> vestFundsC vesting
        retrieve = endpoint @"retrieve_funds" $ \payment -> do  
            liveness <- retrieveFundsC vesting payment
            case liveness of
                Alive -> awaitPromise retrieve
                Dead -> pure ()


payIntoContract :: Value -> TxConstraints () ()
payIntoContract value = mustPayToTheScript () value

vestFundsC :: VestingParams -> Contract () s T.Text ()
vestFundsC vesting = do
    let tx = payIntoContract (totalAmount vesting)
    void $ submitTxConstraints (typedValidator vesting) tx

data Liveness = Alive | Dead

retrieveFundsC :: VestingParams -> Value -> Contract () s T.Text Liveness
retrieveFundsC vesting payment = do 
    let inst = typedValidator vesting
        addr = Scripts.validatorAddress inst
    nextTime <- awaitTime 0
    unspentOutputs <- utxoAt addr
    let 
        currentlyLocked = foldMap (Validation.txOutValue . Tx.txOutTxOut . snd) (Map.toList unspentOutputs)
        remainingValue = currentlyLocked - payment
        mustRemainLocked = totalAmount vesting - availableAt vesting nextTime
        maxPayment = currentlyLocked - mustRemainLocked

    when (remainingValue `Value.lt` mustRemainLocked)
        $ throwError 
        $ T.unwords
            [ T.pack "Cannot take out"
            , T.pack (Haskell.show payment) `T.append`  T.pack "."
            , T.pack "The maximum is"
            , T.pack (Haskell.show maxPayment) `T.append`  T.pack "."
            , T.pack "At least"
            , T.pack (Haskell.show mustRemainLocked)
            , T.pack "must remain locked by the script."
            ]
    let liveness = if remainingValue `Value.gt` mempty then Alive else Dead
        remainingOutputs = case liveness of
                            Alive -> payIntoContract remainingValue
                            Dead -> mempty
        tx = Typed.collectFromScript unspentOutputs ()
                    Haskell.<> remainingOutputs
                    Haskell.<> mustValidateIn (Interval.from nextTime)
                    Haskell.<> mustBeSignedBy (vestingOwner vesting)
    void $ submitTxConstraintsSpending inst unspentOutputs tx
    return liveness

vesting :: AsContractError e => Contract () VestingSchema e ()
vesting = do
    logInfo "Initialized the Vesting Contract"


-- vest_funds :: AsContractError e => Contract () VestingSchema e ()
-- vest_funds = endpoint @"vest_funds" @Vesting $ \_ -> do
--     logInfo "vesting_funds in the contract"


-- vest_funds vesting = endpoint @"vest_funds" @() $ \_ -> do
--     let inst = typedValidator vesting
--     logInfo  "Vesting Funds "
--     let tx = mustPayToTheScript _ (Ada.lovelaceValueOf 1)
--     void (submitTxConstraints vesting tx)



