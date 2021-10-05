{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module StakingContract.StakingV1 (ServiceExecution(..), SERedeemer(..), SEStartSchema, SEUseSchema, startEndpoint, useEndpoints) where

import           Control.Monad                hiding (fmap)
import           Data.Aeson                   (FromJSON, ToJSON)
import           Data.Monoid                  (Last (..))
import           Data.Text                    (Text, pack)
import           GHC.Generics                 (Generic)
import           Plutus.Contract              as Contract
import           Plutus.Contract.StateMachine
import qualified PlutusTx
import           PlutusTx.Prelude             hiding (Semigroup(..), check, unless)
import           Ledger                       hiding (singleton)
import           Ledger.Ada                   as Ada
import           Ledger.Constraints           as Constraints
import qualified Ledger.Typed.Scripts         as Scripts
import           Ledger.Value
import           Prelude                      (Semigroup (..), Show (..), uncurry)
import qualified Prelude

owner :: PubKeyHash
owner = "b4ddb46e5c9eff21ecfbe426feec1ae54712f16f31e993742a804030"

tokenName :: Prelude.String
tokenName = "CHESS"

mkPolicy :: PubKeyHash -> BuiltinData -> ScriptContext -> Bool
mkPolicy pkh redeemer ctx = True

mkStakingValidator :: BuiltinData -> BuiltinData -> ScriptContext -> Bool
mkStakingValidator _datum _redeemer _ctx = True

data ServiceExecution = ServiceExecution
  { seOwner :: !PubKeyHash,
    seToken :: !AssetClass,
    seDaemon :: !PubKeyHash,
    seTT :: !(Maybe ThreadToken)
  }
  deriving (Show, Generic, FromJSON, ToJSON, Prelude.Eq)

PlutusTx.makeLift ''ServiceExecution

data SERedeemer
  = SetServicePrice Integer -- Set the price per call of the service
  | DepositFunds Integer -- Amount to be deposited in the script
  | ClaimFee Integer -- Amount to be claimed from the script by the serviceProvider (owner)
  | WithdrawUnused Integer -- Amount to be claimed from the script by the user
  deriving (Show, Prelude.Eq)

PlutusTx.unstableMakeIsData ''SERedeemer

{-# INLINABLE lovelaces #-}
lovelaces :: Value -> Integer
lovelaces = Ada.getLovelace . Ada.fromValue

{-# INLINABLE transition #-}
transition ::
  ServiceExecution ->
  State Integer ->
  SERedeemer ->
  Maybe (TxConstraints Void Void, State Integer)
transition se state redeemer =
  case (stateValue state, stateData state, redeemer) of
    (v, _, SetServicePrice p)
      | p >= 1 ->
        -- Just (Constraints.mustBeSignedBy (seOwner se), State p v)
        Just (mempty, State p v)
    (v, p, DepositFunds f)
      | p >= 1 ->
        Just (mempty, State p $ v <> assetClassValue (seToken se) f)
    (v, p, ClaimFee c)
      | c >= 1 ->
        Just
          -- ( Constraints.mustBeSignedBy (seDaemon se),
          --   State p $ v <> assetClassValue (seToken se) (negate c)
          -- )
          ( mempty ,
            State p $ v <> assetClassValue (seToken se) (negate c)
          )
    (v, p, WithdrawUnused u)
      | u >= 1 ->
        Just
          -- ( Constraints.mustBeSignedBy (seDaemon se),
          --   State p $ v <> assetClassValue (seToken se) (negate u)
          -- )
          ( mempty,
            State p $ v <> assetClassValue (seToken se) (negate u)
          )
    _ -> Nothing

{-# INLINABLE seStateMachine #-}
seStateMachine :: ServiceExecution -> StateMachine Integer SERedeemer
seStateMachine se =
  mkStateMachine (seTT se) (transition se) (const False)

-- CONVERT STATE MACHINE INTO PLUTUS CONTRACT BELOW

type SE = StateMachine Integer SERedeemer

seTypedValidator :: ServiceExecution -> Scripts.TypedValidator SE
seTypedValidator se =
  Scripts.mkTypedValidator @SE
    ($$(PlutusTx.compile [||mkSEValidator||]) `PlutusTx.applyCode` PlutusTx.liftCode se)
    $$(PlutusTx.compile [||wrap||])
  where
    wrap = Scripts.wrapValidator @Integer @SERedeemer

{-# INLINABLE mkSEValidator #-}
mkSEValidator :: ServiceExecution -> Integer -> SERedeemer -> ScriptContext -> Bool
mkSEValidator = mkValidator . seStateMachine

seValidator :: ServiceExecution -> Validator
seValidator = Scripts.validatorScript . seTypedValidator

seClient :: ServiceExecution -> StateMachineClient Integer SERedeemer
seClient se = mkStateMachineClient $ StateMachineInstance (seStateMachine se) (seTypedValidator se)

-- Parse Error
mapErrorSM :: Contract w s SMContractError a -> Contract w s Text a
mapErrorSM = mapError $ pack . show

-- OFFCHAIN CODE BELOW
nftName :: TokenName
nftName = "Service Execution"

startSE :: AssetClass -> Bool -> Contract (Last ServiceExecution) s Text ()
startSE token useTT = do
  pkh <- pubKeyHash <$> Contract.ownPubKey
  tt <- if useTT then Just <$> mapErrorSM getThreadToken else return Nothing
  let se =
        ServiceExecution
          { seOwner = pkh,
            seDaemon = pkh,
            seToken = token,
            seTT = tt
          }
      client = seClient se
  void $ mapErrorSM $ runInitialise client 0 mempty
  tell $ Last $ Just se
  logInfo $ "started service execution " ++ show se

-- OWNER Actions
setServicePrice :: ServiceExecution -> Integer -> Contract w s Text ()
setServicePrice se p = void $ mapErrorSM $ runStep (seClient se) $ SetServicePrice p

claimFee :: ServiceExecution -> Integer -> Contract w s Text ()
claimFee se amt = void $ mapErrorSM $ runStep (seClient se) $ ClaimFee amt

-- USER Actions
depositFunds :: ServiceExecution -> Integer -> Contract w s Text ()
depositFunds se amt = void $ mapErrorSM $ runStep (seClient se) $ DepositFunds amt

withdrawUnused :: ServiceExecution -> Integer -> Contract w s Text ()
withdrawUnused se amt = void $ mapErrorSM $ runStep (seClient se) $ WithdrawUnused amt

type SEStartSchema =
  Endpoint "start" (CurrencySymbol, TokenName, Bool)

type SEUseSchema =
  Endpoint "set service price" Integer
    .\/ Endpoint "claim fee" Integer
    .\/ Endpoint "deposit funds" Integer
    .\/ Endpoint "withdraw unused" Integer

startEndpoint :: Contract (Last ServiceExecution) SEStartSchema Text ()
startEndpoint = forever $
  handleError logError $
    awaitPromise $
      endpoint @"start" $ \(cs, tn, useTT) -> startSE (AssetClass (cs, tn)) useTT

useEndpoints :: ServiceExecution -> Contract () SEUseSchema Text ()
useEndpoints se =
  forever $
    handleError logError $
      awaitPromise $
        setServicePrice' `select` claimFee' `select` depositFunds' `select` withdrawUnused'
  where
    setServicePrice' = endpoint @"set service price" $ setServicePrice se
    claimFee' = endpoint @"claim fee" $ claimFee se
    depositFunds' = endpoint @"deposit funds" $ depositFunds se
    withdrawUnused' = endpoint @"withdraw unused" $ withdrawUnused se
