{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE NumericUnderscores    #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}


module Spec.StakingV1Trace
    ( tests
    , runMyTrace
    ) where

import           Control.Lens
import           Control.Monad           hiding ( fmap )
import           Control.Monad.Freer.Extras    as Extras
import           Data.Default                   ( Default(..) )
import qualified Data.Map                      as Map
import           Data.Monoid                    ( Last(..) )
import           Ledger
import           Ledger.Ada                    as Ada
import           Ledger.Value
import           Plutus.Contract.Test
import           Plutus.Trace.Emulator         as Emulator
import           PlutusTx.Prelude
import           Prelude                        ( IO
                                                , Show(..)
                                                , String
                                                )
import           StakingContract.StakingV1
import           Test.Tasty
import           Wallet.Emulator.Wallet

tests :: TestTree
tests = checkPredicateOptions
    (defaultCheckOptions & emulatorConfig .~ emCfg)
    "service execution trace"
    (    walletFundsChange
          (findWallet 1)
          (Ada.lovelaceValueOf 10_000_000 <> assetClassValue token (-60))
    .&&. walletFundsChange
             (findWallet 2)
             (Ada.lovelaceValueOf (-20_000_000) <> assetClassValue token 20)
    .&&. walletFundsChange
             (findWallet 3)
             (Ada.lovelaceValueOf (-5_000_000) <> assetClassValue token 5)
    )
    myTrace


runMyTrace :: IO ()
runMyTrace = runEmulatorTraceIO' def emCfg myTrace

emCfg :: EmulatorConfig
emCfg = EmulatorConfig
    (Left $ Map.fromList [ (findWallet w, v) | w <- [1 .. 3] ])
    def
    def
  where
    v :: Value
    v = Ada.lovelaceValueOf 1_000_000_000 <> assetClassValue token 1000

currency :: CurrencySymbol
currency = "aa"

name :: TokenName
name = "A"

token :: AssetClass
token = AssetClass (currency, name)

myTrace :: EmulatorTrace ()
myTrace = do
    h <- activateContractWallet (findWallet 1) startEndpoint
    callEndpoint @"start" h (currency, name, True)
    void $ Emulator.waitNSlots 5
    Last m <- observableState h
    case m of
        Nothing -> Extras.logError @String
            "error starting staking contract - service execution"
        Just se -> do
            Extras.logInfo $ "started Service execution " ++ show se
            h1 <- activateContractWallet (findWallet 1) $ useEndpoints se
            h2 <- activateContractWallet (findWallet 2) $ useEndpoints se
            h3 <- activateContractWallet (findWallet 3) $ useEndpoints se

            callEndpoint @"set service price" h1 1_000_000
            void $ Emulator.waitNSlots 5

            callEndpoint @"claim fee" h1 100
            void $ Emulator.waitNSlots 5

            callEndpoint @"deposit funds" h2 20
            void $ Emulator.waitNSlots 5

            callEndpoint @"withdraw unused" h3 5
            void $ Emulator.waitNSlots 5

findWallet :: Integer -> Wallet
findWallet num = fromWalletNumber $ WalletNumber num
