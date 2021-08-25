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

module           HelloWorld (
    HelloWorldSchema, 
    helloWorld) where

import qualified Prelude               as Haskell
import           Plutus.Contract
import           PlutusTx.Prelude      hiding (pure, (<$>))
import           Ledger


type HelloWorldSchema = Endpoint "greet" ()

newtype GuessParams = GuessParams
    { guessWord :: Haskell.String
    }
    deriving stock (Haskell.Eq, Haskell.Show)

helloWorld :: AsContractError e => Contract () HelloWorldSchema e ()
helloWorld = do
    logInfo "Initialized Hello World Contract"


greet :: AsContractError e => Promise () HelloWorldSchema e ()
greet = endpoint @"greet" @() $ \_ -> do
    logInfo "Halloo Welt"
