#! /bin/bash
cardano_cli="/home/ubuntu/testnet/lib/cardano-cli"
mainnet="--mainnet"
testnet="--testnet-magic 1097911063"
network=$testnet
todaysDate=$(date --iso-8601=date)
$cardano_cli transaction submit --tx-file ./transactions/minting-tx-$todaysDate.signed $network
