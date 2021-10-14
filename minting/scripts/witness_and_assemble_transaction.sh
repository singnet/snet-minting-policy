#! /bin/bash
cardano_cli="/home/ubuntu/testnet/lib/cardano-cli"
cardano_address="/home/ubuntu/testnet/lib/cardano-address/bin/cardano-address"
mainnet="--mainnet"
testnet="--testnet-magic 1097911063"
network=$testnet
todaysDate=$(date --iso-8601=date)
# Prepare witness
$cardano_cli transaction witness --tx-body-file ./transactions/minting-tx-$todaysDate.body --signing-key-file ./issuer/issuer-policy-0.skey $network --out-file ./witness/minting-tx-issuer-policy-$todaysDate.witness
$cardano_cli transaction witness --tx-body-file ./transactions/minting-tx-$todaysDate.body --signing-key-file ./issuer/issuer-acct-0-payment-0.skey $network --out-file ./witness/minting-tx-issuer-payment-$todaysDate.witness
$cardano_cli transaction witness --tx-body-file ./transactions/minting-tx-$todaysDate.body --signing-key-file ./custodian/custodian-acct-0-payment-0.skey $network --out-file ./witness/minting-tx-custodian-payment-$todaysDate.witness
# Assemble transaction
$cardano_cli transaction assemble --tx-body-file ./transactions/minting-tx-$todaysDate.body --witness-file ./witness/minting-tx-issuer-policy-$todaysDate.witness --witness-file ./witness/minting-tx-issuer-payment-$todaysDate.witness --witness-file ./witness/minting-tx-custodian-payment-$todaysDate.witness --out-file ./transactions/minting-tx-$todaysDate.signed