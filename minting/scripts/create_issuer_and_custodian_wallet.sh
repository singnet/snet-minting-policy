#! /bin/bash
cardano_cli="/home/ubuntu/testnet/lib/cardano-cli"
cardano_address="/home/ubuntu/testnet/lib/cardano-address/bin/cardano-address"
mainnet="--mainnet"
testnet="--testnet-magic 1097911063"
network=$testnet
$cardano_address --version
$cardano_cli --version
# Create issuer wallet
DIRNAME=issuer
echo $DIRNAME
rm -r $DIRNAME/*
cd $DIRNAME
$cardano_address recovery-phrase generate > $DIRNAME.mnemonic
$cardano_address key from-recovery-phrase Shelley < $DIRNAME.mnemonic > $DIRNAME-root.xsk
$cardano_address key child "1855H/1815H/0H" < $DIRNAME-root.xsk > $DIRNAME-policy-0.xsk
$cardano_cli key convert-cardano-address-key --shelley-payment-key --signing-key-file $DIRNAME-policy-0.xsk --out-file $DIRNAME-policy-0.skey
$cardano_cli key verification-key --signing-key-file $DIRNAME-policy-0.skey --verification-key-file $DIRNAME-policy-0.vkeyx
$cardano_cli key non-extended-key --extended-verification-key-file $DIRNAME-policy-0.vkeyx --verification-key-file $DIRNAME-policy-0.vkey
$cardano_address key child "1852H/1815H/0H" < $DIRNAME-root.xsk > $DIRNAME-acct-0.xsk
$cardano_address key public --with-chain-code < $DIRNAME-acct-0.xsk > $DIRNAME-acct-0.xvk
$cardano_address key child "0/0" < $DIRNAME-acct-0.xsk > $DIRNAME-acct-0-payment-0.xsk
$cardano_cli key convert-cardano-address-key --shelley-payment-key --signing-key-file $DIRNAME-acct-0-payment-0.xsk --out-file $DIRNAME-acct-0-payment-0.skey
$cardano_cli key verification-key --signing-key-file $DIRNAME-acct-0-payment-0.skey --verification-key-file $DIRNAME-acct-0-payment-0.vkeyx
$cardano_cli key non-extended-key --extended-verification-key-file $DIRNAME-acct-0-payment-0.vkeyx --verification-key-file $DIRNAME-acct-0-payment-0.vkey
$cardano_cli address build --payment-verification-key-file $DIRNAME-acct-0-payment-0.vkey --out-file $DIRNAME-acct-0-payment-0.addr $network
$cardano_cli address key-hash --payment-verification-key-file $DIRNAME-policy-0.vkey > $DIRNAME-policy-0.keyHash
cd ..
# Create custodian wallet
DIRNAME=custodian
echo $DIRNAME
rm -r $DIRNAME/*
cd $DIRNAME
$cardano_address recovery-phrase generate > $DIRNAME.mnemonic
$cardano_address key from-recovery-phrase Shelley < $DIRNAME.mnemonic > $DIRNAME-root.xsk
$cardano_address key child "1855H/1815H/0H" < $DIRNAME-root.xsk > $DIRNAME-policy-0.xsk
$cardano_cli key convert-cardano-address-key --shelley-payment-key --signing-key-file $DIRNAME-policy-0.xsk --out-file $DIRNAME-policy-0.skey
$cardano_cli key verification-key --signing-key-file $DIRNAME-policy-0.skey --verification-key-file $DIRNAME-policy-0.vkeyx
$cardano_cli key non-extended-key --extended-verification-key-file $DIRNAME-policy-0.vkeyx --verification-key-file $DIRNAME-policy-0.vkey
$cardano_address key child "1852H/1815H/0H" < $DIRNAME-root.xsk > $DIRNAME-acct-0.xsk
$cardano_address key public --with-chain-code < $DIRNAME-acct-0.xsk > $DIRNAME-acct-0.xvk
$cardano_address key child "0/0" < $DIRNAME-acct-0.xsk > $DIRNAME-acct-0-payment-0.xsk
$cardano_cli key convert-cardano-address-key --shelley-payment-key --signing-key-file $DIRNAME-acct-0-payment-0.xsk --out-file $DIRNAME-acct-0-payment-0.skey
$cardano_cli key verification-key --signing-key-file $DIRNAME-acct-0-payment-0.skey --verification-key-file $DIRNAME-acct-0-payment-0.vkeyx
$cardano_cli key non-extended-key --extended-verification-key-file $DIRNAME-acct-0-payment-0.vkeyx --verification-key-file $DIRNAME-acct-0-payment-0.vkey
$cardano_cli address build --payment-verification-key-file $DIRNAME-acct-0-payment-0.vkey --out-file $DIRNAME-acct-0-payment-0.addr $network
$cardano_cli address key-hash --payment-verification-key-file $DIRNAME-policy-0.vkey > $DIRNAME-policy-0.keyHash
cd ..
