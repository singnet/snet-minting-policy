#! /bin/bash
cardano_cli="/home/ubuntu/testnet/lib/cardano-cli"
cardano_address="/home/ubuntu/testnet/lib/cardano-address/bin/cardano-address"
mainnet="--mainnet"
testnet="--testnet-magic 1097911063"
network=$testnet
todaysDate=$(date --iso-8601=date)
AGIX_DIGITS="10"
AGIX_MINT="15000000"
# We cannot mint more that 18,446,744,073,709,551,615 tokens. Restriction from cardano side.
MINT_QTY=$(AGIX_MINT * 10**AGIX_DIGITS)
NAME=".TGIX"
POLICYID="$(cat mintng-script.policyId)"
SCRIPT=mintng-script.json
TX_IN_HASH="e9142b42e970d09b7e4b1847c2bd48e81fee8a32b7c83802fb48bcfaafe8a3c3"
TX_IX="0"
TX_IN_VALUE="1000000000"
TX_IN="${TX_IN_HASH}#${TX_IX}"
# Issue with cardano-cli. Unable to calculate minimum amount needed to complete the transaction.
TX_OUT_AMT="1544798"
RAW_FEES="$($cardano_cli transaction calculate-min-fee --protocol-params-file old-protocol-params.json --tx-body-file ./transactions/minting-tx-$todaysDate.body --tx-in-count 1 --tx-out-count 2 --witness-count 3 $network)"
FEE=${RAW_FEES,0:-9}
CHANGE_QTY="$((TX_IN_VALUE - TX_OUT_AMT - FEE))"
CHANGE_ADDR="$(cat ../issuer/issuer-acct-0-payment-0.addr)"
CUSTODY_ADDR="$(cat ../custodian/custodian-acct-0-payment-0.addr)"
$cardano_cli transaction build-raw --alonzo-era --fee "$FEE" --tx-in "$TX_IN" --tx-out "${CUSTODY_ADDR}+${TX_OUT_AMT}+$MINT_QTY ${POLICYID}${NAME}" --tx-out "${CHANGE_ADDR}+${CHANGE_QTY}" --mint "$MINT_QTY ${POLICYID}${NAME}" --minting-script-file "$SCRIPT" --out-file ./transactions/minting-tx-$todaysDate.body