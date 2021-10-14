#! /bin/bash
issuerHash="$(< ./issuer/issuer-policy-0.keyHash)"
custodianHash="$(< ./custodian/custodian-policy-0.keyHash)"
echo $issuerHash
cat << EOF > minting-script.json
{
"type": "all",
"scripts":
[
{
"type": "sig",
"keyHash": "$issuerHash"
},
{
"type": "sig",
"keyHash": "$custodianHash"
}
]
}
EOF
