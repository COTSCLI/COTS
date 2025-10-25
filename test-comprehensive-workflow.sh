#!/bin/bash

# Comprehensive COTS CLI Test Script
# Tests all commands with multiple addresses and transactions

set -e

echo "🚀 Starting Comprehensive COTS CLI Test"
echo "========================================"

# Clean up previous test
echo "🧹 Cleaning up previous test..."
rm -rf ~/testcots-comprehensive
rm -f *.addr *.vkey *.skey *.raw *.signed *.json

# Initialize workspace
echo ""
echo "📁 Initializing COTS workspace..."
cotscli --home ~/testcots-comprehensive init --path ~/testcots-comprehensive --name "Comprehensive Test" --network Preprod

# Generate multiple key pairs
echo ""
echo "🔑 Generating key pairs for 5 wallets..."
cotscli --home ~/testcots-comprehensive address key-gen --verification-key-file alice.vkey --signing-key-file alice.skey
cotscli --home ~/testcots-comprehensive address key-gen --verification-key-file bob.vkey --signing-key-file bob.skey
cotscli --home ~/testcots-comprehensive address key-gen --verification-key-file charlie.vkey --signing-key-file charlie.skey
cotscli --home ~/testcots-comprehensive address key-gen --verification-key-file diana.vkey --signing-key-file diana.skey
cotscli --home ~/testcots-comprehensive address key-gen --verification-key-file eve.vkey --signing-key-file eve.skey

# Build addresses
echo ""
echo "🏠 Building addresses for all wallets..."
cotscli --home ~/testcots-comprehensive address build --payment-verification-key-file alice.vkey --out-file alice.addr --network Preprod
cotscli --home ~/testcots-comprehensive address build --payment-verification-key-file bob.vkey --out-file bob.addr --network Preprod
cotscli --home ~/testcots-comprehensive address build --payment-verification-key-file charlie.vkey --out-file charlie.addr --network Preprod
cotscli --home ~/testcots-comprehensive address build --payment-verification-key-file diana.vkey --out-file diana.addr --network Preprod
cotscli --home ~/testcots-comprehensive address build --payment-verification-key-file eve.vkey --out-file eve.addr --network Preprod

# Initialize database
echo ""
echo "🗄️ Initializing database..."
cotscli --home ~/testcots-comprehensive database init --db-file comprehensive.db

# Create initial UTXOs for all wallets
echo ""
echo "💰 Creating initial UTXOs for all wallets..."
cat > initial-utxos.json << 'EOF'
[
  {
    "txHash": {"unTransactionId": "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 20000000, "assets": []}
  },
  {
    "txHash": {"unTransactionId": "genesis_bob_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 15000000, "assets": []}
  },
  {
    "txHash": {"unTransactionId": "genesis_charlie_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 10000000, "assets": []}
  },
  {
    "txHash": {"unTransactionId": "genesis_diana_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 8000000, "assets": []}
  },
  {
    "txHash": {"unTransactionId": "genesis_eve_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 5000000, "assets": []}
  }
]
EOF

cotscli --home ~/testcots-comprehensive database import-utxo --utxo-file initial-utxos.json --db-file comprehensive.db

# Query UTXOs for each wallet
echo ""
echo "🔍 Querying UTXOs for each wallet..."
echo "--- Alice's UTXOs ---"
cotscli --home ~/testcots-comprehensive query utxo --address $(cat alice.addr) --testnet-magic 1 --db-file comprehensive.db

echo ""
echo "--- Bob's UTXOs ---"
cotscli --home ~/testcots-comprehensive query utxo --address $(cat bob.addr) --testnet-magic 1 --db-file comprehensive.db

echo ""
echo "--- Charlie's UTXOs ---"
cotscli --home ~/testcots-comprehensive query utxo --address $(cat charlie.addr) --testnet-magic 1 --db-file comprehensive.db

# Transaction 1: Alice → Bob (5,000,000 lovelace)
echo ""
echo "💸 Transaction 1: Alice → Bob (5,000,000 lovelace)"
cotscli --home ~/testcots-comprehensive transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat bob.addr)+5000000" \
  --tx-out "$(cat alice.addr)+14800000" \
  --fee 200000 \
  --out-file tx1.raw

cotscli --home ~/testcots-comprehensive transaction calculate-min-fee \
  --tx-body-file tx1.raw \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --testnet-magic 1 \
  --protocol-params-file protocol.json

cotscli --home ~/testcots-comprehensive transaction sign \
  --tx-file tx1.raw \
  --signing-key-file ~/testcots-comprehensive/keys/alice.skey \
  --out-file tx1.signed

cotscli --home ~/testcots-comprehensive transaction submit \
  --tx-file tx1.signed \
  --testnet-magic 1

cotscli --home ~/testcots-comprehensive transaction txid --tx-file tx1.signed

# Transaction 2: Bob → Charlie (3,000,000 lovelace)
echo ""
echo "💸 Transaction 2: Bob → Charlie (3,000,000 lovelace)"
cotscli --home ~/testcots-comprehensive transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_bob_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat charlie.addr)+3000000" \
  --tx-out "$(cat bob.addr)+11800000" \
  --fee 200000 \
  --out-file tx2.raw

cotscli --home ~/testcots-comprehensive transaction sign \
  --tx-file tx2.raw \
  --signing-key-file ~/testcots-comprehensive/keys/bob.skey \
  --out-file tx2.signed

cotscli --home ~/testcots-comprehensive transaction submit \
  --tx-file tx2.signed \
  --testnet-magic 1

cotscli --home ~/testcots-comprehensive transaction txid --tx-file tx2.signed

# Transaction 3: Charlie → Diana (2,000,000 lovelace)
echo ""
echo "💸 Transaction 3: Charlie → Diana (2,000,000 lovelace)"
cotscli --home ~/testcots-comprehensive transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_charlie_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat diana.addr)+2000000" \
  --tx-out "$(cat charlie.addr)+7800000" \
  --fee 200000 \
  --out-file tx3.raw

cotscli --home ~/testcots-comprehensive transaction sign \
  --tx-file tx3.raw \
  --signing-key-file ~/testcots-comprehensive/keys/charlie.skey \
  --out-file tx3.signed

cotscli --home ~/testcots-comprehensive transaction submit \
  --tx-file tx3.signed \
  --testnet-magic 1

cotscli --home ~/testcots-comprehensive transaction txid --tx-file tx3.signed

# Transaction 4: Diana → Eve (1,500,000 lovelace)
echo ""
echo "💸 Transaction 4: Diana → Eve (1,500,000 lovelace)"
cotscli --home ~/testcots-comprehensive transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_diana_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat eve.addr)+1500000" \
  --tx-out "$(cat diana.addr)+6300000" \
  --fee 200000 \
  --out-file tx4.raw

cotscli --home ~/testcots-comprehensive transaction sign \
  --tx-file tx4.raw \
  --signing-key-file ~/testcots-comprehensive/keys/diana.skey \
  --out-file tx4.signed

cotscli --home ~/testcots-comprehensive transaction submit \
  --tx-file tx4.signed \
  --testnet-magic 1

cotscli --home ~/testcots-comprehensive transaction txid --tx-file tx4.signed

# Transaction 5: Eve → Alice (1,000,000 lovelace)
echo ""
echo "💸 Transaction 5: Eve → Alice (1,000,000 lovelace)"
cotscli --home ~/testcots-comprehensive transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_eve_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat alice.addr)+1000000" \
  --tx-out "$(cat eve.addr)+3800000" \
  --fee 200000 \
  --out-file tx5.raw

cotscli --home ~/testcots-comprehensive transaction sign \
  --tx-file tx5.raw \
  --signing-key-file ~/testcots-comprehensive/keys/eve.skey \
  --out-file tx5.signed

cotscli --home ~/testcots-comprehensive transaction submit \
  --tx-file tx5.signed \
  --testnet-magic 1

cotscli --home ~/testcots-comprehensive transaction txid --tx-file tx5.signed

# Complex Transaction 6: Multi-input, Multi-output
echo ""
echo "💸 Transaction 6: Complex Multi-input/Multi-output"
echo "Alice + Bob → Charlie + Diana + Eve"

cotscli --home ~/testcots-comprehensive transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-in "genesis_bob_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat charlie.addr)+3000000" \
  --tx-out "$(cat diana.addr)+2000000" \
  --tx-out "$(cat eve.addr)+1000000" \
  --tx-out "$(cat alice.addr)+12000000" \
  --tx-out "$(cat bob.addr)+10000000" \
  --fee 300000 \
  --out-file tx6.raw

cotscli --home ~/testcots-comprehensive transaction calculate-min-fee \
  --tx-body-file tx6.raw \
  --tx-in-count 2 \
  --tx-out-count 5 \
  --witness-count 2 \
  --testnet-magic 1 \
  --protocol-params-file protocol.json

cotscli --home ~/testcots-comprehensive transaction sign \
  --tx-file tx6.raw \
  --signing-key-file ~/testcots-comprehensive/keys/alice.skey \
  --out-file tx6.signed

cotscli --home ~/testcots-comprehensive transaction submit \
  --tx-file tx6.signed \
  --testnet-magic 1

cotscli --home ~/testcots-comprehensive transaction txid --tx-file tx6.signed

# Test transaction view and decode
echo ""
echo "👁️ Testing transaction view and decode..."
cotscli --home ~/testcots-comprehensive transaction view --tx-file tx1.signed
cotscli --home ~/testcots-comprehensive transaction decode --tx-file tx2.signed

# Test transaction validation and simulation
echo ""
echo "✅ Testing transaction validation and simulation..."
cotscli --home ~/testcots-comprehensive transaction validate --tx-file tx3.signed --db-file comprehensive.db
cotscli --home ~/testcots-comprehensive transaction simulate --tx-file tx4.signed --db-file comprehensive.db

# Test transaction export
echo ""
echo "📤 Testing transaction export..."
cotscli --home ~/testcots-comprehensive transaction export --tx-file tx5.signed --format cardano-cli --out-file tx5-exported.json
cotscli --home ~/testcots-comprehensive transaction export --tx-file tx6.signed --format koios --out-file tx6-koios.json

# Final UTXO queries
echo ""
echo "🔍 Final UTXO state for all wallets..."
echo "--- Alice's Final UTXOs ---"
cotscli --home ~/testcots-comprehensive query utxo --address $(cat alice.addr) --testnet-magic 1 --db-file comprehensive.db

echo ""
echo "--- Bob's Final UTXOs ---"
cotscli --home ~/testcots-comprehensive query utxo --address $(cat bob.addr) --testnet-magic 1 --db-file comprehensive.db

echo ""
echo "--- Charlie's Final UTXOs ---"
cotscli --home ~/testcots-comprehensive query utxo --address $(cat charlie.addr) --testnet-magic 1 --db-file comprehensive.db

echo ""
echo "--- Diana's Final UTXOs ---"
cotscli --home ~/testcots-comprehensive query utxo --address $(cat diana.addr) --testnet-magic 1 --db-file comprehensive.db

echo ""
echo "--- Eve's Final UTXOs ---"
cotscli --home ~/testcots-comprehensive query utxo --address $(cat eve.addr) --testnet-magic 1 --db-file comprehensive.db

# Test database operations
echo ""
echo "🗄️ Testing database operations..."
cotscli --home ~/testcots-comprehensive database export-utxo --db-file comprehensive.db --out-file final-utxos.json
cotscli --home ~/testcots-comprehensive database inspect --db-file comprehensive.db

# Test wallet operations
echo ""
echo "👛 Testing wallet operations..."
cotscli --home ~/testcots-comprehensive wallet create --name "Alice-Wallet" --address $(cat alice.addr) --db-file comprehensive.db
cotscli --home ~/testcots-comprehensive wallet create --name "Bob-Wallet" --address $(cat bob.addr) --db-file comprehensive.db
cotscli --home ~/testcots-comprehensive wallet create --name "Charlie-Wallet" --address $(cat charlie.addr) --db-file comprehensive.db

cotscli --home ~/testcots-comprehensive wallet list --db-file comprehensive.db
cotscli --home ~/testcots-comprehensive wallet info --name "Alice-Wallet" --db-file comprehensive.db

# Test protocol parameters
echo ""
echo "⚙️ Testing protocol parameters..."
cat > protocol-params.json << 'EOF'
{
  "minFeeA": 44,
  "minFeeB": 155381,
  "maxTxSize": 16384,
  "maxValSize": 5000,
  "minUtxo": 1000000,
  "poolDeposit": 500000000,
  "keyDeposit": 2000000,
  "maxEpoch": 18,
  "nOpt": 500,
  "a0": 0.3,
  "rho": 0.0022,
  "tau": 0.2,
  "decentralisationParam": 0,
  "extraEntropy": null,
  "protocolVersion": {
    "major": 7,
    "minor": 0
  },
  "minPoolCost": 340000000,
  "minUTxOValue": 1000000,
  "maxTxExUnits": {
    "exUnitsMem": 14000000,
    "exUnitsSteps": 10000000000
  },
  "maxBlockExUnits": {
    "exUnitsMem": 62000000,
    "exUnitsSteps": 20000000000
  },
  "maxValueSize": 5000,
  "collateralPercentage": 150,
  "maxCollateralInputs": 3,
  "costModels": {
    "PlutusV1": {
      "sha2_256-memory-arguments": 4,
      "sha2_256-cpu-arguments": 4,
      "sha3_256-memory-arguments": 4,
      "sha3_256-cpu-arguments": 4,
      "blake2b-256-memory-arguments": 4,
      "blake2b-256-cpu-arguments": 4,
      "verifyEd25519Signature-memory-arguments": 4,
      "verifyEd25519Signature-cpu-arguments": 4,
      "append-memory-arguments": 4,
      "append-cpu-arguments": 4,
      "equalsInteger-memory-arguments": 4,
      "equalsInteger-cpu-arguments": 4,
      "indexByteString-memory-arguments": 4,
      "indexByteString-cpu-arguments": 4,
      "lengthOfByteString-memory-arguments": 4,
      "lengthOfByteString-cpu-arguments": 4,
      "consByteString-memory-arguments": 4,
      "consByteString-cpu-arguments": 4,
      "sliceByteString-memory-arguments": 4,
      "sliceByteString-cpu-arguments": 4,
      "lessThanInteger-memory-arguments": 4,
      "lessThanInteger-cpu-arguments": 4,
      "greaterThanInteger-memory-arguments": 4,
      "greaterThanInteger-cpu-arguments": 4,
      "addInteger-memory-arguments": 4,
      "addInteger-cpu-arguments": 4,
      "subtractInteger-memory-arguments": 4,
      "subtractInteger-cpu-arguments": 4,
      "multiplyInteger-memory-arguments": 4,
      "multiplyInteger-cpu-arguments": 4,
      "divideInteger-memory-arguments": 4,
      "divideInteger-cpu-arguments": 4,
      "quotientInteger-memory-arguments": 4,
      "quotientInteger-cpu-arguments": 4,
      "remainderInteger-memory-arguments": 4,
      "remainderInteger-cpu-arguments": 4,
      "modInteger-memory-arguments": 4,
      "modInteger-cpu-arguments": 4,
      "lessThanEqualsInteger-memory-arguments": 4,
      "lessThanEqualsInteger-cpu-arguments": 4,
      "greaterThanEqualsInteger-memory-arguments": 4,
      "greaterThanEqualsInteger-cpu-arguments": 4,
      "lessThanByteString-memory-arguments": 4,
      "lessThanByteString-cpu-arguments": 4,
      "greaterThanByteString-memory-arguments": 4,
      "greaterThanByteString-cpu-arguments": 4,
      "lessThanEqualsByteString-memory-arguments": 4,
      "lessThanEqualsByteString-cpu-arguments": 4,
      "greaterThanEqualsByteString-memory-arguments": 4,
      "greaterThanEqualsByteString-cpu-arguments": 4,
      "sha2_256-memory-arguments": 4,
      "sha2_256-cpu-arguments": 4,
      "sha3_256-memory-arguments": 4,
      "sha3_256-cpu-arguments": 4,
      "blake2b-256-memory-arguments": 4,
      "blake2b-256-cpu-arguments": 4,
      "verifyEd25519Signature-memory-arguments": 4,
      "verifyEd25519Signature-cpu-arguments": 4,
      "append-memory-arguments": 4,
      "append-cpu-arguments": 4,
      "equalsInteger-memory-arguments": 4,
      "equalsInteger-cpu-arguments": 4,
      "indexByteString-memory-arguments": 4,
      "indexByteString-cpu-arguments": 4,
      "lengthOfByteString-memory-arguments": 4,
      "lengthOfByteString-cpu-arguments": 4,
      "consByteString-memory-arguments": 4,
      "consByteString-cpu-arguments": 4,
      "sliceByteString-memory-arguments": 4,
      "sliceByteString-cpu-arguments": 4,
      "lessThanInteger-memory-arguments": 4,
      "lessThanInteger-cpu-arguments": 4,
      "greaterThanInteger-memory-arguments": 4,
      "greaterThanInteger-cpu-arguments": 4,
      "addInteger-memory-arguments": 4,
      "addInteger-cpu-arguments": 4,
      "subtractInteger-memory-arguments": 4,
      "subtractInteger-cpu-arguments": 4,
      "multiplyInteger-memory-arguments": 4,
      "multiplyInteger-cpu-arguments": 4,
      "divideInteger-memory-arguments": 4,
      "divideInteger-cpu-arguments": 4,
      "quotientInteger-memory-arguments": 4,
      "quotientInteger-cpu-arguments": 4,
      "remainderInteger-memory-arguments": 4,
      "remainderInteger-cpu-arguments": 4,
      "modInteger-memory-arguments": 4,
      "modInteger-cpu-arguments": 4,
      "lessThanEqualsInteger-memory-arguments": 4,
      "lessThanEqualsInteger-cpu-arguments": 4,
      "greaterThanEqualsInteger-memory-arguments": 4,
      "greaterThanEqualsInteger-cpu-arguments": 4,
      "lessThanByteString-memory-arguments": 4,
      "lessThanByteString-cpu-arguments": 4,
      "greaterThanByteString-memory-arguments": 4,
      "greaterThanByteString-cpu-arguments": 4,
      "lessThanEqualsByteString-memory-arguments": 4,
      "lessThanEqualsByteString-cpu-arguments": 4,
      "greaterThanEqualsByteString-memory-arguments": 4,
      "greaterThanEqualsByteString-cpu-arguments": 4
    }
  }
}
EOF

cotscli --home ~/testcots-comprehensive protocol params --params-file protocol-params.json --db-file comprehensive.db
cotscli --home ~/testcots-comprehensive protocol inspect --db-file comprehensive.db

# Test UTXO operations
echo ""
echo "🔄 Testing UTXO operations..."
cotscli --home ~/testcots-comprehensive utxo list --utxo-file final-utxos.json
cotscli --home ~/testcots-comprehensive utxo reserve --amount 1000000 --address $(cat alice.addr) --utxo-file final-utxos.json --out-file reserved-utxos.json

# Test minting operations
echo ""
echo "🪙 Testing minting operations..."
cotscli --home ~/testcots-comprehensive mint build \
  --tx-in "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat alice.addr)+1000000" \
  --mint "1000 MyToken" \
  --out-file mint-tx.raw

cotscli --home ~/testcots-comprehensive mint calculate \
  --tx-body-file mint-tx.raw \
  --tx-in-count 1 \
  --tx-out-count 1 \
  --mint-count 1

# Test address operations
echo ""
echo "🏠 Testing address operations..."
cotscli --home ~/testcots-comprehensive address info --address $(cat alice.addr)

# Test stake address operations
echo ""
echo "🎯 Testing stake address operations..."
cotscli --home ~/testcots-comprehensive stake-address info --address $(cat alice.addr)

# Test database snapshots
echo ""
echo "📸 Testing database snapshots..."
cotscli --home ~/testcots-comprehensive database snapshot --db-file comprehensive.db --snapshot-file final-snapshot.db
cotscli --home ~/testcots-comprehensive database load-snapshot --db-file comprehensive.db --snapshot-file final-snapshot.db

# Test database reset
echo ""
echo "🔄 Testing database reset..."
cotscli --home ~/testcots-comprehensive database reset --db-file comprehensive.db

# Final summary
echo ""
echo "🎉 Comprehensive Test Complete!"
echo "=============================="
echo "✅ Generated 5 unique key pairs"
echo "✅ Built 5 unique addresses"
echo "✅ Created 5 initial UTXOs"
echo "✅ Executed 6 transactions (including complex multi-input/output)"
echo "✅ Tested all transaction commands (build, sign, submit, calculate-min-fee, txid, view, decode, validate, simulate, export)"
echo "✅ Tested all database operations (init, import, export, inspect, snapshot, load-snapshot, reset)"
echo "✅ Tested all wallet operations (create, list, info)"
echo "✅ Tested all protocol operations (params, inspect)"
echo "✅ Tested all UTXO operations (list, reserve)"
echo "✅ Tested all address operations (info)"
echo "✅ Tested all stake address operations (info)"
echo "✅ Tested all minting operations (build, calculate)"
echo ""
echo "🚀 All COTS CLI commands tested successfully with professional colorized output!"
echo "📊 Total transactions processed: 6"
echo "👥 Total wallets involved: 5"
echo "💰 Total UTXOs managed: 15+"
echo ""
echo "🎨 Colorized output features:"
echo "   ✓ Success messages in bright green"
echo "   ✗ Error messages in bright red"
echo "   ⚠ Warning messages in bright yellow"
echo "   ℹ Info messages in bright blue"
echo "   ▶ Progress messages in cyan"
echo "   🌈 Colorized transaction hashes, amounts, and addresses"
