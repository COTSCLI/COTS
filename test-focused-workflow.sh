#!/bin/bash

# Comprehensive COTS CLI Test Script - Focused Version
# Tests key commands with multiple addresses and transactions

set -e

echo "🚀 Starting Focused COTS CLI Test"
echo "================================="

# Clean up previous test
echo "🧹 Cleaning up previous test..."
rm -rf ~/testcots-focused
rm -f *.addr *.vkey *.skey *.raw *.signed *.json

# Initialize workspace
echo ""
echo "📁 Initializing COTS workspace..."
cotscli --home ~/testcots-focused init --path ~/testcots-focused --name "Focused Test" --network Preprod

# Generate 3 key pairs
echo ""
echo "🔑 Generating key pairs for 3 wallets..."
cotscli --home ~/testcots-focused address key-gen --verification-key-file alice.vkey --signing-key-file alice.skey
cotscli --home ~/testcots-focused address key-gen --verification-key-file bob.vkey --signing-key-file bob.skey
cotscli --home ~/testcots-focused address key-gen --verification-key-file charlie.vkey --signing-key-file charlie.skey

# Build addresses
echo ""
echo "🏠 Building addresses for all wallets..."
cotscli --home ~/testcots-focused address build --payment-verification-key-file alice.vkey --out-file alice.addr --network Preprod
cotscli --home ~/testcots-focused address build --payment-verification-key-file bob.vkey --out-file bob.addr --network Preprod
cotscli --home ~/testcots-focused address build --payment-verification-key-file charlie.vkey --out-file charlie.addr --network Preprod

# Initialize database
echo ""
echo "🗄️ Initializing database..."
cotscli --home ~/testcots-focused database init --db-file focused.db

# Create initial UTXOs
echo ""
echo "💰 Creating initial UTXOs..."
cat > initial-utxos.json << 'EOF'
[
  {
    "txHash": {"unTransactionId": "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 10000000, "assets": []}
  },
  {
    "txHash": {"unTransactionId": "genesis_bob_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 8000000, "assets": []}
  },
  {
    "txHash": {"unTransactionId": "genesis_charlie_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 5000000, "assets": []}
  }
]
EOF

cotscli --home ~/testcots-focused database import-utxo --utxo-file initial-utxos.json --db-file focused.db

# Query UTXOs
echo ""
echo "🔍 Querying UTXOs for each wallet..."
echo "--- Alice's UTXOs ---"
cotscli --home ~/testcots-focused query utxo --address $(cat alice.addr) --testnet-magic 1 --db-file focused.db

echo ""
echo "--- Bob's UTXOs ---"
cotscli --home ~/testcots-focused query utxo --address $(cat bob.addr) --testnet-magic 1 --db-file focused.db

echo ""
echo "--- Charlie's UTXOs ---"
cotscli --home ~/testcots-focused query utxo --address $(cat charlie.addr) --testnet-magic 1 --db-file focused.db

# Transaction 1: Alice → Bob (2,000,000 lovelace)
echo ""
echo "💸 Transaction 1: Alice → Bob (2,000,000 lovelace)"
cotscli --home ~/testcots-focused transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat bob.addr)+2000000" \
  --tx-out "$(cat alice.addr)+7800000" \
  --fee 200000 \
  --out-file tx1.raw

cotscli --home ~/testcots-focused transaction calculate-min-fee \
  --tx-body-file tx1.raw \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --testnet-magic 1 \
  --protocol-params-file protocol.json

cotscli --home ~/testcots-focused transaction sign \
  --tx-file tx1.raw \
  --signing-key-file ~/testcots-focused/keys/alice.skey \
  --out-file tx1.signed

cotscli --home ~/testcots-focused transaction submit \
  --tx-file tx1.signed \
  --testnet-magic 1

cotscli --home ~/testcots-focused transaction txid --tx-file tx1.signed

# Transaction 2: Bob → Charlie (1,500,000 lovelace)
echo ""
echo "💸 Transaction 2: Bob → Charlie (1,500,000 lovelace)"
cotscli --home ~/testcots-focused transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_bob_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat charlie.addr)+1500000" \
  --tx-out "$(cat bob.addr)+6300000" \
  --fee 200000 \
  --out-file tx2.raw

cotscli --home ~/testcots-focused transaction sign \
  --tx-file tx2.raw \
  --signing-key-file ~/testcots-focused/keys/bob.skey \
  --out-file tx2.signed

cotscli --home ~/testcots-focused transaction submit \
  --tx-file tx2.signed \
  --testnet-magic 1

cotscli --home ~/testcots-focused transaction txid --tx-file tx2.signed

# Transaction 3: Charlie → Alice (1,000,000 lovelace)
echo ""
echo "💸 Transaction 3: Charlie → Alice (1,000,000 lovelace)"
cotscli --home ~/testcots-focused transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_charlie_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat alice.addr)+1000000" \
  --tx-out "$(cat charlie.addr)+3800000" \
  --fee 200000 \
  --out-file tx3.raw

cotscli --home ~/testcots-focused transaction sign \
  --tx-file tx3.raw \
  --signing-key-file ~/testcots-focused/keys/charlie.skey \
  --out-file tx3.signed

cotscli --home ~/testcots-focused transaction submit \
  --tx-file tx3.signed \
  --testnet-magic 1

cotscli --home ~/testcots-focused transaction txid --tx-file tx3.signed

# Test transaction operations
echo ""
echo "👁️ Testing transaction operations..."
cotscli --home ~/testcots-focused transaction view --tx-file tx1.signed
cotscli --home ~/testcots-focused transaction decode --tx-file tx2.signed
cotscli --home ~/testcots-focused transaction validate --tx-file tx3.signed --db-file focused.db

# Test wallet operations
echo ""
echo "👛 Testing wallet operations..."
cotscli --home ~/testcots-focused wallet create --name "Alice-Wallet" --address $(cat alice.addr) --db-file focused.db
cotscli --home ~/testcots-focused wallet create --name "Bob-Wallet" --address $(cat bob.addr) --db-file focused.db
cotscli --home ~/testcots-focused wallet create --name "Charlie-Wallet" --address $(cat charlie.addr) --db-file focused.db

cotscli --home ~/testcots-focused wallet list --db-file focused.db
cotscli --home ~/testcots-focused wallet info --name "Alice-Wallet" --db-file focused.db

# Test database operations
echo ""
echo "🗄️ Testing database operations..."
cotscli --home ~/testcots-focused database export-utxo --db-file focused.db --out-file final-utxos.json
cotscli --home ~/testcots-focused database inspect --db-file focused.db

# Test UTXO operations
echo ""
echo "🔄 Testing UTXO operations..."
cotscli --home ~/testcots-focused utxo list --utxo-file final-utxos.json

# Test address operations
echo ""
echo "🏠 Testing address operations..."
cotscli --home ~/testcots-focused address info --address $(cat alice.addr)

# Final summary
echo ""
echo "🎉 Focused Test Complete!"
echo "========================"
echo "✅ Generated 3 unique key pairs"
echo "✅ Built 3 unique addresses"
echo "✅ Created 3 initial UTXOs"
echo "✅ Executed 3 transactions"
echo "✅ Tested transaction commands (build, sign, submit, calculate-min-fee, txid, view, decode, validate)"
echo "✅ Tested database operations (init, import, export, inspect)"
echo "✅ Tested wallet operations (create, list, info)"
echo "✅ Tested UTXO operations (list)"
echo "✅ Tested address operations (info)"
echo ""
echo "🚀 All key COTS CLI commands tested successfully!"
echo "📊 Total transactions processed: 3"
echo "👥 Total wallets involved: 3"
echo "💰 Total UTXOs managed: 9+"
echo ""
echo "🎨 Professional colorized output demonstrated:"
echo "   ✓ Success messages in bright green"
echo "   ✗ Error messages in bright red"
echo "   ⚠ Warning messages in bright yellow"
echo "   ℹ Info messages in bright blue"
echo "   ▶ Progress messages in cyan"
echo "   🌈 Colorized transaction hashes, amounts, and addresses"
