# COTS CLI — Professional Command Examples (Cardano CLI Compatible)

## 🚀 Quick Start with Professional Colorized Output

### 1. Initialize Workspace with Custom Home Directory

```bash
cotscli --home ~/my-cots-workspace init --path ~/my-cots-workspace --name "My Project" --network Preprod
# ✅ SUCCESS: Initialized COTS workspace at: ~/my-cots-workspace/preprod
# ℹ Created config: ~/my-cots-workspace/preprod/config.json
```

### 2. Generate Keys with Professional Visual Feedback

```bash
# Key generation with enhanced visual feedback
cotscli --home ~/my-cots-workspace address key-gen \
  --verification-key-file alice.vkey \
  --signing-key-file alice.skey
# ▶ Generating payment key pair...
# ℹ Verification key: ~/my-cots-workspace/keys/alice.vkey
# ℹ Signing key: ~/my-cots-workspace/keys/alice.skey
# ✅ Payment key pair generated successfully!
# ℹ Files saved in: ~/my-cots-workspace/keys
```

### 3. Build Address with Professional Formatting

```bash
# Address construction with color and formatting
cotscli --home ~/my-cots-workspace address build \
  --payment-verification-key-file alice.vkey \
  --out-file alice.addr \
  --network Preprod \
  --initial-amount 10000000
# ▶ Building Cardano address...
# ✅ Address built: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# ℹ File saved at: alice.addr
```

### 4. Initialize SQLite Database with Professional Structure

```bash
cotscli --home ~/my-cots-workspace database init --db-file project.db
# ▶ Initializing SQLite database and COTS home structure...
# ℹ Database file: project.db
# ℹ Created directory: ~/my-cots-workspace/keys
# ℹ Created directory: ~/my-cots-workspace/addresses
# ℹ Created directory: ~/my-cots-workspace/utxos
# ℹ Created directory: ~/my-cots-workspace/transactions
# ℹ Created directory: ~/my-cots-workspace/protocol
# ℹ Created directory: ~/my-cots-workspace/scripts
# ✅ Database and home structure initialized successfully!
```

---

## 🔍 Professional UTXO Management

### Query UTXO (Cardano CLI Compatible)

```bash
# UTXO query with professional formatting and colors
cotscli --home ~/my-cots-workspace query utxo \
  --address $(cat alice.addr) \
  --testnet-magic 1 \
  --db-file project.db
# ▶ Querying UTXOs for address: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# ℹ Testnet magic: 1
# ℹ Using default socket path
# 
# ═══ UTXO Query Results ═══
# 
# TxHash │ TxIx │ Amount
# ─────────────────────────────────────────────────────────────────────────
# genesis_alice_0000000000000000000000000000000000000000000000000...    0    10000000 lovelace
# ℹ Found 1 UTXOs
```

### Generate Initial UTXOs Automatically

```bash
# Generate initial UTXOs JSON file automatically
cotscli --home ~/my-cots-workspace database generate-utxo \
  --addresses "$(cat alice.addr),$(cat bob.addr)" \
  --amounts "10000000,5000000" \
  --out-file initial-utxos.json \
  --prefix "genesis"
# ▶ Generating initial UTXOs JSON file...
# ℹ Addresses: addr_test1..., addr_test1...
# ℹ Amounts: 10000000, 5000000
# ℹ Output file: initial-utxos.json
# ✅ Generated 2 initial UTXOs successfully!
# ℹ File saved at: initial-utxos.json
```

### Import UTXOs with Professional Feedback

```bash
# Import with professional feedback
cotscli --home ~/my-cots-workspace database import-utxo \
  --utxo-file initial-utxos.json \
  --db-file project.db
# ▶ Importing UTXOs from JSON file...
# ℹ Database file: ~/my-cots-workspace/project.db
# ℹ UTXO file: initial-utxos.json
# ✅ SUCCESS: Imported 2 UTXOs successfully!
```

---

## 💰 Professional Transaction Workflow (Cardano CLI Compatible)

### NEW: Complete Transaction Send Command

The new `transaction send` command orchestrates the full Cardano CLI workflow automatically:

```bash
# Complete transaction workflow: build-raw -> calculate-min-fee -> sign -> submit -> txid
cotscli --home ~/my-cots-workspace transaction send \
  --from-address "$(cat alice.addr)" \
  --to-address "$(cat bob.addr)" \
  --amount 500000 \
  --db-file project.db \
  --out-file tx1 \
  --signing-key-file alice.skey \
  --testnet-magic 1
```

**Output:**
```
▶ Starting transaction workflow...
ℹ From: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
ℹ To: addr_test1a8b2016c6583c4fbe6379fdc1facfb6e2acae11528ebf9475616543762d3259b
ℹ Amount: 500000 lovelace
▶ Step 1: Building raw transaction...
✅ Raw transaction built successfully!
▶ Step 2: Calculating minimum fee...
ℹ Calculated fee: 200000 lovelace
▶ Step 3: Signing transaction...
✅ Transaction signed successfully!
▶ Step 4: Submitting transaction...
✅ Transaction successfully submitted.
▶ Step 5: Getting transaction ID...
✅ Transaction ID: e4fe21e4cd39b8043815fd989e61bdf7f337cc9e1c8aa816af80000000000000
▶ Updating UTXOs in database...
✅ UTXOs updated in database!

═══ Transaction Complete ═══

✅ Amount sent: 500000 lovelace
✅ Fee paid: 200000 lovelace
✅ Change returned: 13600000 lovelace
✅ Transaction ID: e4fe21e4cd39b8043815fd989e61bdf7f337cc9e1c8aa816af80000000000000
✅ Files created: tx1.raw, tx1.signed
```

### Get Transaction ID

```bash
# Get transaction ID from signed transaction
cotscli --home ~/my-cots-workspace transaction txid --tx-file tx1.signed
# ▶ Calculating transaction ID...
# ℹ Transaction file: tx1.signed
# ℹ Transaction loaded from file
# ✅ Transaction ID calculated!
# 
# ▸ Transaction ID
# 
# ℹ Transaction ID: e4fe21e4cd39b8043815fd989e61bdf7f337cc9e1c8aa816af80000000000000
```

### Individual Transaction Commands (Cardano CLI Compatible)

For advanced users who want to control each step:

```bash
# Step 1: Build raw transaction
cotscli --home ~/my-cots-workspace transaction build-raw \
  --babbage-era \
  --tx-in e4fe21e4cd39b8043815fd989e61bdf7f337cc9e1c8aa816af80000000000000#0 \
  --tx-out $(cat bob.addr)+500000 \
  --tx-out $(cat alice.addr)+13600000 \
  --fee 200000 \
  --out-file tx.raw

# Step 2: Calculate minimum fee
cotscli --home ~/my-cots-workspace transaction calculate-min-fee \
  --tx-body-file tx.raw \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --testnet-magic 1 \
  --protocol-params-file protocol.json

# Step 3: Sign transaction
cotscli --home ~/my-cots-workspace transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file alice.skey \
  --testnet-magic 1 \
  --out-file tx.signed

# Step 4: Submit transaction
cotscli --home ~/my-cots-workspace transaction submit \
  --tx-file tx.signed \
  --testnet-magic 1
# ✅ Transaction successfully submitted.

# Step 5: Get transaction ID
cotscli --home ~/my-cots-workspace transaction txid --tx-file tx.signed
```

---

## 👛 Professional Wallet Management

### Create Wallets with Professional Feedback

```bash
# Create wallet with professional output
cotscli --home ~/my-cots-workspace wallet create \
  --name "Alice" \
  --address "$(cat alice.addr)" \
  --db-file project.db
# ▶ Creating new wallet...
# ℹ Name: Alice
# ℹ Address: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# ℹ Database file: project.db
# ✅ SUCCESS: Wallet created successfully!
```

### List All Wallets

```bash
cotscli --home ~/my-cots-workspace wallet list --db-file project.db
# ▶ Listing all wallets...
# 
# ═══ Wallet List ═══
# 
# Name  │ Address                                    │ Created
# ─────────────────────────────────────────────────────────────────────────
# Alice │ addr_test1782de20fb78e40885fb198dfb33... │ 2024-01-15 10:30:00
# Bob   │ addr_test1a8b2016c6583c4fbe6379fdc1fa... │ 2024-01-15 10:31:00
# ℹ Found 2 wallets
```

### Get Wallet Information

```bash
cotscli --home ~/my-cots-workspace wallet info --name "Alice" --db-file project.db
# ▶ Getting wallet information...
# 
# ═══ Wallet Information ═══
# 
# Name:    Alice
# Address: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# Created: 2024-01-15 10:30:00
# 
# ▸ UTXOs
# ℹ Found 2 UTXOs
# TxHash: e4fe21e4cd39b8043815fd989e61bdf7f337cc9e1c8aa816af80000000000000
# Amount: 13600000 lovelace
```

---

## 🎯 Complete Professional Workflow Example

### Full Transaction Lifecycle

```bash
# 1. Initialize workspace
cotscli --home ~/workflow init --path ~/workflow --name "Transaction Demo" --network Preprod

# 2. Initialize database
cotscli --home ~/workflow database init --db-file demo.db

# 3. Generate keys for Alice and Bob
cotscli --home ~/workflow address key-gen --verification-key-file alice.vkey --signing-key-file alice.skey
cotscli --home ~/workflow address key-gen --verification-key-file bob.vkey --signing-key-file bob.skey

# 4. Build addresses
cotscli --home ~/workflow address build --payment-verification-key-file alice.vkey --out-file alice.addr --network Preprod --initial-amount 10000000
cotscli --home ~/workflow address build --payment-verification-key-file bob.vkey --out-file bob.addr --network Preprod

# 5. Move address files
mv alice.addr bob.addr ~/workflow/addresses/

# 6. Generate initial UTXOs
cotscli --home ~/workflow database generate-utxo \
  --addresses "$(cat ~/workflow/addresses/alice.addr)" \
  --amounts "10000000" \
  --out-file initial-utxos.json

# 7. Import UTXOs
cotscli --home ~/workflow database import-utxo --utxo-file initial-utxos.json --db-file demo.db

# 8. Create wallets
cotscli --home ~/workflow wallet create --name "Alice" --address "$(cat ~/workflow/addresses/alice.addr)" --db-file demo.db
cotscli --home ~/workflow wallet create --name "Bob" --address "$(cat ~/workflow/addresses/bob.addr)" --db-file demo.db

# 9. Check initial UTXOs
cotscli --home ~/workflow query utxo --address "$(cat ~/workflow/addresses/alice.addr)" --testnet-magic 1 --db-file demo.db

# 10. Send transaction (NEW WORKFLOW)
cotscli --home ~/workflow transaction send \
  --from-address "$(cat ~/workflow/addresses/alice.addr)" \
  --to-address "$(cat ~/workflow/addresses/bob.addr)" \
  --amount 2000000 \
  --db-file demo.db \
  --out-file demo-tx \
  --signing-key-file alice.skey \
  --testnet-magic 1

# 11. Get transaction ID
cotscli --home ~/workflow transaction txid --tx-file demo-tx.signed

# 12. Verify final UTXOs
cotscli --home ~/workflow query utxo --address "$(cat ~/workflow/addresses/alice.addr)" --testnet-magic 1 --db-file demo.db
cotscli --home ~/workflow query utxo --address "$(cat ~/workflow/addresses/bob.addr)" --testnet-magic 1 --db-file demo.db

# 13. List wallets
cotscli --home ~/workflow wallet list --db-file demo.db
```

---

## 🎨 Professional Output Features

### Color Scheme
- **✅ Success**: Bold green with checkmarks
- **▶ Progress**: Bold cyan with arrows  
- **ℹ Info**: Bright blue
- **⚠ Warning**: Bold yellow
- **❌ Error**: Bold red

### Professional Formatting
- **Section Headers**: Bold white with decorative borders
- **Table Headers**: Bold white with separators
- **Data Highlighting**: Colorized transaction hashes, amounts, addresses
- **Status Indicators**: Professional status messages

### Cardano CLI Compatibility
- **Exact Command Structure**: Matches `cardano-cli` syntax
- **Parameter Names**: Uses same parameter names as Cardano CLI
- **Output Format**: Professional table formatting
- **Transaction Workflow**: Complete `build-raw` → `calculate-min-fee` → `sign` → `submit` → `txid` process

---

## 📁 File Structure

```
~/workspace/
├── keys/           # Key files (.vkey, .skey)
├── addresses/      # Address files (.addr)
├── utxos/          # UTXO JSON files
├── transactions/   # Transaction files (.raw, .signed)
├── protocol/       # Protocol parameter files
└── scripts/        # Script files
```

---

## 🔧 Advanced Features

### Database Management
```bash
# Create database snapshot
cotscli --home ~/workspace database snapshot --db-file project.db --out-file snapshot.json

# Load database snapshot
cotscli --home ~/workflow database load-snapshot --db-file project.db --snapshot-file snapshot.json

# Inspect database
cotscli --home ~/workspace database inspect --db-file project.db
```

### Protocol Parameters
```bash
# Update protocol parameters
cotscli --home ~/workspace protocol update --file protocol.json --db-file project.db

# Fetch protocol parameters
cotscli --home ~/workspace protocol fetch --url https://api.koios.rest/api/v0/epoch_params --out-file protocol.json --db-file project.db
```

### Transaction Validation and Simulation
```bash
# Validate transaction
cotscli --home ~/workspace transaction validate --tx-file tx.signed --db-file project.db

# Simulate transaction
cotscli --home ~/workspace transaction simulate --tx-file tx.signed --db-file project.db

# View transaction details
cotscli --home ~/workspace transaction view --tx-file tx.signed --verbose
```

---

## 🚀 Best Practices

1. **Always use `--home` parameter** for consistent workspace management
2. **Use `transaction send`** for complete transaction workflows
3. **Verify UTXOs** after each transaction with `query utxo`
4. **Create wallets** for better organization and tracking
5. **Use professional colorized output** for better user experience
6. **Follow Cardano CLI patterns** for familiarity and compatibility

This professional implementation provides a complete, Cardano CLI-compatible transaction simulation environment with beautiful, colorized output and robust UTXO management! 🎉