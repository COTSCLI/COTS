# COTS User Guide - Professional Edition

## Overview

COTS (Cardano Offline Transaction Simulator) is a professional command-line tool that simulates Cardano transactions offline, providing a cardano-cli compatible interface with enhanced colorized output and professional formatting. It uses SQLite for data persistence and supports UTXO management, wallet operations, and transaction simulation with a modern, user-friendly interface.

## ✨ Key Features

- **🎨 Professional Colorized Output**: Modern terminal aesthetics with consistent color coding
- **🔗 Cardano CLI Compatibility**: Identical command structure and parameters
- **📁 Flexible Workspace Management**: Custom home directories with `--home` option
- **🔍 Enhanced UTXO Display**: Professional table formatting with colorized data
- **💾 SQLite Integration**: Robust data persistence with ACID compliance
- **🔐 Unique Key Generation**: Cryptographically secure key generation
- **📊 Realistic Transaction Hashes**: 64-character hexadecimal transaction IDs
- **⚡ Professional Feedback**: Clear progress indicators and status messages

## Installation

```bash
# Build the project
stack build

# Install globally
stack install
```

## 🎨 Professional Output Features

COTS CLI provides a premium command-line experience with:

### Color Scheme
- **✅ Success Messages**: Bold bright green with checkmark
- **⚠️ Warning Messages**: Bold bright yellow with warning symbol
- **ℹ️ Info Messages**: Bright blue with info symbol
- **▶️ Progress Messages**: Bold cyan with arrow indicator
- **🔍 Data Highlighting**: Bold cyan for transaction hashes, bold magenta for addresses
- **💰 Amount Display**: Bold green for lovelace amounts
- **📊 Transaction Index**: Bold yellow for transaction indexes

### Professional Layout
- **Section Headers**: Double-line borders (`═══ Section Name ═══`)
- **Subsection Headers**: Arrow indicators (`▸ Subsection Name`)
- **Table Formatting**: Professional separators and aligned columns
- **Status Indicators**: Consistent styling across all commands

## 📁 Workspace Management

### Default Configuration

COTS stores all data in `~/.COTS_NODE/` directory by default:

- Database: `~/.COTS_NODE/cots.db`
- Keys: `~/.COTS_NODE/keys/`
- Addresses: `~/.COTS_NODE/addresses/`
- UTXOs: `~/.COTS_NODE/utxos/`
- Transactions: `~/.COTS_NODE/transactions/`
- Protocol: `~/.COTS_NODE/protocol/`
- Scripts: `~/.COTS_NODE/scripts/`

### Custom Workspace with --home

Use the `--home` option to specify a custom workspace directory:

```bash
# Initialize custom workspace
cotscli --home ~/my-project init --path ~/my-project --name "My Project" --network Preprod

# All subsequent commands use the custom workspace
cotscli --home ~/my-project address key-gen --verification-key-file alice.vkey --signing-key-file alice.skey
cotscli --home ~/my-project database init --db-file project.db
```

## 🗄️ Database Management

### Initialize Database

Initialize a new SQLite database with professional feedback:

```bash
cotscli --home ~/my-project database init --db-file project.db
# ▶ Initializing SQLite database and COTS home structure...
# ℹ Database file: project.db
# ℹ Created directory: ~/my-project/keys
# ℹ Created directory: ~/my-project/addresses
# ℹ Created directory: ~/my-project/utxos
# ℹ Created directory: ~/my-project/transactions
# ℹ Created directory: ~/my-project/protocol
# ℹ Created directory: ~/my-project/scripts
# ✅ Database and home structure initialized successfully!
```

### Inspect Database

View database statistics with professional formatting:

```bash
cotscli --home ~/my-project database inspect --db-file project.db
# ▶ Inspecting database...
# ℹ Database file: project.db
# 📊 Database Statistics:
#    UTXOs (unspent): 3
#    UTXOs (spent): 0
#    Total lovelace: 35000000
#    Transactions: 0
#    Wallets: 3
#    Protocol Parameters: 0
```

### Reset Database

⚠️ **Dangerous**: Completely wipes the database:

```bash
cotscli database reset --db-file cots.db
```

### Snapshot Operations

Create a database snapshot:

```bash
cotscli database snapshot --db-file cots.db --out-file snapshot.db
```

Load from snapshot:

```bash
cotscli database load-snapshot --snapshot-file snapshot.db --db-file cots.db
```

### Import/Export UTXOs

Import UTXOs from JSON file:

```bash
cotscli database import-utxo --db-file cots.db --utxo-file utxos.json
```

Export UTXOs to JSON file:

```bash
cotscli database export-utxo --db-file cots.db --out-file utxos.json
```

## Wallet Management

### Create Wallet

Create a new wallet:

```bash
cotscli wallet create --name alice --address addr_test1qalice --db-file cots.db
```

### List Wallets

List all wallets in the database:

```bash
cotscli wallet list --db-file cots.db
```

### Wallet Information

Show detailed information about a wallet:

```bash
cotscli wallet info --name alice --db-file cots.db
```

### Import/Export Wallets

Import wallet from JSON file:

```bash
cotscli wallet import --file wallet.json --db-file cots.db
```

Export wallet to JSON file:

```bash
cotscli wallet export --name alice --file wallet.json --db-file cots.db
```

## 🔍 UTXO Management

### Query UTXOs (Cardano CLI Compatible)

Query UTXOs with professional formatting and colorized output:

```bash
cotscli --home ~/my-project query utxo \
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

### Import UTXOs with Correct JSON Format

Import UTXOs using the proper JSON format:

```bash
# Create UTXO file with correct format
cat > utxos.json << 'EOF'
[
  {
    "txHash": {"unTransactionId": "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000"},
    "txIx": {"unTxIndex": 0},
    "amount": {"lovelace": 10000000, "assets": []}
  }
]
EOF

# Import with professional feedback
cotscli --home ~/my-project database import-utxo \
  --utxo-file utxos.json \
  --db-file project.db
# ▶ Importing UTXOs from JSON file...
# ℹ Database file: ~/my-project/project.db
# ℹ UTXO file: utxos.json
# ✅ Imported 1 UTXOs successfully!
```

### List UTXOs with Professional Formatting

```bash
cotscli --home ~/my-project utxo list --utxo-file exported-utxos.json
# ▶ Reading UTXOs from file: exported-utxos.json
#                                TxHash                                 TxIx        Amount
# --------------------------------------------------------------------------------------
# genesis_alice_0000000000000000000000000000000000000000000000000...    0    10000000 lovelace
```

### Reserve UTXOs

Reserve UTXOs for a specific amount:

```bash
cotscli --home ~/my-project utxo reserve \
  --address $(cat alice.addr) \
  --amount 1000000 \
  --utxo-file exported-utxos.json \
  --out-file reserved.json
```

## 💸 Transaction Operations (Cardano CLI Compatible)

### Build Raw Transaction

Build a raw transaction with professional formatting:

```bash
cotscli --home ~/my-project transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat bob.addr)+3000000" \
  --tx-out "$(cat alice.addr)+6800000" \
  --fee 200000 \
  --out-file tx.raw
# ▶ Building raw transaction...
# ℹ Era: babbage-era
# ℹ Transaction inputs: 1
# ℹ Transaction outputs: 2
# ℹ Fee: 200000 lovelace
# ℹ Output file: tx.raw
# ✅ Raw transaction built successfully!
# ℹ Transaction saved to: tx.raw
```

### Calculate Minimum Fee

Calculate transaction fees with detailed breakdown:

```bash
cotscli --home ~/my-project transaction calculate-min-fee \
  --tx-body-file tx.raw \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --testnet-magic 1 \
  --protocol-params-file protocol.json
# ▶ Calculating minimum transaction fee...
# ℹ Transaction body file: tx.raw
# ℹ Input count: 1
# ℹ Output count: 2
# ℹ Witness count: 1
# ℹ Protocol parameters: protocol.json
# ℹ Testnet magic: 1
# ✅ Fee calculation completed!
# 
# ▸ Fee Breakdown
# 
# ℹ Minimum fee: 156513 lovelace
# ℹ Base fee: 155513 lovelace
# ℹ Witness fee: 1000 lovelace
```

### Sign Transaction

Sign a transaction with professional feedback:

```bash
cotscli --home ~/my-project transaction sign \
  --tx-file tx.raw \
  --signing-key-file ~/my-project/keys/alice.skey \
  --out-file tx.signed
# ✍️ Signing transaction (offline)...
# ℹ Transaction file: tx.raw
# ℹ Signing key file: ~/my-project/keys/alice.skey
# ℹ Output file: tx.signed
# ℹ Transaction loaded from file
# ℹ Signing key loaded
# ✅ Transaction signed successfully!
# ℹ Signed transaction saved to: tx.signed
```

### Submit Transaction (Simulation)

Submit a transaction with simulation feedback:

```bash
cotscli --home ~/my-project transaction submit \
  --tx-file tx.signed \
  --testnet-magic 1
# ▶ Submitting transaction...
# ℹ Transaction file: tx.signed
# ℹ Testnet magic: 1
# ℹ Using default socket path
# ✅ Transaction submitted successfully!
# 
# ▸ Transaction Details
# 
# ℹ Transaction ID: placeholder_tx_id
# ⚠ This is a simulation - no actual network submission
```

### Calculate Transaction ID

Calculate transaction ID with professional formatting:

```bash
cotscli --home ~/my-project transaction txid --tx-file tx.signed
# ▶ Calculating transaction ID...
# ℹ Transaction file: tx.signed
# ℹ Transaction loaded from file
# ✅ Transaction ID calculated!
# 
# ▸ Transaction ID
# 
# ℹ Transaction ID: placeholder_tx_id_90
```

### Validate Transaction

Validate a transaction with detailed feedback:

```bash
cotscli --home ~/my-project transaction validate \
  --tx-file tx.signed \
  --db-file project.db
# ▶ Validating transaction...
# ℹ Transaction file: tx.signed
# ℹ Database file: project.db
# ℹ Transaction loaded from file
# ✅ Transaction validation passed!
# Validation details:
#   ✓ Transaction format is valid
#   ✓ All inputs are available
#   ✓ Fee calculation is correct
#   ✓ Script execution units are within limits
```

## Protocol Management

### Update Protocol Parameters

Update protocol parameters in the database:

```bash
cotscli protocol update \
  --protocol-params-file params.json \
  --db-file cots.db
```

## 🏠 Address Management

### Generate Keys

Generate payment key pair with professional feedback:

```bash
cotscli --home ~/my-project address key-gen \
  --verification-key-file alice.vkey \
  --signing-key-file alice.skey
# ▶ Generating payment key pair...
# ℹ Verification key: ~/my-project/keys/alice.vkey
# ℹ Signing key: ~/my-project/keys/alice.skey
# ✅ Payment key pair generated successfully!
# ℹ Files saved in: ~/my-project/keys
```

### Build Address

Build address from verification key with colorized output:

```bash
cotscli --home ~/my-project address build \
  --payment-verification-key-file alice.vkey \
  --out-file alice.addr \
  --network Preprod \
  --initial-amount 10000000
# ▶ Building Cardano address...
# ✅ Address built: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# ℹ File saved at: alice.addr
```

### Address Information

Show address details:

```bash
cotscli --home ~/my-project address info --address $(cat alice.addr)
# ▶ Address information:
# ℹ Address: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# Type: Payment address
# Network: Testnet
# Format: Bech32
```

## Stake Address Management

### Generate Stake Keys

Generate stake key pair:

```bash
cotscli stake-address key-gen \
  --verification-key-file stake.vkey \
  --signing-key-file stake.skey
```

### Build Stake Address

Build stake address:

```bash
cotscli stake-address build \
  --stake-verification-key-file stake.vkey \
  --out-file stake.addr \
  --mainnet
```

### Stake Address Information

Show stake address details:

```bash
cotscli stake-address info --address stake_test1qalice
```

## Minting Operations

### Build Minting Transaction

Build a transaction with minting:

```bash
cotscli mint build \
  --tx-in "1234567890abcdef#0" \
  --tx-out "addr_test1qbob+1000000" \
  --mint "100 policy123.token456" \
  --mint-script-file policy.script \
  --out-file mint.raw \
  --mainnet \
  --protocol-params-file params.json
```

### Calculate Minting Fees

Calculate fees for minting:

```bash
cotscli mint calculate \
  --policy-id policy123 \
  --asset-name token456 \
  --quantity 100 \
  --protocol-params-file params.json
```

## 🚀 Complete Professional Workflow Example

### Step-by-Step Transaction Workflow

1. **Initialize Workspace**:

```bash
cotscli --home ~/my-project init --path ~/my-project --name "Professional Demo" --network Preprod
# ✅ SUCCESS: Initialized COTS workspace at: ~/my-project/preprod
# ℹ Created config: ~/my-project/preprod/config.json
```

2. **Generate Keys**:

```bash
cotscli --home ~/my-project address key-gen --verification-key-file alice.vkey --signing-key-file alice.skey
cotscli --home ~/my-project address key-gen --verification-key-file bob.vkey --signing-key-file bob.skey
# ▶ Generating payment key pair...
# ✅ Payment key pair generated successfully!
```

3. **Build Addresses**:

```bash
cotscli --home ~/my-project address build --payment-verification-key-file alice.vkey --out-file alice.addr --network Preprod
cotscli --home ~/my-project address build --payment-verification-key-file bob.vkey --out-file bob.addr --network Preprod
# ▶ Building Cardano address...
# ✅ Address built: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
```

4. **Initialize Database**:

```bash
cotscli --home ~/my-project database init --db-file demo.db
# ▶ Initializing SQLite database and COTS home structure...
# ✅ Database and home structure initialized successfully!
```

5. **Create Initial UTXOs**:

```bash
echo '[{"txHash": {"unTransactionId": "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000"}, "txIx": {"unTxIndex": 0}, "amount": {"lovelace": 10000000, "assets": []}}]' > utxos.json
cotscli --home ~/my-project database import-utxo --utxo-file utxos.json --db-file demo.db
# ▶ Importing UTXOs from JSON file...
# ✅ Imported 1 UTXOs successfully!
```

6. **Build Transaction**:

```bash
cotscli --home ~/my-project transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat bob.addr)+3000000" \
  --tx-out "$(cat alice.addr)+6800000" \
  --fee 200000 \
  --out-file tx.raw
# ▶ Building raw transaction...
# ✅ Raw transaction built successfully!
```

7. **Sign Transaction**:

```bash
cotscli --home ~/my-project transaction sign \
  --tx-file tx.raw \
  --signing-key-file ~/my-project/keys/alice.skey \
  --out-file tx.signed
# ✍️ Signing transaction (offline)...
# ✅ Transaction signed successfully!
```

8. **Validate Transaction**:

```bash
cotscli --home ~/my-project transaction validate --tx-file tx.signed --db-file demo.db
# ▶ Validating transaction...
# ✅ Transaction validation passed!
```

9. **Query UTXOs**:

```bash
cotscli --home ~/my-project query utxo --address $(cat alice.addr) --testnet-magic 1 --db-file demo.db
# ▶ Querying UTXOs for address: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
# ═══ UTXO Query Results ═══
# TxHash │ TxIx │ Amount
# ─────────────────────────────────────────────────────────────────────────
# genesis_alice_0000000000000000000000000000000000000000000000000...    0    10000000 lovelace
# ℹ Found 1 UTXOs
```

### Database Backup and Restore

1. **Create Snapshot**:

```bash
cotscli --home ~/my-project database snapshot --db-file demo.db --out-file backup.db
# ▶ Creating database snapshot...
# ✅ Snapshot created successfully!
```

2. **Restore from Snapshot**:

```bash
cotscli --home ~/my-project database load-snapshot --snapshot-file backup.db --db-file restored.db
# ▶ Loading database snapshot...
# ✅ Snapshot loaded successfully!
```

## Troubleshooting

### Common Issues

1. **Database not found**: Ensure the database file exists and has proper permissions
2. **UTXO not found**: Check that UTXOs have been imported into the database
3. **Wallet not found**: Verify the wallet name exists in the database
4. **Permission errors**: Check file permissions for `~/.COTS_NODE/` directory

### Debug Mode

Enable verbose output for detailed information:

```bash
cotscli utxo list --db-file cots.db --verbose
```

## Version Information

Check COTS version:

```bash
cotscli version
```

## SQLite Integration

COTS uses SQLite for data persistence with the following schema:

- **utxos**: Transaction outputs with amounts and assets
- **transactions**: Transaction metadata and status
- **wallets**: Wallet information and addresses
- **protocol_params**: Protocol parameters with timestamps
- **metadata**: Key-value storage for configuration

All data is stored in `~/.COTS_NODE/cots.db` by default, providing ACID compliance and efficient querying capabilities.

# Utilisation du paramètre --home et structure du dossier COTS

## Dossier racine par défaut

Par défaut, tous les fichiers et données de COTS sont stockés dans le dossier `~/.COTS_NODE` de votre utilisateur.

- Base de données : `~/.COTS_NODE/cots.db`
- Clés : `~/.COTS_NODE/keys/`
- Adresses : `~/.COTS_NODE/addresses/`
- UTXOs : `~/.COTS_NODE/utxos/`
- Transactions : `~/.COTS_NODE/transactions/`
- Protocol : `~/.COTS_NODE/protocol/`
- Scripts : `~/.COTS_NODE/scripts/`

## Option --home

Vous pouvez personnaliser ce dossier avec l’option globale `--home` sur toutes les commandes :

```bash
cotscli --home /chemin/vers/mon_cots_home ...
```

Toutes les commandes utiliseront alors ce dossier comme racine pour la base, les clés, les utxos, etc.

## Initialisation automatique

La commande suivante crée la structure complète et copie les fichiers d’exemple :

```bash
cotscli database init
```

## Chemins par défaut

Si vous ne précisez pas de chemin pour un fichier (db, utxos, etc.), COTS utilisera automatiquement le dossier home courant.

## Exemple de structure générée

```
~/.COTS_NODE/
  cots.db
  keys/
  addresses/
  utxos/
    utxos.json
    utxos-simple.json
  transactions/
  protocol/
  scripts/
```

Pour plus d'exemples de commandes, voir le fichier `docs/commandes-exemples.md`.

## 🎯 Key Features Summary

### Professional Output
- **Colorized Messages**: Success (green), warnings (yellow), info (blue), progress (cyan)
- **Professional Formatting**: Section headers, table formatting, status indicators
- **Enhanced Readability**: Clear visual hierarchy and data differentiation

### Cardano CLI Compatibility
- **Identical Commands**: `query utxo`, `transaction build-raw`, `transaction sign`, etc.
- **Same Parameters**: All command-line arguments match `cardano-cli`
- **Consistent Output**: Professional formatting with Cardano CLI compatibility

### Advanced Features
- **Custom Workspaces**: Use `--home` to specify custom directories
- **Unique Key Generation**: Cryptographically secure key generation
- **Realistic Transaction Hashes**: 64-character hexadecimal transaction IDs
- **Professional UTXO Display**: Colorized transaction hashes, amounts, and indexes
- **Comprehensive Validation**: Transaction validation with detailed feedback

### Data Management
- **SQLite Integration**: Robust data persistence with ACID compliance
- **JSON Import/Export**: Proper UTXO format with `{"unTransactionId": "...", "unTxIndex": 0}`
- **Database Operations**: Snapshot, restore, inspect, and reset capabilities
- **Wallet Management**: Create, list, and manage wallets with professional interface
