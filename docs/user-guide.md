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
- **🔄 Complete Transaction Workflow**: Automated `build-raw` → `calculate-min-fee` → `sign` → `submit` → `txid` process

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

COTS creates a structured workspace in your home directory:

```
~/.cotscli/
├── preprod/          # Preprod network configuration
│   ├── config.json   # Network-specific configuration
│   ├── keys/         # Key files (.vkey, .skey)
│   ├── addresses/    # Address files (.addr)
│   ├── utxos/        # UTXO JSON files
│   ├── transactions/ # Transaction files (.raw, .signed)
│   ├── protocol/     # Protocol parameter files
│   └── scripts/      # Script files
├── mainnet/          # Mainnet network configuration
├── testnet/          # Testnet network configuration
└── preview/          # Preview network configuration
```

### Custom Workspace with `--home`

Use the `--home` parameter to specify custom workspace locations:

```bash
# Initialize custom workspace
cotscli --home ~/my-project init --path ~/my-project --name "My Project" --network Preprod

# All subsequent commands use the custom workspace
cotscli --home ~/my-project database init --db-file project.db
cotscli --home ~/my-project address key-gen --verification-key-file alice.vkey --signing-key-file alice.skey
```

## 🚀 Quick Start Guide

### 1. Initialize Your Workspace

```bash
# Initialize with custom home directory
cotscli --home ~/my-cots init --path ~/my-cots --name "My COTS Project" --network Preprod
```

### 2. Set Up Database

```bash
# Initialize SQLite database
cotscli --home ~/my-cots database init --db-file myproject.db
```

### 3. Generate Keys and Addresses

```bash
# Generate key pair
cotscli --home ~/my-cots address key-gen \
  --verification-key-file alice.vkey \
  --signing-key-file alice.skey

# Build address
cotscli --home ~/my-cots address build \
  --payment-verification-key-file alice.vkey \
  --out-file alice.addr \
  --network Preprod \
  --initial-amount 10000000
```

### 4. Generate and Import Initial UTXOs

```bash
# Generate initial UTXOs automatically
cotscli --home ~/my-cots database generate-utxo \
  --addresses "$(cat ~/my-cots/addresses/alice.addr)" \
  --amounts "10000000" \
  --out-file initial-utxos.json

# Import UTXOs
cotscli --home ~/my-cots database import-utxo \
  --utxo-file initial-utxos.json \
  --db-file myproject.db
```

### 5. Create Wallets

```bash
# Create wallet
cotscli --home ~/my-cots wallet create \
  --name "Alice" \
  --address "$(cat ~/my-cots/addresses/alice.addr)" \
  --db-file myproject.db
```

### 6. Send Transactions (NEW WORKFLOW)

```bash
# Complete transaction workflow
cotscli --home ~/my-cots transaction send \
  --from-address "$(cat ~/my-cots/addresses/alice.addr)" \
  --to-address "$(cat ~/my-cots/addresses/bob.addr)" \
  --amount 500000 \
  --db-file myproject.db \
  --out-file tx1 \
  --signing-key-file alice.skey \
  --testnet-magic 1
```

## 💰 Transaction Management

### NEW: Complete Transaction Workflow

The `transaction send` command orchestrates the full Cardano CLI workflow automatically:

1. **Build Raw Transaction** - Creates transaction body
2. **Calculate Minimum Fee** - Determines optimal fee
3. **Sign Transaction** - Signs with private key
4. **Submit Transaction** - Simulates submission
5. **Get Transaction ID** - Generates unique hash
6. **Update UTXOs** - Updates database with new UTXOs

```bash
cotscli --home ~/workspace transaction send \
  --from-address "$(cat alice.addr)" \
  --to-address "$(cat bob.addr)" \
  --amount 1000000 \
  --db-file project.db \
  --out-file my-tx \
  --signing-key-file alice.skey \
  --testnet-magic 1
```

**Professional Output:**
```
▶ Starting transaction workflow...
ℹ From: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
ℹ To: addr_test1a8b2016c6583c4fbe6379fdc1facfb6e2acae11528ebf9475616543762d3259b
ℹ Amount: 1000000 lovelace
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

✅ Amount sent: 1000000 lovelace
✅ Fee paid: 200000 lovelace
✅ Change returned: 8800000 lovelace
✅ Transaction ID: e4fe21e4cd39b8043815fd989e61bdf7f337cc9e1c8aa816af80000000000000
✅ Files created: my-tx.raw, my-tx.signed
```

### Individual Transaction Commands

For advanced users who need granular control:

```bash
# Build raw transaction
cotscli --home ~/workspace transaction build-raw \
  --babbage-era \
  --tx-in txhash#0 \
  --tx-out addr+amount \
  --fee 200000 \
  --out-file tx.raw

# Calculate minimum fee
cotscli --home ~/workspace transaction calculate-min-fee \
  --tx-body-file tx.raw \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --testnet-magic 1 \
  --protocol-params-file protocol.json

# Sign transaction
cotscli --home ~/workspace transaction sign \
  --tx-body-file tx.raw \
  --signing-key-file key.skey \
  --testnet-magic 1 \
  --out-file tx.signed

# Submit transaction
cotscli --home ~/workspace transaction submit \
  --tx-file tx.signed \
  --testnet-magic 1

# Get transaction ID
cotscli --home ~/workspace transaction txid --tx-file tx.signed
```

## 🔍 UTXO Management

### Query UTXOs (Cardano CLI Compatible)

```bash
# Query UTXOs for an address
cotscli --home ~/workspace query utxo \
  --address $(cat alice.addr) \
  --testnet-magic 1 \
  --db-file project.db
```

**Professional Output:**
```
▶ Querying UTXOs for address: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715
ℹ Testnet magic: 1
ℹ Using default socket path

═══ UTXO Query Results ═══

TxHash │ TxIx │ Amount
─────────────────────────────────────────────────────────────────────────
e4fe21e4cd39b8043815fd989e61bdf7f337cc9e1c8aa816af80000000000000    0    1000000 lovelace
fb9e3d7c245e72dfbbd1b9a8ba247c367aa12206469aab424360000000000000    1    8800000 lovelace
ℹ Found 2 UTXOs
```

### Generate Initial UTXOs

```bash
# Generate initial UTXOs automatically
cotscli --home ~/workspace database generate-utxo \
  --addresses "$(cat alice.addr),$(cat bob.addr)" \
  --amounts "10000000,5000000" \
  --out-file initial-utxos.json \
  --prefix "genesis"
```

### Import/Export UTXOs

```bash
# Import UTXOs from JSON file
cotscli --home ~/workspace database import-utxo \
  --utxo-file initial-utxos.json \
  --db-file project.db

# Export UTXOs to JSON file
cotscli --home ~/workspace database export-utxo \
  --out-file exported-utxos.json \
  --db-file project.db
```

## 👛 Wallet Management

### Create Wallets

```bash
# Create a new wallet
cotscli --home ~/workspace wallet create \
  --name "Alice" \
  --address "$(cat alice.addr)" \
  --db-file project.db
```

### List Wallets

```bash
# List all wallets
cotscli --home ~/workspace wallet list --db-file project.db
```

**Professional Output:**
```
▶ Listing all wallets...

═══ Wallet List ═══

Name  │ Address                                    │ Created
────────────────────────────────────────────────────────────────────────
Alice │ addr_test1782de20fb78e40885fb198dfb33... │ 2024-01-15 10:30:00
Bob   │ addr_test1a8b2016c6583c4fbe6379fdc1fa... │ 2024-01-15 10:31:00
ℹ Found 2 wallets
```

### Get Wallet Information

```bash
# Get detailed wallet information
cotscli --home ~/workspace wallet info --name "Alice" --db-file project.db
```

## 🔑 Address Management

### Generate Keys

```bash
# Generate payment key pair
cotscli --home ~/workspace address key-gen \
  --verification-key-file alice.vkey \
  --signing-key-file alice.skey
```

### Build Addresses

```bash
# Build payment address
cotscli --home ~/workspace address build \
  --payment-verification-key-file alice.vkey \
  --out-file alice.addr \
  --network Preprod \
  --initial-amount 10000000
```

### Generate Stake Keys (Optional)

```bash
# Generate stake key pair
cotscli --home ~/workspace stake-address key-gen \
  --verification-key-file stake.vkey \
  --signing-key-file stake.skey

# Build stake address
cotscli --home ~/workspace stake-address build \
  --stake-verification-key-file stake.vkey \
  --out-file stake.addr \
  --network Preprod
```

## 🗄️ Database Management

### Initialize Database

```bash
# Initialize SQLite database
cotscli --home ~/workspace database init --db-file project.db
```

### Database Snapshots

```bash
# Create database snapshot
cotscli --home ~/workspace database snapshot \
  --db-file project.db \
  --out-file snapshot.json

# Load database snapshot
cotscli --home ~/workspace database load-snapshot \
  --db-file project.db \
  --snapshot-file snapshot.json
```

### Database Inspection

```bash
# Inspect database and get statistics
cotscli --home ~/workspace database inspect --db-file project.db
```

## ⚙️ Protocol Parameters

### Update Protocol Parameters

```bash
# Update from local file
cotscli --home ~/workspace protocol update \
  --file protocol.json \
  --db-file project.db
```

### Fetch Protocol Parameters

```bash
# Fetch from Koios API
cotscli --home ~/workspace protocol fetch \
  --url https://api.koios.rest/api/v0/epoch_params \
  --out-file protocol.json \
  --db-file project.db
```

## 🎯 Advanced Features

### Transaction Validation

```bash
# Validate transaction
cotscli --home ~/workspace transaction validate \
  --tx-file tx.signed \
  --db-file project.db
```

### Transaction Simulation

```bash
# Simulate transaction
cotscli --home ~/workspace transaction simulate \
  --tx-file tx.signed \
  --db-file project.db
```

### Transaction Viewing

```bash
# View transaction details
cotscli --home ~/workspace transaction view \
  --tx-file tx.signed \
  --verbose
```

### Transaction Export

```bash
# Export transaction in different formats
cotscli --home ~/workspace transaction export \
  --tx-file tx.signed \
  --format CardanoCLI \
  --out-file exported-tx.json
```

## 🔧 Configuration

### Network Configuration

COTS supports multiple networks:

- **Mainnet**: Production Cardano network
- **Testnet**: Legacy testnet
- **Preview**: Preview testnet
- **Preprod**: Preproduction testnet

### Custom Configuration

Each network has its own configuration file:

```json
{
  "network": "Preprod",
  "protocolParameters": {
    "minFeeA": 44,
    "minFeeB": 155381,
    "maxTxSize": 16384,
    "maxValSize": 5000,
    "keyDeposit": 2000000,
    "poolDeposit": 500000000,
    "minPoolCost": 340000000,
    "coinsPerUtxoWord": 34482,
    "maxCollateralInputs": 3,
    "collateralPercentage": 150,
    "maxBlockExecutionUnits": {
      "memory": 50000000,
      "steps": 40000000000
    },
    "maxTxExecutionUnits": {
      "memory": 14000000,
      "steps": 10000000000
    },
    "maxValueSize": 5000,
    "collateralPercentage": 150,
    "maxCollateralInputs": 3
  },
  "wallets": []
}
```

## 🚀 Best Practices

### 1. Workspace Organization

- Always use the `--home` parameter for consistent workspace management
- Organize files in the appropriate subdirectories (keys/, addresses/, utxos/, etc.)
- Use descriptive names for your projects

### 2. Transaction Management

- Use `transaction send` for complete workflows
- Always verify UTXOs after transactions with `query utxo`
- Keep transaction files organized in the transactions/ directory

### 3. Database Management

- Create regular snapshots of your database
- Use descriptive database file names
- Monitor database size and performance

### 4. Security

- Keep private keys secure and never share them
- Use testnet for development and testing
- Regularly backup your workspace

### 5. Professional Output

- Take advantage of the colorized output for better readability
- Use verbose options when needed for detailed information
- Follow the professional formatting for consistent experience

## 🐛 Troubleshooting

### Common Issues

1. **"Config file not found"**
   - Ensure you've initialized the workspace with `cotscli init`
   - Check that the `--home` path is correct

2. **"UTXO file not found"**
   - Verify the file exists in the current directory or utxos/ subdirectory
   - Check file permissions

3. **"Insufficient funds"**
   - Verify UTXOs are properly imported
   - Check that the source address has sufficient balance

4. **"Database locked"**
   - Ensure no other processes are using the database
   - Check file permissions

### Getting Help

```bash
# Get help for any command
cotscli --help
cotscli transaction --help
cotscli database --help
cotscli wallet --help
```

## 📚 Additional Resources

- **Command Examples**: See `docs/commandes-exemples.md` for detailed examples
- **Professional Features**: See `docs/professional-features.md` for advanced features
- **SQLite Implementation**: See `docs/sqlite-implementation.md` for database details
- **CI/CD**: See `docs/ci-cd.md` for development workflow

## 🎉 Conclusion

COTS provides a professional, Cardano CLI-compatible environment for offline transaction simulation with beautiful colorized output, robust UTXO management, and complete transaction workflow automation. The new `transaction send` command makes it easy to perform complete transaction workflows while maintaining full compatibility with Cardano CLI patterns.

Enjoy building with COTS! 🚀