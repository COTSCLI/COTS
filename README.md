# Cardano Offline Transaction Simulator (COTS) - Professional Edition

[![CI/CD Pipeline](https://github.com/COTSCLI/COTS/actions/workflows/ci.yml/badge.svg)](https://github.com/COTSCLI/COTS/actions/workflows/ci.yml)
[![Code Coverage](https://github.com/COTSCLI/COTS/actions/workflows/coverage.yml/badge.svg)](https://github.com/COTSCLI/COTS/actions/workflows/coverage.yml)
[![codecov](https://codecov.io/gh/COTSCLI/COTS/branch/main/graph/badge.svg)](https://codecov.io/gh/COTSCLI/COTS)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Haskell](https://img.shields.io/badge/Haskell-9.4.8-blue.svg)](https://www.haskell.org)

COTS is a professional CLI tool for simulating Cardano transactions offline with enhanced colorized output, Cardano CLI compatibility, and modern terminal aesthetics. Compute fees, manage UTxOs in SQLite, and export transactions in Cardano-CLI or Koios formats with a premium user experience.

## ✨ Professional Features

- **🎨 Professional Colorized Output**: Modern terminal aesthetics with consistent color coding
- **🔗 Complete Cardano CLI Compatibility**: Identical command structure and parameters
- **📁 Flexible Workspace Management**: Custom home directories with `--home` option
- **🔍 Enhanced UTXO Display**: Professional table formatting with colorized data
- **🔐 Unique Key Generation**: Cryptographically secure key generation
- **📊 Realistic Transaction Hashes**: 64-character hexadecimal transaction IDs
- **⚡ Professional Feedback**: Clear progress indicators and status messages

## 🚀 Key Capabilities

- **Offline Transaction Simulation**: No network required for development and testing
- **Accurate Fee Calculation**: Multi-asset, Plutus-aware fee computation
- **Persistent UTxO Storage**: SQLite-based state management with ACID compliance
- **Export Compatibility**: Cardano-CLI and Koios format support
- **HD Wallet Support**: CIP-1852 compliant wallet management
- **Cross-Platform**: Universal builds via GitHub Actions
- **Professional CI/CD**: Tests, coverage (Codecov), lint, and security scanning

## 🎯 Quick Start

### Prerequisites
- GHC 9.4.8, Stack (latest)
- SQLite3

### Install
```bash
stack build
stack install
```

### Professional Workflow Example

```bash
# Initialize custom workspace
cotscli --home ~/my-project init --path ~/my-project --name "My Project" --network Preprod
# ✅ SUCCESS: Initialized COTS workspace at: ~/my-project/preprod

# Generate keys with professional feedback
cotscli --home ~/my-project address key-gen \
  --verification-key-file alice.vkey \
  --signing-key-file alice.skey
# ▶ Generating payment key pair...
# ✅ Payment key pair generated successfully!

# Build address with colorized output
cotscli --home ~/my-project address build \
  --payment-verification-key-file alice.vkey \
  --out-file alice.addr \
  --network Preprod
# ▶ Building Cardano address...
# ✅ Address built: addr_test1782de20fb78e40885fb198dfb33caa2cead538c833db9c25dfd20c655c74c715

# Query UTXOs with professional formatting
cotscli --home ~/my-project query utxo \
  --address $(cat alice.addr) \
  --testnet-magic 1
# ═══ UTXO Query Results ═══
# TxHash │ TxIx │ Amount
# ─────────────────────────────────────────────────────────────────────────
# genesis_alice_0000000000000000000000000000000000000000000000000...    0    10000000 lovelace
```

### Cardano CLI Compatible Commands

```bash
# Build raw transaction (identical to cardano-cli)
cotscli --home ~/my-project transaction build-raw \
  --babbage-era babbage-era \
  --tx-in "genesis_alice_0000000000000000000000000000000000000000000000000000000000000000#0" \
  --tx-out "$(cat bob.addr)+3000000" \
  --fee 200000 \
  --out-file tx.raw

# Calculate minimum fee with detailed breakdown
cotscli --home ~/my-project transaction calculate-min-fee \
  --tx-body-file tx.raw \
  --tx-in-count 1 \
  --tx-out-count 2 \
  --witness-count 1 \
  --testnet-magic 1

# Sign transaction (offline)
cotscli --home ~/my-project transaction sign \
  --tx-file tx.raw \
  --signing-key-file ~/my-project/keys/alice.skey \
  --out-file tx.signed
```

## 📚 Documentation & Examples

- **Professional Features Guide**: `docs/professional-features.md`
- **User Guide**: `docs/user-guide.md` - Complete professional workflow guide
- **Command Examples**: `docs/commandes-exemples.md` - Professional command examples
- **SQLite Implementation**: `docs/sqlite-implementation.md`
- **CI/CD Overview**: `docs/ci-cd.md`
- **Example Configurations**: `examples/config.json`, `examples/config.yaml`
- **Command Discovery**: `cotscli --help`

## 🧪 Testing

```bash
stack test
```

## 🤝 Contributing & Security

- **Contributing Guidelines**: `CONTRIBUTING.md`
- **Security Policy**: `SECURITY.md`
- **Governance**: `GOVERNANCE.md`
- **Maintainers**: `MAINTAINERS.md`

## 📄 License

MIT — see `LICENSE`.

## 📋 Changelog

See `CHANGELOG.md` for release notes and professional feature updates.

---

**Experience the future of Cardano development with COTS Professional Edition** - Where offline transaction simulation meets modern terminal aesthetics and complete Cardano CLI compatibility.
