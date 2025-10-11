# Cardano Offline Transaction Simulator (COTS)

[![CI/CD Pipeline](https://github.com/COTSCLI/COTS/actions/workflows/ci.yml/badge.svg)](https://github.com/COTSCLI/COTS/actions/workflows/ci.yml)
[![Code Coverage](https://github.com/COTSCLI/COTS/actions/workflows/coverage.yml/badge.svg)](https://github.com/COTSCLI/COTS/actions/workflows/coverage.yml)
[![codecov](https://codecov.io/gh/COTSCLI/COTS/branch/main/graph/badge.svg)](https://codecov.io/gh/COTSCLI/COTS)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Haskell](https://img.shields.io/badge/Haskell-9.4.8-blue.svg)](https://www.haskell.org)

COTS is a CLI tool for simulating Cardano transactions offline: compute fees, manage UTxOs in SQLite, and export transactions in Cardano-CLI or Koios formats.

## Features

- Offline transaction simulation (no network required)
- Accurate fee calculation (multi-asset, Plutus-aware)
- Persistent UTxO/state storage in SQLite
- Export to Cardano-CLI and Koios formats
- HD wallet support (CIP‑1852)
- Cross‑platform builds via GitHub Actions
- CI: tests, coverage (Codecov), lint, and security scanning

## Quick Start

### Prerequisites
- GHC 9.4.8, Stack (latest)
- SQLite3

### Install
```bash
stack build
```

### Simulate a transaction
```bash
cotscli simulate \
  -c examples/config.json \
  -f alice \
  -t <destination_address> \
  -a 100000000
```

### Validate configuration
```bash
cotscli validate -c examples/config.json
```

### Export for Cardano‑CLI
```bash
cotscli simulate -c examples/config.json -f alice -t <addr> -a 100000000 --export-cardano-cli
```

## Documentation & Examples
- User Guide: `docs/user-guide.md`
- SQLite design notes: `docs/sqlite-implementation.md`
- CI/CD overview: `docs/ci-cd.md`
- Example configs: `examples/config.json`, `examples/config.yaml`
- Discover commands: `cotscli --help`

## Testing
```bash
stack test
```

## Contributing & Security
- Contributing guidelines: `CONTRIBUTING.md`
- Security policy and reporting: `SECURITY.md`

## Governance & Maintainers
- Governance: `GOVERNANCE.md`
- Maintainers: `MAINTAINERS.md`

## License
MIT — see `LICENSE`.

## Changelog
See `CHANGELOG.md` for release notes.
