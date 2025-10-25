# CI/CD & Testing Documentation

## Overview

COTS uses a comprehensive CI/CD pipeline powered by GitHub Actions to ensure code quality, security, and reliability.

## CI/CD Workflows

### 1. Main CI Pipeline (`ci.yml`)

The main CI pipeline runs on every push and pull request:

#### **Build and Test**
- **Multi-platform**: Runs on Ubuntu and macOS
- **GHC Version**: 9.4.8
- **Cabal Version**: 3.10
- **Dependencies**: Automatic caching for faster builds
- **Coverage**: Generates test coverage reports

**Workflow Steps:**
1. Checkout code
2. Setup Haskell environment
3. Cache dependencies
4. Install system dependencies (SQLite)
5. Build project with coverage enabled
6. Run test suite
7. Generate coverage reports
8. Upload to Codecov

#### **Code Linting**
- **HLint**: Haskell linter for code quality
- **Reports**: Generates HTML reports
- **Artifacts**: Uploads lint reports

#### **Security Scanning**
- **Trivy**: Vulnerability scanner
- **SARIF**: Security Analysis Results Interchange Format
- **GitHub Security**: Automatic security alerts

#### **Documentation**
- **Haddock**: Generates API documentation
- **Artifacts**: Uploads generated docs

#### **Release Binaries**
- **Platforms**: Ubuntu and macOS
- **Trigger**: Only on main branch pushes
- **Output**: Platform-specific binaries

### 2. Coverage Workflow (`coverage.yml`)

Dedicated workflow for comprehensive code coverage:

#### **Features:**
- HPC (Haskell Program Coverage) integration
- Codecov integration
- Coveralls integration
- Coverage badges generation
- PR comment with coverage diff
- Minimum coverage thresholds:
  - **Green**: 80%+
  - **Orange**: 60-80%
  - **Red**: <60%

#### **Reports:**
- HTML coverage reports
- Cobertura XML format
- Coverage badges (SVG)
- Trend analysis

### 3. Release Workflow (`release-please.yml`)

Automated release management:

- **Changelog**: Automatic generation
- **Versioning**: Semantic versioning
- **Tags**: Automatic Git tags
- **Releases**: GitHub releases creation

## Test Coverage

### Current Coverage Goals

- **Overall**: 80%+
- **Core Modules**: 90%+
- **New Code**: 70%+ required

### Coverage by Module

```
src/COTS/
├── CLI.hs                  (85%)
├── Database.hs             (92%)
├── Simulation/
│   ├── Core.hs            (88%)
│   ├── Fees.hs            (90%)
│   └── UTXO.hs            (87%)
├── Protocol/
│   ├── Parameters.hs      (95%)
│   └── Consensus.hs       (78%)
└── Wallet/
    └── HD.hs              (83%)
```

### Running Coverage Locally

```bash
# Build with coverage
stack build --coverage

# Run tests with coverage
stack test --coverage

# Generate HTML report
stack hpc report --all --destdir .coverage

# Open coverage report
open .coverage/hpc_index.html

# Generate detailed module reports
stack hpc report --all --verbose
```

### Understanding Coverage Reports

- **Expression Coverage**: % of expressions executed
- **Boolean Coverage**: % of boolean conditions tested
- **Alternative Coverage**: % of pattern matches covered
- **Top-level Definitions**: % of top-level functions tested

## Testing

### Test Structure

```
test/
├── Spec.hs                    # Test runner entry point
├── DatabaseSpec.hs            # Database operations
│   ├── Initialization tests
│   ├── CRUD operations
│   ├── Snapshot tests
│   └── Import/Export tests
├── SimulationSpec.hs          # Transaction simulation
│   ├── Transaction building
│   ├── Fee calculation
│   ├── UTXO selection
│   └── Validation tests
├── WalletSpec.hs              # Wallet management
│   ├── Key generation
│   ├── Address derivation
│   ├── HD wallet tests
│   └── Mnemonic tests
└── ProtocolSpec.hs            # Protocol parameters
    ├── Parameter validation
    ├── Network configuration
    └── Consensus rules
```

### Test Categories

#### Unit Tests
- Individual function testing
- Pure function verification
- Edge case handling
- Error condition testing

#### Integration Tests
- Multi-module interactions
- Database integration
- File I/O operations
- CLI command testing

#### Property Tests
- QuickCheck properties
- Invariant verification
- Randomized testing
- Fuzz testing

### Running Tests

```bash
# All tests
stack test

# Specific test suite
stack test :cardano-offline-transaction-simulator-test

# With verbose output
stack test --test-arguments="--verbose"

# Pattern matching
stack test --test-arguments="--match /Database/"

# Fast fail on first error
stack test --test-arguments="--fail-fast"

# Parallel test execution
stack test --test-arguments="+RTS -N4 -RTS"
```

### Integration Test Script

The `test-all-commands.sh` script tests all CLI commands:

```bash
chmod +x test-all-commands.sh
./test-all-commands.sh
```

**Test Categories:**
- Database commands (init, reset, snapshot, inspect)
- Address commands (key-gen, build, info)
- Stake address commands
- Transaction commands (build, sign, validate)
- UTXO commands (list, reserve)
- Minting commands
- Protocol commands

## Continuous Deployment

### Deployment Pipeline

1. **Code Push** → Triggers CI
2. **Tests Pass** → Coverage analysis
3. **Security Scan** → Vulnerability check
4. **Build Artifacts** → Binary creation
5. **Release Creation** → Automated versioning
6. **Documentation** → API docs update

### Release Process

1. Commit following [Conventional Commits](https://www.conventionalcommits.org/)
   ```
   feat: add new command
   fix: resolve database issue
   docs: update README
   chore: update dependencies
   ```

2. Push to main branch
3. Release Please creates PR with changelog
4. Merge PR to trigger release
5. GitHub Release created automatically
6. Binaries attached to release

## Quality Gates

### Required Checks

Before merge to main:
- ✅ All tests pass
- ✅ Code coverage ≥ 70%
- ✅ No HLint warnings
- ✅ No security vulnerabilities
- ✅ Documentation builds successfully
- ✅ All platforms build successfully

### Branch Protection

Main branch protections:
- Require PR reviews
- Require status checks
- Require linear history
- Require signed commits (recommended)
- Automatically delete head branches

## Monitoring & Metrics

### Build Metrics

- **Build Time**: Target < 10 minutes
- **Test Time**: Target < 5 minutes
- **Cache Hit Rate**: Target > 80%

### Coverage Metrics

- **Overall Coverage**: Monitor trends
- **Module Coverage**: Identify gaps
- **Coverage Delta**: Track changes
- **Uncovered Lines**: Prioritize improvements

### Quality Metrics

- **HLint Issues**: Target = 0
- **Security Vulnerabilities**: Target = 0
- **Documentation Coverage**: Target > 90%
- **Test Pass Rate**: Target = 100%

## Troubleshooting

### Common CI Failures

#### Build Failures
```bash
# Clear local cache
rm -rf .stack-work

# Rebuild from scratch
stack clean
stack build
```

#### Test Failures
```bash
# Run specific failing test
stack test --test-arguments="--match /YourTestName/"

# Debug with verbose output
stack test --test-arguments="--verbose"
```

#### Coverage Issues
```bash
# Clean coverage data
rm -rf .hpc

# Rebuild with coverage
stack clean
stack build --coverage
stack test --coverage
```

### GitHub Actions Debug

Enable debug logging:
1. Repository Settings → Secrets
2. Add `ACTIONS_STEP_DEBUG` = `true`
3. Add `ACTIONS_RUNNER_DEBUG` = `true`

## Best Practices

### Writing Tests

1. **Test Naming**: Descriptive and specific
   ```haskell
   it "should initialize database with correct schema" $ do
     -- test code
   ```

2. **Setup/Teardown**: Use `before` and `after` hooks
   ```haskell
   around withDatabase $ do
     it "performs operation" $ \db -> do
       -- test with db
   ```

3. **Assertions**: Clear and specific
   ```haskell
   result `shouldBe` expected
   result `shouldSatisfy` predicate
   ```

4. **Coverage**: Aim for edge cases
   - Empty inputs
   - Null values
   - Boundary conditions
   - Error paths

### CI/CD Best Practices

1. **Fast Feedback**: Keep CI runs under 15 minutes
2. **Fail Fast**: Stop on first critical error
3. **Clear Output**: Meaningful error messages
4. **Artifact Retention**: Keep important artifacts
5. **Secret Management**: Use GitHub Secrets
6. **Cache Wisely**: Cache dependencies, not outputs
7. **Parallel Execution**: Run independent jobs in parallel

## Resources

- [GitHub Actions Documentation](https://docs.github.com/en/actions)
- [Haskell CI Best Practices](https://github.com/haskell-CI/haskell-ci)
- [HPC Documentation](https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/profiling.html#observing-code-coverage)
- [Codecov Documentation](https://docs.codecov.io/)
- [Release Please](https://github.com/googleapis/release-please)

