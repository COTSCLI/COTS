# CI/CD & Testing Documentation

## Overview

COTS uses a comprehensive CI/CD pipeline powered by GitHub Actions to ensure code quality, security, and reliability. The pipeline includes testing for the new transaction workflow features and professional output system.

## CI/CD Workflows

### 1. Main CI Pipeline (`ci.yml`)

The main CI pipeline runs on every push and pull request:

#### **Build and Test**
- **Multi-platform**: Runs on Ubuntu and macOS
- **GHC Version**: 9.4.8
- **Cabal Version**: 3.10
- **Dependencies**: Automatic caching for faster builds
- **Coverage**: Generates test coverage reports
- **Transaction Workflow Testing**: Tests new `transaction send` command

**Workflow Steps:**
1. Checkout code
2. Setup Haskell environment
3. Cache dependencies
4. Install system dependencies (SQLite)
5. Build project with coverage enabled
6. Run test suite including transaction workflow tests
7. Generate coverage reports
8. Upload to Codecov

#### **Code Linting**
- **HLint**: Haskell linter for code quality
- **Reports**: Generates HTML reports
- **Artifacts**: Uploads lint reports
- **Professional Output**: Validates colorized output formatting

#### **Security Scanning**
- **Trivy**: Vulnerability scanner
- **SARIF**: Security Analysis Results Interchange Format
- **GitHub Security**: Automatic security alerts
- **Cryptographic Testing**: Validates transaction hash generation

#### **Documentation**
- **Haddock**: Generates API documentation
- **Artifacts**: Uploads generated docs
- **Professional Features**: Documents new transaction workflow

#### **Release Binaries**
- **Platforms**: Ubuntu and macOS
- **Trigger**: Only on main branch pushes
- **Output**: Platform-specific binaries
- **Transaction Commands**: Includes new `transaction send` command

### 2. Coverage Workflow (`coverage.yml`)

Dedicated coverage analysis with enhanced reporting:

#### **Coverage Analysis**
- **Comprehensive**: Tests all transaction workflow components
- **Professional Output**: Coverage for colorized output system
- **UTXO Management**: Coverage for enhanced UTXO features
- **Database Operations**: Coverage for SQLite operations

#### **Coverage Reports**
- **Codecov Integration**: Automatic coverage reporting
- **Threshold Enforcement**: Minimum coverage requirements
- **Professional Metrics**: Coverage for professional features

### 3. SBOM Workflow (`sbom.yml`)

Software Bill of Materials generation:

#### **SBOM Generation**
- **CycloneDX**: Industry-standard SBOM format
- **Dependency Tracking**: Complete dependency tree
- **Security Compliance**: Vulnerability tracking
- **Professional Features**: Includes new transaction workflow dependencies

## Testing Strategy

### Unit Tests

#### **Transaction Workflow Tests**
```haskell
-- Test transaction send command
testTransactionSend :: IO ()
testTransactionSend = do
  -- Test complete workflow
  result <- runTransactionSend testOptions
  assertEqual "Transaction should succeed" Success result
  
  -- Test UTXO updates
  utxos <- queryUTXOs testAddress
  assertEqual "Should have correct UTXOs" expectedUTXOs utxos
```

#### **Professional Output Tests**
```haskell
-- Test colorized output
testProfessionalOutput :: IO ()
testProfessionalOutput = do
  output <- captureOutput $ runCommand testCommand
  assertContains "Should contain color codes" "\033[1;92m" output
  assertContains "Should contain success message" "✅" output
```

#### **UTXO Management Tests**
```haskell
-- Test enhanced UTXO features
testUTXOManagement :: IO ()
testUTXOManagement = do
  -- Test address association
  utxos <- getUTXOsByAddress testAddress
  assertEqual "Should filter by address" expectedCount (length utxos)
  
  -- Test dynamic UTXO creation
  createUTXO testAddress testAmount
  utxos' <- getUTXOsByAddress testAddress
  assertEqual "Should create new UTXO" (expectedCount + 1) (length utxos')
```

### Integration Tests

#### **Complete Transaction Workflow**
```bash
#!/bin/bash
# Test complete transaction workflow

# Initialize workspace
cotscli --home ~/test init --path ~/test --name "Test" --network Preprod

# Initialize database
cotscli --home ~/test database init --db-file test.db

# Generate keys and addresses
cotscli --home ~/test address key-gen --verification-key-file alice.vkey --signing-key-file alice.skey
cotscli --home ~/test address build --payment-verification-key-file alice.vkey --out-file alice.addr --network Preprod --initial-amount 10000000

# Generate and import UTXOs
cotscli --home ~/test database generate-utxo --addresses "$(cat alice.addr)" --amounts "10000000" --out-file initial-utxos.json
cotscli --home ~/test database import-utxo --utxo-file initial-utxos.json --db-file test.db

# Create wallet
cotscli --home ~/test wallet create --name "Alice" --address "$(cat alice.addr)" --db-file test.db

# Test transaction send command
cotscli --home ~/test transaction send \
  --from-address "$(cat alice.addr)" \
  --to-address "$(cat alice.addr)" \
  --amount 1000000 \
  --db-file test.db \
  --out-file test-tx \
  --signing-key-file alice.skey \
  --testnet-magic 1

# Verify transaction ID
cotscli --home ~/test transaction txid --tx-file test-tx.signed

# Verify UTXOs updated
cotscli --home ~/test query utxo --address "$(cat alice.addr)" --testnet-magic 1 --db-file test.db
```

#### **Professional Output Validation**
```bash
#!/bin/bash
# Test professional output formatting

# Test colorized output
output=$(cotscli --home ~/test address key-gen --verification-key-file test.vkey --signing-key-file test.skey 2>&1)
echo "$output" | grep -q "✅" || exit 1
echo "$output" | grep -q "▶" || exit 1
echo "$output" | grep -q "ℹ" || exit 1

# Test table formatting
output=$(cotscli --home ~/test query utxo --address test.addr --testnet-magic 1 --db-file test.db 2>&1)
echo "$output" | grep -q "═══" || exit 1
echo "$output" | grep -q "│" || exit 1
```

### Performance Tests

#### **Transaction Workflow Performance**
```haskell
-- Test transaction workflow performance
testTransactionPerformance :: IO ()
testTransactionPerformance = do
  startTime <- getCurrentTime
  replicateM_ 100 $ runTransactionSend testOptions
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  assertBool "Should complete within reasonable time" (duration < 10)
```

#### **Database Performance**
```haskell
-- Test database performance with large datasets
testDatabasePerformance :: IO ()
testDatabasePerformance = do
  -- Create large number of UTXOs
  utxos <- replicateM 1000 $ createTestUTXO
  startTime <- getCurrentTime
  mapM_ insertUTXO utxos
  endTime <- getCurrentTime
  let duration = diffUTCTime endTime startTime
  assertBool "Should handle large datasets efficiently" (duration < 5)
```

## Quality Assurance

### Code Quality

#### **Static Analysis**
- **HLint**: Haskell linter for code quality
- **Professional Standards**: Enforces professional coding standards
- **Transaction Workflow**: Validates transaction workflow implementation
- **Output Formatting**: Ensures consistent professional output

#### **Type Safety**
- **Haskell Type System**: Leverages strong typing for reliability
- **Transaction Types**: Type-safe transaction operations
- **UTXO Types**: Type-safe UTXO management
- **Professional Types**: Type-safe professional output system

### Security Testing

#### **Cryptographic Testing**
```haskell
-- Test transaction hash generation
testTransactionHashGeneration :: IO ()
testTransactionHashGeneration = do
  hash1 <- generateTransactionHash 1
  hash2 <- generateTransactionHash 2
  assertBool "Hashes should be different" (hash1 /= hash2)
  assertEqual "Hash should be 64 characters" 64 (length hash1)
  assertBool "Hash should be hexadecimal" (all isHexDigit hash1)
```

#### **Input Validation**
```haskell
-- Test input validation
testInputValidation :: IO ()
testInputValidation = do
  -- Test invalid amounts
  result1 <- runTransactionSend (testOptions { amount = 0 })
  assertEqual "Should reject zero amount" ValidationError result1
  
  -- Test invalid addresses
  result2 <- runTransactionSend (testOptions { toAddress = "invalid" })
  assertEqual "Should reject invalid address" ValidationError result2
```

### Professional Output Testing

#### **Color Output Validation**
```haskell
-- Test colorized output
testColorOutput :: IO ()
testColorOutput = do
  output <- captureOutput $ successMsg "Test message"
  assertContains "Should contain green color" "\033[1;92m" output
  assertContains "Should contain checkmark" "✅" output
```

#### **Table Formatting Tests**
```haskell
-- Test table formatting
testTableFormatting :: IO ()
testTableFormatting = do
  output <- captureOutput $ printUTXOTable testUTXOs
  assertContains "Should contain table headers" "TxHash" output
  assertContains "Should contain separators" "│" output
  assertContains "Should contain borders" "═══" output
```

## Continuous Integration Features

### Automated Testing

#### **Transaction Workflow Tests**
- **Complete Workflow**: Tests entire transaction process
- **Individual Steps**: Tests each step of the workflow
- **Error Handling**: Tests error scenarios
- **Performance**: Tests workflow performance

#### **Professional Output Tests**
- **Color Validation**: Tests colorized output
- **Format Validation**: Tests table formatting
- **Message Validation**: Tests professional messages
- **Accessibility**: Tests output accessibility

#### **UTXO Management Tests**
- **Address Association**: Tests UTXO-address association
- **Dynamic Creation**: Tests dynamic UTXO creation
- **Change Handling**: Tests change UTXO handling
- **Database Updates**: Tests database updates

### Code Coverage

#### **Coverage Targets**
- **Transaction Workflow**: 95% coverage for transaction commands
- **Professional Output**: 90% coverage for output system
- **UTXO Management**: 95% coverage for UTXO operations
- **Database Operations**: 90% coverage for database functions

#### **Coverage Reports**
- **HTML Reports**: Detailed coverage reports
- **Codecov Integration**: Automatic coverage reporting
- **Threshold Enforcement**: Minimum coverage requirements
- **Professional Metrics**: Coverage for professional features

### Security Scanning

#### **Vulnerability Scanning**
- **Trivy Scanner**: Automated vulnerability scanning
- **SARIF Reports**: Security Analysis Results Interchange Format
- **GitHub Security**: Automatic security alerts
- **Dependency Scanning**: Third-party dependency scanning

#### **Cryptographic Validation**
- **Hash Generation**: Validates transaction hash generation
- **Key Generation**: Validates key generation security
- **Random Number Generation**: Validates random number generation
- **Professional Security**: Validates professional security features

## Deployment Pipeline

### Release Process

#### **Automated Releases**
- **Version Bumping**: Automatic version management
- **Changelog Generation**: Automatic changelog updates
- **Binary Generation**: Cross-platform binary generation
- **Documentation Updates**: Automatic documentation updates

#### **Release Validation**
- **Transaction Workflow**: Validates transaction commands
- **Professional Output**: Validates professional features
- **UTXO Management**: Validates UTXO operations
- **Database Compatibility**: Validates database operations

### Quality Gates

#### **Pre-Release Checks**
- **Test Coverage**: Minimum coverage requirements
- **Performance Tests**: Performance benchmarks
- **Security Scans**: Security vulnerability checks
- **Professional Standards**: Professional output validation

#### **Post-Release Validation**
- **Integration Tests**: End-to-end testing
- **User Acceptance**: User acceptance testing
- **Performance Monitoring**: Performance monitoring
- **Professional Feedback**: Professional feature validation

## Monitoring and Observability

### Test Metrics

#### **Transaction Workflow Metrics**
- **Success Rate**: Transaction workflow success rate
- **Performance**: Transaction workflow performance
- **Error Rate**: Transaction workflow error rate
- **Professional Quality**: Professional output quality

#### **Professional Output Metrics**
- **Color Accuracy**: Color output accuracy
- **Format Consistency**: Format consistency
- **Message Quality**: Professional message quality
- **User Experience**: User experience metrics

### Continuous Improvement

#### **Test Optimization**
- **Test Speed**: Optimize test execution speed
- **Test Coverage**: Improve test coverage
- **Test Quality**: Improve test quality
- **Professional Standards**: Maintain professional standards

#### **Professional Enhancement**
- **Output Quality**: Enhance professional output
- **User Experience**: Improve user experience
- **Feature Completeness**: Complete professional features
- **Documentation Quality**: Improve documentation quality

## Best Practices

### Testing Best Practices

1. **Comprehensive Testing**: Test all transaction workflow components
2. **Professional Validation**: Validate professional output features
3. **Performance Testing**: Test performance with large datasets
4. **Security Testing**: Test cryptographic and security features
5. **Integration Testing**: Test complete workflows end-to-end

### CI/CD Best Practices

1. **Automated Testing**: Automate all testing processes
2. **Quality Gates**: Implement quality gates for releases
3. **Security Scanning**: Regular security vulnerability scanning
4. **Professional Standards**: Maintain professional coding standards
5. **Documentation**: Keep documentation up-to-date

### Professional Standards

1. **Code Quality**: Maintain high code quality standards
2. **Professional Output**: Ensure consistent professional output
3. **User Experience**: Focus on user experience quality
4. **Feature Completeness**: Complete professional feature implementation
5. **Documentation**: Maintain comprehensive documentation

## Conclusion

The COTS CI/CD pipeline ensures high-quality, professional software delivery with comprehensive testing of the new transaction workflow features and professional output system. The pipeline includes:

- **Complete transaction workflow testing**
- **Professional output validation**
- **Enhanced UTXO management testing**
- **Security and performance validation**
- **Professional quality assurance**

This ensures that COTS delivers a professional, reliable, and secure Cardano transaction simulation environment! 🚀