# Contributing to COTS

Thank you for your interest in contributing to Cardano Offline Transaction Simulator (COTS)! We welcome contributions from the community.

## Table of Contents

- [Code of Conduct](#code-of-conduct)
- [Getting Started](#getting-started)
- [Development Setup](#development-setup)
- [How to Contribute](#how-to-contribute)
- [Coding Standards](#coding-standards)
- [Testing Guidelines](#testing-guidelines)
- [Pull Request Process](#pull-request-process)
- [Reporting Bugs](#reporting-bugs)
- [Suggesting Features](#suggesting-features)

## Code of Conduct

This project adheres to a Code of Conduct that all contributors are expected to follow. Please read [CODE_OF_CONDUCT.md](CODE_OF_CONDUCT.md) before contributing.

## Getting Started

1. **Fork the repository** on GitHub
2. **Clone your fork** locally:
   ```bash
   git clone https://github.com/YOUR_USERNAME/COTS.git
   cd COTS
   ```
3. **Add upstream remote** (to sync with main repo):
   ```bash
   git remote add upstream https://github.com/COTSCLI/COTS.git
   ```

**Note:** You will work on your fork and submit Pull Requests. You must not push directly to the upstream repository.

### Fork-only PR policy

- Open PRs from your fork (`origin`) only; do not push any branches to `upstream`.
- Direct pushes to `upstream` are restricted to maintainers and automation.
- Keep your fork up to date and base your feature branches on the latest `upstream/main`.

Recommended safety guard to prevent accidental upstream pushes:

```bash
# Make upstream fetch-only locally
git remote set-url --push upstream DISABLE
# Verify: upstream should have no push URL
git remote -v
```

Sync your fork with upstream before starting work:

```bash
git fetch upstream
git checkout main
git rebase upstream/main
git push origin main
```

Create a feature branch on your fork and push to your fork:

```bash
git checkout -b feature/your-feature
# ...do work...
git push -u origin feature/your-feature
```

## Development Setup

### Prerequisites

- **GHC**: 9.4.8 or later
- **Stack**: Latest version
- **SQLite3**: For database functionality
- **Git**: For version control

### Building the Project

```bash
# Install dependencies and build
stack build

# Run tests
stack test

# Run with coverage
stack test --coverage

# Generate documentation
stack haddock
```

### Running the Application

```bash
# Run the CLI
stack exec cotscli -- --help

# Or install locally
stack install
cotscli --help
```

## How to Contribute

### Types of Contributions

We welcome various types of contributions:

- **Bug fixes**: Fix issues in the codebase
- **Features**: Add new functionality
- **Documentation**: Improve or add documentation
- **Tests**: Add or improve test coverage
- **Examples**: Add usage examples
- **Performance**: Optimize existing code
- **Refactoring**: Improve code quality

## Coding Standards

### Haskell Style Guide

- Follow standard Haskell style conventions
- Use **2 spaces** for indentation (not tabs)
- Maximum line length: **100 characters**
- Use meaningful variable and function names
- Add type signatures for all top-level functions
- Document complex functions with comments

### Code Quality Tools

We use the following tools to maintain code quality:

```bash
# Run HLint for style suggestions
stack install hlint
hlint src/ app/ test/

# Format code (optional, but recommended)
stack install ormolu
ormolu --mode inplace $(find . -name '*.hs')
```

### Module Organization

```haskell
{-# LANGUAGE ... #-}  -- Language extensions at top

-- |
-- Module documentation here
module MyModule
  ( exportedFunction1
  , exportedFunction2
  ) where

-- Standard library imports
import Data.Text (Text)
import qualified Data.Text as T

-- Third-party imports
import Database.SQLite.Simple

-- Local imports
import COTS.Types
```

## Testing Guidelines

### Writing Tests

- All new features **must** include tests
- Bug fixes should include regression tests
- Aim for **70%+ code coverage** for new code
- Use descriptive test names

### Test Structure

```haskell
describe "Feature Name" $ do
  it "should do something specific" $ do
    -- Arrange
    let input = ...
    
    -- Act
    result <- someFunction input
    
    -- Assert
    result `shouldBe` expected
```

### Running Tests

```bash
# Run all tests
stack test

# Run specific test suite
stack test cardano-offline-transaction-simulator:test:cardano-offline-transaction-simulator-test

# Run with verbose output
stack test --test-arguments="--verbose"

# Run with coverage
stack test --coverage

# View coverage report
stack hpc report --all
```

## Pull Request Process

### Before Submitting

1. **Create a feature branch** (on your fork):
   ```bash
   git checkout -b feature/your-feature-name
   ```

2. **Make your changes** with clear, atomic commits

3. **Write/update tests** for your changes

4. **Run the test suite**:
   ```bash
   stack test --coverage
   ```

5. **Run the linter**:
   ```bash
   hlint src/ app/ test/
   ```

6. **Update documentation** if needed

7. **Add entry to CHANGELOG.md** under "Unreleased"

8. **Push to your fork**:
   ```bash
   git push origin feature/your-feature-name
   ```

9. **Create a Pull Request** from your fork to the main repository

### Commit Message Format

Use clear and descriptive commit messages:

```
<type>: <subject>

<body>

<footer>
```

**Types:**
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation changes
- `test`: Test additions or changes
- `refactor`: Code refactoring
- `perf`: Performance improvements
- `chore`: Build process or tooling changes

**Example:**
```
feat: add minting policy validation

- Implement native script validation
- Add Plutus policy validation
- Include comprehensive test coverage

Closes #123
```

### Pull Request Checklist

- [ ] Code builds without errors (`stack build`)
- [ ] All tests pass (`stack test`)
- [ ] Code coverage is maintained or improved
- [ ] HLint passes with no warnings
- [ ] Documentation is updated
- [ ] CHANGELOG.md is updated
- [ ] Commit messages are clear and descriptive
- [ ] PR description explains the changes

### Review Process

1. **Submit your PR** with a clear description
2. **Wait for CI/CD** to pass (automated tests)
3. **Respond to review comments** promptly
4. **Make requested changes** in new commits
5. **Maintainer will merge** once approved

## Reporting Bugs

### Before Reporting

- Check if the bug has already been reported
- Verify it's not a configuration issue
- Test with the latest version

### Bug Report Template

When reporting bugs, please include:

```markdown
**Description**
Clear description of the bug

**To Reproduce**
Steps to reproduce the behavior:
1. Run command '...'
2. With configuration '...'
3. See error

**Expected Behavior**
What you expected to happen

**Actual Behavior**
What actually happened

**Environment**
- OS: [e.g., Ubuntu 22.04]
- GHC Version: [e.g., 9.4.8]
- Stack Version: [e.g., 2.13.1]
- COTS Version: [e.g., 0.1.0]

**Additional Context**
- Configuration files
- Error logs
- Stack traces
```

## Suggesting Features

We love feature suggestions! Please:

1. **Check existing issues** to avoid duplicates
2. **Describe the use case** clearly
3. **Explain the benefits** to users
4. **Consider implementation** complexity

### Feature Request Template

```markdown
**Problem/Need**
Describe the problem or need this feature addresses

**Proposed Solution**
Describe your suggested solution

**Alternative Solutions**
Any alternative approaches you've considered

**Additional Context**
- Use cases
- Examples
- Related features
```

## Documentation Contributions

Documentation improvements are always welcome:

- Fix typos or clarify existing docs
- Add examples and tutorials
- Improve API documentation
- Translate documentation

## Community

- **Issues**: GitHub Issues for bug reports and features
- **Discussions**: GitHub Discussions for questions and ideas
- **Pull Requests**: For code contributions

## Recognition

Contributors will be:
- Listed in the project's contributors
- Mentioned in release notes
- Added to MAINTAINERS.md (for significant contributions)

## License

By contributing, you agree that your contributions will be licensed under the MIT License.

## Questions?

If you have questions about contributing:
- Open a GitHub Discussion
- Check existing issues and PRs
- Read the documentation in the `docs/` folder

Thank you for contributing to COTS! 🎉

