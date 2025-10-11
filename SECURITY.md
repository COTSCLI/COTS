# Security Policy

## Supported Versions

We release patches for security vulnerabilities. The following versions are currently supported:

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

The COTS team takes security bugs seriously. We appreciate your efforts to responsibly disclose your findings and will make every effort to acknowledge your contributions.

### How to Report a Security Vulnerability

**Please do not report security vulnerabilities through public GitHub issues.**

Instead, please report them via one of the following methods:

1. **Email**: Send details to **security@cotscli.io** (or the maintainer email in package.yaml)
2. **GitHub Security Advisory**: Use the [GitHub Security Advisory](https://github.com/COTSCLI/COTS/security/advisories/new) feature
3. **Private Disclosure**: Contact the maintainers directly via email

### What to Include

When reporting a security vulnerability, please include:

- **Type of vulnerability** (e.g., SQL injection, authentication bypass, etc.)
- **Full path** of source file(s) related to the vulnerability
- **Location** of the affected source code (tag/branch/commit or direct URL)
- **Step-by-step instructions** to reproduce the issue
- **Proof-of-concept or exploit code** (if possible)
- **Impact** of the vulnerability, including how an attacker might exploit it
- **Your name/affiliation** (if you wish to be credited)

### What to Expect

After submitting a security report:

1. **Acknowledgment**: We will acknowledge receipt within **48 hours**
2. **Assessment**: We will assess the vulnerability and determine its impact within **7 days**
3. **Updates**: We will keep you informed about the progress toward a fix
4. **Disclosure**: We will coordinate the disclosure timeline with you
5. **Credit**: We will credit you in the security advisory (if you wish)

### Security Update Process

1. **Validation**: We confirm the vulnerability and determine its severity
2. **Fix Development**: We develop a fix in a private repository
3. **Testing**: We thoroughly test the fix
4. **Advisory**: We prepare a security advisory
5. **Release**: We release a patched version
6. **Disclosure**: We publish the security advisory

### Security Best Practices for Users

When using COTS, we recommend:

1. **Keep Updated**: Always use the latest version
2. **Secure Storage**: Store wallet keys and sensitive data securely
3. **Review Config**: Carefully review configuration files before use
4. **Network Security**: Be cautious when exporting transactions to external APIs
5. **Database Security**: Protect SQLite database files with appropriate permissions
6. **Audit Scripts**: Review Plutus scripts before simulation
7. **Backup**: Regularly backup your database and configuration files

### Known Security Considerations

#### Offline Transaction Simulation

COTS is designed for **offline** transaction simulation. Keep in mind:

- Private keys are handled locally but should be protected
- Database files contain transaction history and should be secured
- Configuration files may contain sensitive information

#### Database Security

The SQLite database stores:
- UTXO state
- Transaction history
- Wallet addresses (but not private keys)

**Recommendation**: Use file permissions to restrict database access:
```bash
chmod 600 cots.db
```

#### Plutus Script Execution

When simulating Plutus scripts:
- COTS performs offline validation only
- Scripts are not executed on-chain
- Always verify script behavior before real deployment

### Security Audit Status

- **Last Security Review**: TBD
- **External Audit**: Not yet conducted
- **Penetration Testing**: Not yet conducted

### Vulnerability Disclosure Timeline

- **Day 0**: Vulnerability reported
- **Day 2**: Acknowledgment sent
- **Day 7**: Assessment complete
- **Day 30**: Fix developed and tested
- **Day 35**: Patched version released
- **Day 45**: Public disclosure (or earlier if agreed)

### Bug Bounty Program

We currently do not have a formal bug bounty program, but we deeply appreciate security researchers who report vulnerabilities responsibly. We will:

- Publicly acknowledge your contribution (with permission)
- Credit you in release notes and security advisories
- Provide a letter of appreciation for your portfolio

### Security-Related Configuration

#### Recommended File Permissions

```bash
# Configuration files
chmod 600 config.json config.yaml

# Database files
chmod 600 *.db

# Private keys (if stored)
chmod 400 *.skey
```

#### Environment Variables

Avoid storing sensitive information in environment variables. Use secure configuration management instead.

### Security Tools

Our CI/CD pipeline includes:

- **Trivy**: Vulnerability scanning
- **HLint**: Code quality and potential issues
- **Stack**: Dependency verification
- **Codecov**: Code coverage analysis

### Contact

For security concerns, contact:

- **Email**: maintainer email (see MAINTAINERS.md)
- **GitHub**: [@COTSCLI](https://github.com/COTSCLI)
- **Security Advisory**: [Create Advisory](https://github.com/COTSCLI/COTS/security/advisories/new)

### Acknowledgments

We thank the following researchers for responsibly disclosing vulnerabilities:

- (None yet - we'll update this list as needed)

### Additional Resources

- [OWASP Top 10](https://owasp.org/www-project-top-ten/)
- [Cardano Security Best Practices](https://docs.cardano.org/development-guidelines/security/)
- [Haskell Security Guidelines](https://wiki.haskell.org/Security)

---

**Last Updated**: October 11, 2025

