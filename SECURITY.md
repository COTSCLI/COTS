# Security Policy

## Supported Versions

We aim to keep the latest minor release on the `main` branch secure. Security fixes are generally applied to:

- The latest release on `main`
- The latest prerelease (if applicable)

If you are using older versions, please consider upgrading to receive fixes.

## Reporting a Vulnerability

- Please email security reports to security@cotscli.org.
- If email is not possible, open a GitHub Security Advisory draft in this repository and mark it as private. We will coordinate there.
- Provide a detailed description, reproduction steps, and potential impact. Include environment details (OS, versions) and any logs.

We will acknowledge receipt within 3 business days and provide a timeline for assessment and remediation when possible.

## Coordinated Disclosure

- We prefer coordinated disclosure. Do not disclose publicly until a fix is available and users have a reasonable window to update.
- We will assign a CVE identifier if applicable and credit reporters who wish to be acknowledged.

## Cryptographic Verification

If we publish security releases, release artifacts will be signed. Verify signatures per the instructions in `README.md` or the release notes.

## Security Hardening Guidance

- Build and test in a clean environment
- Keep dependencies up-to-date (Dependabot is enabled)
- Run CI security scans (Trivy) and review alerts
- Use least-privilege for any API keys or tokens used in CI
