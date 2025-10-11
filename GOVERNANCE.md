# Project Governance

This document describes how decisions are made and how the project is governed.

## Roles

- Maintainers: Own technical direction, triage, reviews, and releases. Listed in `MAINTAINERS.md`.
- Contributors: Anyone submitting issues, PRs, docs, or other improvements.
- Users: People who use the software and provide feedback.

## Principles

- Open, transparent, and inclusive collaboration
- Lazy consensus: silence is consent after a reasonable time
- Security-first for changes affecting user safety

## Decision Making

- Routine changes: decided via PR review by maintainers; at least one maintainer approval required; CI must pass
- Significant changes (architecture, deprecations, security posture): open an RFC-style issue for discussion; seek consensus among maintainers
- If consensus cannot be reached, the Lead Maintainer (see `MAINTAINERS.md`) decides

## Proposals (RFCs)

For significant changes:

1. Open an issue titled "RFC: <topic>"
2. Outline problem, motivation, design, alternatives, and migration plan
3. Keep discussion focused and time-boxed; target resolution within 2–4 weeks

## Releases

- Automated release process via GitHub Actions and Release Please
- Semantic versioning: MAJOR.MINOR.PATCH
- Security fixes may trigger out-of-band PATCH releases

## Security

- Follow `SECURITY.md` for reporting and disclosure
- Maintainers coordinate fixes and advisories

## Conflict Resolution

- Start with respectful discussion on the issue/PR
- Escalate to maintainers if unresolved
- Lead Maintainer provides final decision when necessary

## Changes to This Document

Changes to governance follow the same process as significant changes: open an RFC, discuss, and reach consensus among maintainers.
