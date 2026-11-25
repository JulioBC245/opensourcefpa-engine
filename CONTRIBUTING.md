# Contributing

Thank you for your interest in the OpenSourceFPA engine.

Code and documentation contributions are paused until the [Contributor Licence Agreement](CLA.md) (draft, version 0.2) has been reviewed by a French IP lawyer and the draft notice is removed. Issues and bug reports are welcome in the meantime.

Once contributions reopen:

1. Open an issue first for anything larger than a small fix, so we can agree on the approach.
2. Never commit real company data, accounting exports or API keys. Use the fictional fixtures in `data/fixtures/`.
3. Run `Rscript tests/run-all.R` before opening a pull request.
4. Keep the licence header at the top of every R file (`SPDX-License-Identifier: AGPL-3.0-or-later`).
5. In the pull request, write: "I have read the CLA (version 0.2) and I accept it." The Maintainer records your GitHub handle, the date, the pull request and the CLA version in [CONTRIBUTORS-CLA.md](CONTRIBUTORS-CLA.md).
