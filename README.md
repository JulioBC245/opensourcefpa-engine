# OpenSourceFPA engine

Local-first FP&A engine for small and medium-sized companies, written in R with a Shiny screen.

It reads your accounting exports on your own computer, builds a driver-based forecast (P&L, working capital, cash and balance sheet), tests candidate business drivers statistically, and tracks monthly variance against budget. Your accounting data stays on your PC. The setup file (`fpa-config.json`) is produced by the configuration assistant at [opensourcefpa.com](https://opensourcefpa.com); it contains settings and line mappings, not ledger amounts.

## What's inside

| Path | Role |
|---|---|
| `R/` | Engine modules (config, validation, P&L, working capital, cash, balance sheet, drivers, scenarios, variance, reports) |
| `app.R` | Local Shiny screen: statements, variance, scenarios, drivers |
| `fpa-engine.R`, `engine-modules.txt`, `tools/assemble-engine.R` | Single-file engine build |
| `config/` | Example configuration |
| `data/fixtures/` | Fictional example company "Exemple SAS" used by the tests |
| `tests/` | Test suite (`tests/run-all.R`) |
| `docs/` | Notes on the monthly cycle, vertical packs and development |

## Quick start

1. Install R from [CRAN](https://cran.r-project.org/).
2. In R, once: `install.packages(c("jsonlite", "shiny", "bslib", "plotly", "readxl", "openxlsx"))`
3. From this folder: `Rscript tests/run-all.R` to run the tests on the example company.
4. Open the screen with `Rscript app.R "path/to/fpa-config.json"`.

## Optional AI commentary

The engine can draft management commentary with an AI provider of your choice, using your own API key. It is off by default. When you turn it on, figures (not your company name) are sent to that provider. No AI is used in the calculations.

## Licence

Copyright (C) 2026 Julio Bauer Cantuaria.

This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. See [LICENSE](LICENSE).

## Contributing

Contributions are welcome. Before your first pull request is merged, you need to accept the [Contributor Licence Agreement](CLA.md). See [CONTRIBUTING.md](CONTRIBUTING.md).
