# Developer notes

How the OpenSourceFPA engine is laid out, built and tested.

## Layout

| Path | Role |
|---|---|
| `R/` | Engine modules (config, validation, P&L, working capital, cash, balance sheet, drivers, scenarios, variance, reports, screen entry) |
| `app.R` | Local Shiny screen: statements, variance, scenarios, drivers |
| `fpa-engine.R` | Entry point. With no argument it looks for `fpa-config.json` beside itself, then in `config/`, then in `data/fixtures/config/` |
| `engine-modules.txt` | Ordered list of modules that make up the engine |
| `tools/assemble-engine.R` | Builds a single readable `fpa-engine.R` from the modules in that list |
| `config/` | Example configuration (`fpa-config.example.json`) |
| `data/fixtures/` | Fictional company "Exemple SAS" used by the tests |
| `tests/` | Test suite, including acceptance gates 1–9 |
| `input/modeles/` | Model CSV templates and `LISEZMOI.txt` written on first run |
| `work/` · `reports/` | Local outputs from `fpa_run()` (gitignored; never commit customer data) |

## Assemble the single-file engine

From the repository root, pass the output path:

```
Rscript tools/assemble-engine.R path/to/fpa-engine.R
```

The assembler concatenates every module named in `engine-modules.txt`, in that order, into one readable source file. A name in the list that is missing from `R/` stops the build.

## Fixtures and tests

`data/fixtures/` holds a fictional 24-month company, Exemple SAS. Generate or refresh it with `Rscript data/fixtures/build-fixtures.R` from the repository root.

Run the suite from the repository root:

```
Rscript tests/run-all.R
```

`tests/test-init.R` also asserts that `fpa_init()` produces no `.bat`, `.exe`, `.cmd`, `.ps1`, `.vbs` or `.msi` anywhere under the working root.

## Run the engine and the screen

The product ships as readable R source, not as an archive, installer or executable. The configuration assistant at opensourcefpa.com hands over two text files: `fpa-config.json` (settings, line mappings, model tables) and `fpa-engine.R` (the assembled engine). Place `app.R` beside them.

```
Rscript fpa-engine.R "C:/FPA-Local/fpa-config.json"
```

Or, in RStudio, open `fpa-engine.R` and press Source. The engine needs `jsonlite`. It needs `readxl` to read an Excel workbook, and `openxlsx` to write into one. The screen needs `shiny` and `bslib`. The screen also uses `plotly` when it is installed; without it, the same charts stay in base R.

On first run, `fpa_init()` creates the working folders from `paths.working_root_hint` in the JSON (and creates that root if it does not exist):

```
C:\FPA-Local\                 <- working_root_hint
  input\                      <- the user saves their own files here
    modeles\                  <- model CSVs written from the configuration, plus LISEZMOI.txt
  work\                       <- pnl-monthly.csv, bs-monthly.csv, cash-bridge-monthly.csv
  reports\                    <- findings.csv
```

`work/`, `reports/` and `exports/` follow `working_root_hint`, not the folder that holds `fpa-engine.R`. `fpa_init()` never overwrites a file the user has already edited unless `overwrite = TRUE`.

Open the screen from an R session that already has the engine loaded:

```r
fpa_app("C:/FPA-Local/fpa-config.json")
```

`app.R` must sit beside that config, or beside the engine. The product does not ship a `.bat` or an `.exe`. A one-time Windows shortcut is optional; the recipe is in `docs/Second-Person-Cycle.md`.

Called directly, the two steps are:

```r
source("R/fpa-paths.R"); source("R/fpa-config.R"); fpa_source_all()
fpa_init("C:/FPA-Local/fpa-config.json")   # folders + model tables
fpa_run("C:/FPA-Local/fpa-config.json")    # three statements
```

## What the engine covers

- Load `fpa-config.json` (the website assistant contract)
- `fpa_init()`: create the working folder and model tables from that JSON
- Read P&L, opening balance sheet, budget, parameters, decisions and volumes
- Plain-French validation (missing file, month gap, sign flip, balance-sheet balance)
- Monthly P&L through net income, DSO/DPO/DIO working capital, an indirect cash bridge, and a rolling balance sheet
- No plug: cash comes from the bridge; equity rolls with net income; `A = L + E` is asserted

Gates 1–9 are implemented. Horizon, variance and freeze have their own checks. Gates 10, 12, 13 and 15 are started in `fpa-freeze.R` and are not the full ship bar yet. There is no paid or self-service ship until the monthly three statements articulate and acceptance gates 1–17 are green.
