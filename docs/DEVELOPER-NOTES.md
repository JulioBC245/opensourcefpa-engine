# fpa-local

Sibling product codebase for the local-first self-service FP&A application.

- **This folder** is the commercial product tree (M1).
- **Do not** treat `../driver-based-fp&a-ecosystem` as the product. That project stays the XYZ Sugar Co. LinkedIn / Netlify showcase.

## Spec

Governing build contract (Obsidian):

`FinanceAI/FPnA Master Folder/SaaS Idea Assessment/M1 Build Specification - Local-First Self-Service FPA.md`

Shared chat handoff (Engine builder ↔ Web builder): `../docs/HANDOFF-Engine-Web-Builders.md`

Locked from the interactive review on 2026-09-17.

## Layout

| Path | Role |
|---|---|
| `R/` | Engine modules (patterns from sugar, maths written fresh) |
| `data/fixtures/` | Fictional Exemple SAS inputs + config |
| `config/` | Schema examples |
| `tests/` | Gates 1–9, horizon, variance, freeze, and the driver pass |
| `work/` · `reports/` | Local outputs from `fpa_run()` |
| `app.R` | Local screen: statements, variance, drivers. `fpa_app()` opens it |

## How this reaches a customer

No archive, no installer, no executable from the website.

1. The customer installs R themselves from the official source, `cran.r-project.org/bin/windows/base/`.
2. The engine is readable R source: their IT can read every line before approving it.
3. The website hands over **two text files**: `fpa-config.json` (settings, line mappings, model tables) and `fpa-engine.R` (the engine, assembled as one readable source file). Neither is a compiled executable.
4. `fpa_init()` creates `input/`, `input/modeles/`, `work/`, `reports/` and writes the model CSVs from that JSON. This replaces everything the old downloadable zip used to carry.

**Open decision (2026-09-18):** where the engine is hosted is deferred. Local testing only for now. Until a public repository exists, the wizard must not claim one — the Revue panel talks about R and about source readability, nothing more.

`START.bat` and the zip builder were removed on 2026-09-18: a `.bat` inside a downloaded archive is the exact shape antivirus, SmartScreen, and SME IT gateways flag, and nothing needed it.

The subscriber runs **one file**, `fpa-engine.R`. It is readable text, not a binary. The name is deliberate: `run.R` reads like a command to a stranger, and a file called `run` in a download folder is exactly what a cautious buyer refuses to open.

```powershell
Rscript fpa-engine.R "C:\FPA-Local\fpa-config.json"
```

Or, in RStudio: open `fpa-engine.R` and press Source. With no argument it looks for `fpa-config.json` beside itself, then in `config/`, then in `data/fixtures/config/`.

### Folder structure on the customer's machine

The wizard creates nothing on disk. The engine does, on first run, from `paths.working_root_hint` in the JSON (the "Dossier de travail" field on step 6). `dir.create(recursive = TRUE)` means the root itself is created when it does not exist, so the customer never makes a folder by hand.

```
C:\FPA-Local\                 <- working_root_hint
  input\                      <- the customer saves their own files here
    modeles\                  <- model CSVs written from file_drafts, + LISEZMOI.txt
  work\                       <- pnl-monthly.csv, bs-monthly.csv, cash-bridge-monthly.csv
  reports\                    <- findings.csv
```

Run once with no data and the engine creates the structure, names the three files it expects, points at `input\modeles\`, and exits with status 0 (no R error trace). Fill `input\`, run again, and it computes.

`work/`, `reports/`, and `exports/` follow `working_root_hint`, **not** the folder holding `fpa-engine.R` (fixed 2026-09-18: a subscriber who left the engine in `Downloads` got results in `Downloads\work` while empty folders sat in `C:\FPA-Local`). Existing files are never overwritten by `fpa_init()`.

`fpa-engine.R` sources the engine, calls `fpa_init()` then `fpa_run()`, and prints a plain-French summary (periods, revenue, net income, closing cash, whether the balance sheet balanced, anything to check, files written). It needs `jsonlite`. It needs `readxl` to read an Excel workbook, and `openxlsx` to write into one. The screen needs `shiny` and `bslib`.

Open the screen from the R session that already has the engine loaded, with no batch file:

```r
fpa_app("C:/FPA-Local/fpa-config.json")
```

`app.R` must sit beside that config, or beside the engine. The product does not ship a `.bat` or an `.exe`. Create a one-time Windows shortcut yourself. Full recipe: `docs/Second-Person-Cycle.md`. Example target:

```text
"C:\Program Files\R\R-4.6.0\bin\Rscript.exe" -e "Sys.setenv(FPA_CONFIG='C:/FPA-Local/fpa-config.json'); setwd('C:/FPA-Local'); source('fpa-engine.R'); fpa_run('fpa-config.json'); fpa_app('fpa-config.json')"
```

The screen also uses `plotly` when it is installed. Without it, the same charts stay in base R.

Called directly, the two steps are:

```r
source("R/fpa-paths.R"); source("R/fpa-config.R"); fpa_source_all()
fpa_init("C:/FPA-Local/fpa-config.json")   # folders + model tables
fpa_run("C:/FPA-Local/fpa-config.json")    # three statements
```

`fpa_init()` never overwrites a file the customer has already edited unless `overwrite = TRUE`.

## Run the engine

```powershell
cd path\to\opensourcefpa-engine
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" -e "source('tests/helpers.R'); fpa_test_bootstrap(); fpa_run(fpa_ensure_fixtures())"
```

## Run gates 1–9 + init

```powershell
cd path\to\opensourcefpa-engine
& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe" tests/run-all.R
```

`tests/test-init.R` also asserts that `fpa_init()` produces no `.bat`, `.exe`, `.cmd`, `.ps1`, `.vbs`, or `.msi` anywhere under the working root.

## What L1/L2 covers

- Load `fpa-config.json` (website wizard contract)
- `fpa_init()`: create the working folder and model tables from that JSON
- Read P&L, opening BS, budget, parametres, decisions, volumes
- Plain-French validation (missing file, month gap, sign flip, BS balance)
- Monthly P&L through NI, DSO/DPO/DIO WC, indirect cash bridge, rolling BS
- No plug: cash from the bridge; equity rolls with NI; `A = L + E` asserted

## First engineering rule

No paid / self-service ship until monthly three statements articulate and acceptance gates 1–17 are green. Gates 1–9 are implemented. Horizon, variance, and freeze have their own checks. Gates 10, 12, 13, and 15 are started in `fpa-freeze.R` and are not the full ship bar yet. Sugar cash/BS ideas may bootstrap development only.
