# Second-person monthly cycle

This is the guide for gate 16: a second person completes a monthly cycle with this note only, and no console.

The product still needs an R session once. That session opens the screen with one short call. After that, everything below is a click on the screen.

## Before the month

1. Install R from cran.r-project.org if it is not already there.
2. Once: `install.packages(c("jsonlite", "readxl", "openxlsx", "shiny", "bslib", "plotly"))`. `readxl` reads an Excel workbook. `openxlsx` writes into it. Include `openxlsx` when a path in the configuration is a workbook.
3. Keep three working folders under the company root: `input`, `work`, `reports`. The engine creates them on first run if they are missing.
4. Place `fpa-config.json` and `fpa-engine.R` in the company root. Place `app.R` beside them, or beside the engine source.
5. Hotels: set the sector to `hotellerie` in the wizard so steps 3 and 4 use the hotel pack.

## One-time Windows shortcut

You create this yourself. The product does not ship a `.bat` or an `.exe`.

1. Right-click the desktop → New → Shortcut.
2. Target (adjust the R version and the company root):

```text
"C:\Program Files\R\R-4.6.0\bin\Rscript.exe" -e "Sys.setenv(FPA_CONFIG='C:/FPA-Local/fpa-config.json'); setwd('C:/FPA-Local'); source('fpa-engine.R'); fpa_run('fpa-config.json'); fpa_app('fpa-config.json')"
```

3. Name it `FPA` or the company name. Start in: `C:\FPA-Local`.
4. After that, a double-click runs the month and opens the screen. Steps 3–8 of this guide stay clicks.

`app.R` must sit in `C:\FPA-Local` or beside the engine the moteur sources. If `fpa_app` says the file is missing, copy the text file `app.R` into that folder.

## Each month

1. Drop the new month into the P&L extract, the occupancy file, and the parameters file. Do not interpolate missing days.
2. Double-click the Windows shortcut, or in RStudio:

```r
source("fpa-engine.R")
fpa_run("fpa-config.json")
fpa_app("fpa-config.json")
```

3. On the screen, open **Synthese**. Check revenue, operating income, net income, and closing cash against last month.
4. Open **Compte de resultat**. Hover a cell. Imported actuals are black. Mechanical rules are teal. Management assumptions are amber. A forecast month must never show as imported.
5. Open **Bilan**. The gap must be zero. If it is not, stop and send the finding back.
6. Open **Ecarts**. Pick REV. Read the unexplained residual. Price, volume, and mix appear only when the occupancy file carries plan nights and plan rate.
7. Open **Cloture**.
   - Figer le budget once the board has approved it.
   - Enregistrer le vintage after the engine run.
   - Verrouiller les mois clos so a later run cannot rewrite them.
   - Exporter le resume from the stored vintage when you need a pack for management.
8. Open **Impact** only when you want a what-if. Actuals stay on disk. A scenario moves an assumption in the session. A driver shows euros only when `driver_bindings` names it and a forecast horizon exists.

## What this guide does not ask

- No batch file. No executable from the website.
- No typing of file paths after the shortcut exists.
- No console debugging. If the screen refuses a forecast line in French, that line needs a mechanism or a `prevision.<LINE>` assumption. Fix the files, re-run, reopen the screen.

## Honest gap

Gate 16 is green when a second person can finish steps 3–8 with this note only after the shortcut is on their machine. Until then, step 2 still needs one R call.
