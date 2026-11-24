# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Catch-up entry: last two dates, one missing month, append-only CSV write.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

today <- as.Date("2026-09-22")
stopifnot(identical(format(fpa_catchup_needed("rolling", today), "%Y-%m"), "2026-08"))
stopifnot(identical(format(fpa_catchup_needed("plan", today), "%Y-%m"), "2026-12"))

dir <- tempfile("catchup")
dir.create(dir)
long_path <- file.path(dir, "rates.csv")
writeLines(c(
  "\"periode\",\"parametre\",\"valeur\"",
  "\"2025-12\",\"taux.impot\",0.14",
  "\"2026-01\",\"taux.impot\",0.15",
  "\"2025-12\",\"dso\",10",
  "\"2026-01\",\"dso\",12"
), long_path, useBytes = TRUE)
before <- readLines(long_path, warn = FALSE)

cfg <- list(
  paths = list(assumptions = long_path),
  sources = list(list(
    source_id = "src-assumptions",
    source_name = "Taux",
    file_role = "assumptions",
    time_frame = "rolling"
  ))
)
sheet <- fpa_catchup_preview(cfg, "src:src-assumptions", today)
stopifnot(isTRUE(sheet$ok))
stopifnot(identical(sheet$needed, "2026-08"))
stopifnot(identical(sheet$hist, c("2025-12", "2026-01")))
stopifnot(identical(sheet$rows$label, c("dso", "taux.impot")) || identical(sort(sheet$rows$label), c("dso", "taux.impot")))
taux <- sheet$rows[sheet$rows$label == "taux.impot", , drop = FALSE]
stopifnot(identical(taux$h1, "0.14"))
stopifnot(identical(taux$h2, "0.15"))

vals <- stats::setNames(rep("", nrow(sheet$rows)), sheet$rows$id)
vals[[taux$id]] <- "0,16"
saved <- fpa_catchup_save(sheet, vals)
stopifnot(isTRUE(saved$ok))
stopifnot(identical(saved$n, 1L))
after <- readLines(long_path, warn = FALSE)
stopifnot(identical(head(after, length(before)), before))
stopifnot(any(grepl("2026-08", after, fixed = TRUE)))
stopifnot(any(grepl("0.16", after, fixed = TRUE)))
again <- fpa_catchup_save(sheet, vals)
stopifnot(identical(again$n, 0L))
stopifnot(identical(readLines(long_path, warn = FALSE), after))

wide_path <- file.path(dir, "wide.csv")
writeLines(c(
  "periode,capacite,tarif",
  "2026-06,10,1.5",
  "2026-07,11,1.6"
), wide_path, useBytes = TRUE)
wide_before <- readLines(wide_path, warn = FALSE)
wcfg <- list(
  paths = list(ops_other = wide_path),
  sources = list(list(
    source_id = "src-ops",
    source_name = "Volumes",
    file_role = "ops_other",
    time_frame = "rolling"
  ))
)
wsheet <- fpa_catchup_preview(wcfg, "src-ops", today)
stopifnot(isTRUE(wsheet$ok))
stopifnot(identical(wsheet$layout, "wide"))
stopifnot(identical(wsheet$hist, c("2026-06", "2026-07")))
cap <- wsheet$rows[wsheet$rows$label == "capacite", , drop = FALSE]
stopifnot(identical(cap$h2, "11"))
wvals <- stats::setNames(rep("", nrow(wsheet$rows)), wsheet$rows$id)
wvals[[cap$id]] <- "12"
wsaved <- fpa_catchup_save(wsheet, wvals)
stopifnot(identical(wsaved$n, 1L))
wafter <- readLines(wide_path, warn = FALSE)
stopifnot(identical(head(wafter, length(wide_before)), wide_before))
stopifnot(any(grepl("2026-08", wafter, fixed = TRUE)))

xlsx <- file.path(dir, "budget.xlsx")
writeLines("not a workbook", xlsx, useBytes = TRUE)
xcfg <- list(
  paths = list(budget = xlsx),
  sources = list(list(
    source_id = "src-budget",
    source_name = "Budget",
    file_role = "budget",
    time_frame = "plan"
  ))
)
xsheet <- fpa_catchup_preview(xcfg, "src-budget", today)
stopifnot(!isTRUE(xsheet$ok))
stopifnot(identical(xsheet$reason, "excel"))
stopifnot(identical(readLines(xlsx, warn = FALSE), "not a workbook"))

empty <- fpa_catchup_save(sheet, stats::setNames(rep("", nrow(sheet$rows)), sheet$rows$id))
stopifnot(identical(empty$n, 0L))

xlsx_ok <- file.path(dir, "budget-real.xlsx")
openxlsx::write.xlsx(
  data.frame(
    Poste = c("Ventes", "Achats"),
    Montant = c(100, 40),
    stringsAsFactors = FALSE
  ),
  xlsx_ok, overwrite = TRUE
)
bcfg <- list(
  paths = list(budget = xlsx_ok),
  sources = list(list(
    source_id = "src-budget",
    source_name = "Budget",
    file_role = "budget",
    time_frame = "plan",
    periods = list(list(period_end = "2025-12-31"))
  ))
)
bsheet <- fpa_catchup_preview(bcfg, "src-budget", today)
stopifnot(isTRUE(bsheet$ok))
stopifnot(identical(bsheet$layout, "yearcol"))
stopifnot(identical(bsheet$needed, "2026-12"))
stopifnot(identical(bsheet$hist, "2025-12"))
ventes <- bsheet$rows[bsheet$rows$label == "Ventes", , drop = FALSE]
stopifnot(identical(ventes$h1, "100"))
bvals <- stats::setNames(rep("", nrow(bsheet$rows)), bsheet$rows$id)
bvals[[ventes$id]] <- "120"
bsaved <- fpa_catchup_save(bsheet, bvals)
stopifnot(identical(bsaved$n, 1L))
back <- as.data.frame(openxlsx::read.xlsx(xlsx_ok), stringsAsFactors = FALSE)
stopifnot(identical(as.character(back$Montant[[1]]), "100") || identical(back$Montant[[1]], 100))
stopifnot("2026" %in% names(back))
stopifnot(identical(back[["2026"]][[1]], 120))
stopifnot(is.na(back[["2026"]][[2]]))
dates <- fpa_chain_file_dates(xlsx_ok)
stopifnot(as.Date("2026-12-01") %in% dates)

cat("PASS catchup\n")
