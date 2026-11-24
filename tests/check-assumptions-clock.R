# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Rates clock: assumptions_table and parametres.csv, later stamp wins.
# Run from fpa-local: Rscript tests/check-assumptions-clock.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}

fresh <- function() {
  root <- file.path(tempdir(), paste0("fpa-rates-", as.integer(runif(1, 1, 1e9))))
  dir.create(file.path(root, "input"), recursive = TRUE, showWarnings = FALSE)
  csv <- file.path(root, "input", "parametres.csv")
  cfg_path <- file.path(root, "fpa-config.json")
  list(root = root, csv = csv, cfg = cfg_path)
}

write_cfg <- function(box, table = NULL, created = "2099-01-01T00:00:00+00:00") {
  cfg <- list(
    config_version = "1.0.0",
    engine_min_version = "0.1.0",
    created_at = created,
    language = "fr",
    paths = list(
      working_root_hint = box$root,
      assumptions = box$csv
    ),
    assumption_keys = list(
      list(id = "taux.impot", enabled = TRUE, meaning = "tax"),
      list(id = "dso", enabled = TRUE, meaning = "dso"),
      list(id = "retired", enabled = FALSE, meaning = "off")
    )
  )
  if (!is.null(table)) cfg$assumptions_table <- table
  jsonlite::write_json(cfg, box$cfg, auto_unbox = TRUE, pretty = TRUE, null = "null")
  box
}

write_csv <- function(path, rows, stamp = "") {
  fpa_assumptions_write_csv(path, rows, stamp)
}

row <- function(periode, parametre, valeur) list(periode = periode, parametre = parametre, valeur = valeur)

vals <- function(path, id) {
  got <- fpa_assumptions_read_lines(path)
  hit <- Filter(function(r) identical(r$parametre, id), got$rows)
  if (!length(hit)) return(NA_real_)
  hit[[1]]$valeur
}

# JSON newer than the CSV. The CSV takes the JSON number. Stamps meet.
box <- fresh()
write_csv(box$csv, list(row("2026-01", "taux.impot", 0.15), row("2026-01", "dso", 12)), "2026-10-01T10:00:00+02:00")
write_cfg(box, list(
  written_at = "2026-10-08T15:35:00+02:00",
  rows = list(row("2026-01", "taux.impot", 0.25), row("2026-01", "dso", 20))
))
a <- fpa_assumptions_reconcile_path(box$cfg)
check("json newer copies onto the csv", identical(a$action, "json_wins") && abs(vals(box$csv, "taux.impot") - 0.25) < 1e-9 && abs(vals(box$csv, "dso") - 20) < 1e-9)
cfg1 <- fpa_load_config(box$cfg)
check("json newer sets both stamps equal", identical(cfg1$assumptions_table$written_at, fpa_assumptions_csv_clock(box$csv)$stamp))
b <- fpa_assumptions_reconcile_path(box$cfg)
check("a second pass leaves equal stamps", identical(b$action, "equal"))

# CSV newer than the JSON.
box <- fresh()
write_csv(box$csv, list(row("2026-01", "taux.impot", 0.18), row("2026-01", "dso", 30)), "2026-10-08T18:00:00+02:00")
write_cfg(box, list(
  written_at = "2026-10-01T08:00:00+02:00",
  rows = list(row("2026-01", "taux.impot", 0.10), row("2026-01", "dso", 5))
))
c <- fpa_assumptions_reconcile_path(box$cfg)
cfg2 <- fpa_load_config(box$cfg)
check("csv newer copies onto the json", identical(c$action, "csv_wins") && abs(cfg2$assumptions_table$rows[[1]]$valeur - 0.18) < 1e-9)
check("csv newer keeps the csv number", abs(vals(box$csv, "dso") - 30) < 1e-9)
check("csv newer sets both stamps equal", identical(cfg2$assumptions_table$written_at, "2026-10-08T18:00:00+02:00"))

# A blank JSON valeur must not erase a CSV number. created_at is not the clock.
box <- fresh()
write_csv(box$csv, list(row("2026-01", "taux.impot", 0.15), row("2026-01", "dso", 12)), "2026-10-01T10:00:00+02:00")
write_cfg(box, list(
  written_at = "2026-10-08T15:35:00+02:00",
  rows = list(
    list(periode = "2026-01", parametre = "taux.impot", valeur = 0.22),
    list(periode = "2026-01", parametre = "dso", valeur = "")
  )
), created = "2099-12-31T00:00:00+00:00")
fpa_assumptions_reconcile_path(box$cfg)
check("blank json valeur keeps the csv dso", abs(vals(box$csv, "dso") - 12) < 1e-9)
check("a numbered json valeur still wins", abs(vals(box$csv, "taux.impot") - 0.22) < 1e-9)
cfg3 <- fpa_load_config(box$cfg)
check("created_at is not the rates clock", identical(cfg3$assumptions_table$written_at, "2026-10-08T15:35:00+02:00"))

# JSON filled, CSV missing.
box <- fresh()
write_cfg(box, list(
  written_at = "2026-10-08T12:00:00+02:00",
  rows = list(row("2026-01", "dso", 14))
))
d <- fpa_assumptions_reconcile_path(box$cfg)
check("missing csv is created from the json", identical(d$action, "json_to_csv") && file.exists(box$csv) && abs(vals(box$csv, "dso") - 14) < 1e-9)
check("created csv carries the json stamp", identical(fpa_assumptions_csv_clock(box$csv)$stamp, "2026-10-08T12:00:00+02:00"))

# CSV filled, JSON block missing. created_at stays out of it.
box <- fresh()
write_csv(box$csv, list(row("2026-01", "dso", 12)), "2026-09-01T09:00:00+02:00")
write_cfg(box, NULL, created = "2099-06-01T00:00:00+00:00")
e <- fpa_assumptions_reconcile_path(box$cfg)
cfg4 <- fpa_load_config(box$cfg)
check("csv fills a missing json block", identical(e$action, "csv_to_json") && abs(cfg4$assumptions_table$rows[[1]]$valeur - 12) < 1e-9)
check("missing block uses the csv stamp", identical(cfg4$assumptions_table$written_at, "2026-09-01T09:00:00+02:00") && identical(cfg4$created_at, "2099-06-01T00:00:00+00:00"))

# Neither side has numbers.
box <- fresh()
write_cfg(box, NULL)
f <- fpa_assumptions_reconcile_path(box$cfg)
txt <- readLines(box$csv, warn = FALSE, encoding = "UTF-8")
check("neither side writes the header only", identical(f$action, "header") && any(grepl("periode", txt)) && !any(grepl("taux", txt)))
cfg5 <- fpa_load_config(box$cfg)
check("neither side invents a json block", is.null(cfg5$assumptions_table))

# Reader skips the clock and still carries a rate forward, enabled ids only.
box <- fresh()
write_csv(box$csv, list(
  row("2026-01", "dso", 12),
  row("2026-01", "retired", 99),
  row("2026-02", "taux.impot", 0.15)
), "2026-10-08T12:00:00+02:00")
write_cfg(box, NULL)
config <- fpa_load_config(box$cfg)
paths <- fpa_paths(config)
got <- fpa_read_assumptions(config, paths)
check("the clock comment is not a row", isTRUE(got$ok) && !any(grepl("written_at", got$data$parametre)))
check("disabled ids stay out", !("retired" %in% got$data$parametre))
check("the last rate carries forward", abs(got$wide$dso[got$wide$periode == "2026-02"] - 12) < 1e-9)

# A screen edit stamps both sides to the same moment.
box <- fresh()
write_csv(box$csv, list(row("2026-01", "dso", 12)), "2026-01-01T00:00:00+00:00")
write_cfg(box, list(written_at = "2026-01-01T00:00:00+00:00", rows = list(row("2026-01", "dso", 12))))
saved <- fpa_assumptions_save_edit(box$cfg, list(row("2026-01", "dso", 16)))
cfg6 <- fpa_load_config(box$cfg)
check("a screen edit writes the csv and the json", abs(vals(box$csv, "dso") - 16) < 1e-9 && abs(cfg6$assumptions_table$rows[[1]]$valeur - 16) < 1e-9)
check("a screen edit sets both stamps to the same value", identical(saved$stamp, cfg6$assumptions_table$written_at) && identical(saved$stamp, fpa_assumptions_csv_clock(box$csv)$stamp))

cat("PASS", pass, "FAIL", fail, "\n")
if (fail > 0L) quit(status = 1)
