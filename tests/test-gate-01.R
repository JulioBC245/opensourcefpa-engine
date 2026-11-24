# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Gate 1 — Declared files load; missing sheet named in plain French
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)

res <- fpa_run(cfg_path, write = TRUE, stop_on_error = TRUE)
stopifnot(!is.null(res$pnl), nrow(res$pnl) >= 12)
stopifnot(!is.null(res$bs), nrow(res$bs) >= 12)
stopifnot(!is.null(res$cash), nrow(res$cash) >= 12)
stopifnot(file.exists(file.path(root, "work", "pnl-monthly.csv")))

# Missing file → plain French, no cryptic traceback required
cfg <- fpa_load_config(cfg_path)
cfg$paths$pnl_actuals <- file.path(root, "data", "fixtures", "input", "DOES-NOT-EXIST.csv")
tmp <- tempfile(fileext = ".json")
jsonlite::write_json(cfg, tmp, auto_unbox = TRUE, pretty = TRUE)
err <- tryCatch(fpa_run(tmp, write = FALSE, stop_on_error = TRUE), error = function(e) conditionMessage(e))
stopifnot(is.character(err), grepl("introuvable|compte de resultat", err, ignore.case = TRUE))
stopifnot(!grepl("Error in |object '", err))

message("PASS gate 1")
