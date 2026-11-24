# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Gate 8 — Draw 180k: cash, debt, financing CF correct
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)

dec <- data.frame(
  date = as.Date("2024-05-01"),
  libelle = "Tirage credit",
  montant = 180000,
  type = "dette_tirage",
  life_months = NA_real_,
  interest_rate = 0.04,
  break_cost = 0,
  stringsAsFactors = FALSE
)
dec_file <- file.path(root, "work", "decisions-draw.csv")
utils::write.csv(dec, dec_file, row.names = FALSE)
cfg <- fpa_load_config(cfg_path)
cfg$paths$decisions <- dec_file
tmp <- tempfile(fileext = ".json")
jsonlite::write_json(cfg, tmp, auto_unbox = TRUE)

base <- fpa_run(cfg_path, write = FALSE, stop_on_error = TRUE)
alt <- fpa_run(tmp, write = FALSE, stop_on_error = TRUE)

m <- "2024-05"
ib <- match(m, base$bs$periode)
ia <- match(m, alt$bs$periode)
stopifnot(abs((alt$bs$CASH[[ia]] - base$bs$CASH[[ib]]) - 180000) < 1)
stopifnot(abs((alt$bs$LT_DEBT[[ia]] - base$bs$LT_DEBT[[ib]]) - 180000) < 1)
stopifnot(abs(alt$cash$CFF[[ia]] - 180000) < 1)
# Debt never negative
stopifnot(all(alt$bs$LT_DEBT >= -1e-6), all(alt$bs$ST_DEBT >= -1e-6))

message("PASS gate 8")
