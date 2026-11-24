# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Gate 9 — Refinance same month: net cash = difference ± break cost; no negative debt
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)

# Repay 80000 LT opening and redraw 100000 same month, break cost 2000
# Net cash = +100000 - 80000 - 2000 = +18000
dec <- data.frame(
  date = as.Date(c("2024-02-10", "2024-02-10")),
  libelle = c("Remboursement ancien", "Nouveau tirage"),
  montant = c(80000, 100000),
  type = c("dette_remboursement", "dette_tirage"),
  life_months = c(NA_real_, NA_real_),
  interest_rate = c(NA_real_, 0.035),
  break_cost = c(2000, 0),
  stringsAsFactors = FALSE
)
dec_file <- file.path(root, "work", "decisions-refi.csv")
utils::write.csv(dec, dec_file, row.names = FALSE)
cfg <- fpa_load_config(cfg_path)
cfg$paths$decisions <- dec_file
tmp <- tempfile(fileext = ".json")
jsonlite::write_json(cfg, tmp, auto_unbox = TRUE)

base <- fpa_run(cfg_path, write = FALSE, stop_on_error = TRUE)
alt <- fpa_run(tmp, write = FALSE, stop_on_error = TRUE)

m <- "2024-02"
ib <- match(m, base$bs$periode)
ia <- match(m, alt$bs$periode)
net <- 100000 - 80000 - 2000
stopifnot(abs((alt$bs$CASH[[ia]] - base$bs$CASH[[ib]]) - net) < 1)
stopifnot(abs(alt$cash$CFF[[ia]] - net) < 1)
stopifnot(all(alt$bs$LT_DEBT >= -1e-6), all(alt$bs$ST_DEBT >= -1e-6))
# Identity still holds
stopifnot(isTRUE(fpa_assert_identity(alt$bs)$ok))

message("PASS gate 9")
