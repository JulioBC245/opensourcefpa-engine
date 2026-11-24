# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Gate 7 — Capex 180k, life 84, D&A from next month
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)

dec <- data.frame(
  date = as.Date("2024-03-15"),
  libelle = "Machine CNC",
  montant = 180000,
  type = "capex",
  life_months = 84,
  interest_rate = NA_real_,
  break_cost = 0,
  stringsAsFactors = FALSE
)
dec_file <- file.path(root, "work", "decisions-capex.csv")
utils::write.csv(dec, dec_file, row.names = FALSE)

cfg <- fpa_load_config(cfg_path)
cfg$paths$decisions <- dec_file
tmp <- tempfile(fileext = ".json")
jsonlite::write_json(cfg, tmp, auto_unbox = TRUE)

base_cfg <- fpa_ensure_fixtures(root)
base <- fpa_run(base_cfg, write = FALSE, stop_on_error = TRUE)
alt <- fpa_run(tmp, write = FALSE, stop_on_error = TRUE)

# March 2024: cash down 180k vs base, PPE up 180k, no extra D&A yet
m_buy <- "2024-03"
m_next <- "2024-04"
ib <- match(m_buy, base$pnl$periode)
ia <- match(m_buy, alt$pnl$periode)
stopifnot(abs((base$cash$cash_close[[ib]] - alt$cash$cash_close[[ia]]) - 180000) < 1)
stopifnot(abs((alt$bs$PPE[[ia]] - base$bs$PPE[[ib]]) - 180000) < 1)
stopifnot(abs(alt$pnl$D_A[[ia]] - base$pnl$D_A[[ib]]) < 1e-6)

# April: D&A += 180000/84
da_extra <- 180000 / 84
ib2 <- match(m_next, base$pnl$periode)
ia2 <- match(m_next, alt$pnl$periode)
stopifnot(abs((alt$pnl$D_A[[ia2]] - base$pnl$D_A[[ib2]]) - da_extra) < 0.01)
# Investing CF in buy month = -180000
stopifnot(abs(alt$cash$CFI[[ia]] + 180000) < 1)

message("PASS gate 7")
