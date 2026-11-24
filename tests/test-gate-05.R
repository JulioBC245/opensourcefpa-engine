# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Gate 5 — +10 DSO: REV/NI unchanged; AR up; operating cash down ≈ Rev/365×10
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)

base <- fpa_run(cfg_path, write = FALSE, stop_on_error = TRUE)

# Patch parametres: dso += 10
pars <- utils::read.csv(file.path(root, "data", "fixtures", "input", "parametres.csv"),
                        stringsAsFactors = FALSE, fileEncoding = "UTF-8")
pars$valeur[pars$parametre == "dso"] <- pars$valeur[pars$parametre == "dso"] + 10
par_file <- file.path(root, "work", "parametres-dso10.csv")
utils::write.csv(pars, par_file, row.names = FALSE)
cfg <- fpa_load_config(cfg_path)
cfg$paths$assumptions <- par_file
tmp <- tempfile(fileext = ".json")
jsonlite::write_json(cfg, tmp, auto_unbox = TRUE)
alt <- fpa_run(tmp, write = FALSE, stop_on_error = TRUE)

# Compare mid year month
m <- "2024-06"
b_i <- match(m, base$pnl$periode)
a_i <- match(m, alt$pnl$periode)
stopifnot(abs(base$pnl$REV[[b_i]] - alt$pnl$REV[[a_i]]) < 1e-6)
stopifnot(abs(base$pnl$NI[[b_i]] - alt$pnl$NI[[a_i]]) < 1e-6)

ar_up <- alt$wc$AR[[m]] - base$wc$AR[[m]]
expected_ar <- base$pnl$REV[[b_i]] * 10 / fpa_wc_month_days(m)
stopifnot(abs(ar_up - expected_ar) < 0.05)

# Operating cash: telescoping WC means cum CFO drop through month m ≈ AR uplift
idx_b <- seq_len(b_i)
idx_a <- seq_len(a_i)
cfo_drop <- sum(base$cash$CFO[idx_b]) - sum(alt$cash$CFO[idx_a])
stopifnot(abs(cfo_drop - expected_ar) < 0.05)

message("PASS gate 5")
message(sprintf("  AR up=%.2f expected=%.2f cum CFO drop=%.2f", ar_up, expected_ar, cfo_drop))
