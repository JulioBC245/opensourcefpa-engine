# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Gate 2 — Validation: month gap, sign flip, unmatched budget, duplicate month
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)
cfg <- fpa_load_config(cfg_path)
paths <- fpa_paths(cfg)

# --- month gap ---
pnl <- utils::read.csv(file.path(root, "data", "fixtures", "input", "pnl.csv"),
                       stringsAsFactors = FALSE, fileEncoding = "UTF-8")
pnl_gap <- pnl[pnl$periode != "2024-06", , drop = FALSE]
gap_file <- file.path(root, "work", "pnl-gap.csv")
utils::write.csv(pnl_gap, gap_file, row.names = FALSE)
cfg2 <- cfg
cfg2$paths$pnl_actuals <- gap_file
tmp <- tempfile(fileext = ".json")
jsonlite::write_json(cfg2, tmp, auto_unbox = TRUE)
reads <- fpa_read_all_financial(cfg2, paths)
findings <- fpa_validate(cfg2, reads, fpa_mapped_to_bs_vec(reads$opening_bs$mapped))
stopifnot(any(findings$code == "month_gap"))

# --- sign flip ---
pnl_sign <- pnl
pnl_sign$montant[pnl_sign$libelle == "Achats" & pnl_sign$periode == "2024-01"] <- -35000
sign_file <- file.path(root, "work", "pnl-sign.csv")
utils::write.csv(pnl_sign, sign_file, row.names = FALSE)
cfg3 <- cfg
cfg3$paths$pnl_actuals <- sign_file
jsonlite::write_json(cfg3, tmp, auto_unbox = TRUE)
reads3 <- fpa_read_all_financial(cfg3, paths)
findings3 <- fpa_validate(cfg3, reads3, fpa_mapped_to_bs_vec(reads3$opening_bs$mapped))
stopifnot(any(findings3$code == "sign_flip"))

# --- duplicate month ---
pnl_dup <- rbind(pnl, pnl[pnl$periode == "2024-01" & pnl$libelle == "Chiffre d'affaires", ])
dup_file <- file.path(root, "work", "pnl-dup.csv")
utils::write.csv(pnl_dup, dup_file, row.names = FALSE)
cfg4 <- cfg
cfg4$paths$pnl_actuals <- dup_file
jsonlite::write_json(cfg4, tmp, auto_unbox = TRUE)
reads4 <- fpa_read_all_financial(cfg4, paths)
# After map+aggregate duplicates collapse — inject duplicate into mapped directly
m <- reads4$pnl_actuals$mapped
m <- rbind(m, m[1, , drop = FALSE])
reads4$pnl_actuals$mapped <- m
findings4 <- fpa_validate(cfg4, reads4, NULL)
stopifnot(any(findings4$code == "duplicate_month"))

# --- unmatched budget line ---
bud <- utils::read.csv(file.path(root, "data", "fixtures", "input", "budget.csv"),
                       stringsAsFactors = FALSE, fileEncoding = "UTF-8")
bud <- rbind(bud, data.frame(libelle = "Poste inconnu XYZ", periode = "2024-01", montant = 1,
                             stringsAsFactors = FALSE))
bud_file <- file.path(root, "work", "budget-unmatched.csv")
utils::write.csv(bud, bud_file, row.names = FALSE)
cfg5 <- cfg
cfg5$paths$budget <- bud_file
jsonlite::write_json(cfg5, tmp, auto_unbox = TRUE)
reads5 <- fpa_read_all_financial(cfg5, paths)
findings5 <- fpa_validate(cfg5, reads5, NULL)
stopifnot(any(findings5$code == "unmatched_budget"))

message("PASS gate 2")
