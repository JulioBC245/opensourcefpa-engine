# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Gate 4 — Opening BS must balance or refuse with short side named
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)

# Balanced fixture runs
res <- fpa_run(cfg_path, write = FALSE, stop_on_error = TRUE)
bal <- fpa_bs_balance_check(res$opening)
stopifnot(isTRUE(bal$ok))

# Break opening BS
bs <- utils::read.csv(file.path(root, "data", "fixtures", "input", "bs-opening.csv"),
                      stringsAsFactors = FALSE, fileEncoding = "UTF-8")
bs$montant[bs$libelle == "Tresorerie"] <- bs$montant[bs$libelle == "Tresorerie"] + 10000
bad <- file.path(root, "work", "bs-unbalanced.csv")
utils::write.csv(bs, bad, row.names = FALSE)
cfg <- fpa_load_config(cfg_path)
cfg$paths$opening_bs <- bad
tmp <- tempfile(fileext = ".json")
jsonlite::write_json(cfg, tmp, auto_unbox = TRUE)
err <- tryCatch(fpa_run(tmp, write = FALSE, stop_on_error = TRUE), error = function(e) conditionMessage(e))
stopifnot(grepl("ne balance pas|passif|actif", err, ignore.case = TRUE))
stopifnot(grepl("Cote court", err))

message("PASS gate 4")
