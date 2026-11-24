# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Init — engine rebuilds folders and model tables from the JSON alone (no archive)
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)

target <- file.path(tempdir(), paste0("fpa-init-", as.integer(runif(1, 1, 1e6))))
dir.create(target, recursive = TRUE, showWarnings = FALSE)

# Config carrying file_drafts, exactly as the website now emits it
cfg <- fpa_load_config(cfg_path)
cfg$file_drafts <- list(
  assumptions = list(
    filename = "parametres.csv",
    columns = list("periode", "parametre", "valeur"),
    rows = list(
      list("2026-01", "taux.impot", "0.25"),
      list("2026-01", "dso", "45"),
      list("2026-02", "taux.impot", "0.25")
    )
  ),
  decisions = list(
    filename = "investissements-emprunts.csv",
    columns = list("date", "libelle", "montant", "type"),
    rows = list(list("2026-03-15", "Machine, avec virgule", "48000", "capex"))
  )
)
tmp <- tempfile(fileext = ".json")
jsonlite::write_json(cfg, tmp, auto_unbox = TRUE)

res <- fpa_init(tmp, root = target)

stopifnot(dir.exists(file.path(target, "input", "modeles")))
stopifnot(dir.exists(file.path(target, "work")))
stopifnot(dir.exists(file.path(target, "reports")))

par_file <- file.path(target, "input", "modeles", "parametres.csv")
stopifnot(file.exists(par_file))
par_df <- utils::read.csv(par_file, stringsAsFactors = FALSE)
stopifnot(identical(names(par_df), c("periode", "parametre", "valeur")))
stopifnot(nrow(par_df) == 3)
stopifnot("taux.impot" %in% par_df$parametre)

dec_file <- file.path(target, "input", "modeles", "investissements-emprunts.csv")
stopifnot(file.exists(dec_file))
dec_df <- utils::read.csv(dec_file, stringsAsFactors = FALSE)
# Comma inside a cell must survive the CSV round trip
stopifnot(dec_df$libelle[[1]] == "Machine, avec virgule")
stopifnot(dec_df$type[[1]] == "capex")

stopifnot(file.exists(file.path(target, "input", "modeles", "LISEZMOI.txt")))

# No executable is ever produced by the engine init
all_files <- list.files(target, recursive = TRUE, full.names = FALSE)
stopifnot(!any(grepl("\\.(bat|exe|cmd|ps1|vbs|msi)$", all_files, ignore.case = TRUE)))

# Second run keeps user edits
writeLines("periode,parametre,valeur\n2026-01,taux.impot,0.33", par_file)
res2 <- fpa_init(tmp, root = target)
stopifnot(length(res2$skipped) >= 1)
kept <- utils::read.csv(par_file, stringsAsFactors = FALSE)
stopifnot(abs(kept$valeur[[1]] - 0.33) < 1e-9)

unlink(target, recursive = TRUE)
message("PASS init")
