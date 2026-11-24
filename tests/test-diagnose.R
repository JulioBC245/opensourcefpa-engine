# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Pre-flight diagnostic: files vs JSON, no R traceback on blocking issues.
# LAST CHANGE: 2026-09-19
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)
cfg <- fpa_load_config(cfg_path)

ok <- fpa_diagnose(cfg)
stopifnot(!isTRUE(ok$blocking))
stopifnot(ok$n_error == 0)

# --- liasse mapped as monthly long ---
tmp <- tempfile(pattern = "fpa-diag-")
dir.create(tmp)
pnl <- data.frame(
  `Poste comptable` = c("Chiffre d'affaires net", "Achats"),
  `Montant N (€)` = c(100, 40),
  `Montant N-1 (€)` = c(90, 35),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
utils::write.csv(pnl, file.path(tmp, "pnl.csv"), row.names = FALSE, fileEncoding = "UTF-8")
bs <- data.frame(
  Poste = c(
    "Immobilisations", "Creances clients", "Tresorerie", "Total Actif",
    "Capital social", "Reserves", "Resultat reporte (benefice)", "Dettes", "Total Passif"
  ),
  Montant = c(100000, 20000, 10000, 130000, 50000, 20000, 10000, 50000, 130000),
  stringsAsFactors = FALSE
)
utils::write.csv(bs, file.path(tmp, "bs.csv"), row.names = FALSE, fileEncoding = "UTF-8")
budget <- data.frame(
  `Poste comptable` = "Chiffre d'affaires net",
  `Montant (€)` = 120000,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
utils::write.csv(budget, file.path(tmp, "budget.csv"), row.names = FALSE, fileEncoding = "UTF-8")

cfg2 <- cfg
cfg2$paths$working_root_hint <- tmp
cfg2$paths$pnl_actuals <- file.path(tmp, "pnl.csv")
cfg2$paths$opening_bs <- file.path(tmp, "bs.csv")
cfg2$paths$budget <- file.path(tmp, "budget.csv")
cfg2$sources <- list(
  list(
    file_role = "pnl_actuals",
    file_layout = "long",
    column_map = list(
      "Poste comptable" = "libelle",
      "Montant N (€)" = "periode",
      "Montant N-1 (€)" = "periode"
    ),
    column_period = list()
  ),
  list(
    file_role = "opening_bs",
    file_layout = "long",
    column_map = list(Poste = "libelle", Montant = "montant")
  ),
  list(
    file_role = "budget",
    file_layout = "long",
    column_map = list(
      "Poste comptable" = "libelle",
      "Montant (€)" = "periode"
    )
  )
)
cfg2$line_map_bs <- list(
  Immobilisations = "PPE",
  "Creances clients" = "AR",
  Tresorerie = "CASH",
  "Total Actif" = "SUBTOTAL",
  "Capital social" = "EQUITY",
  Reserves = "EQUITY",
  "Resultat reporte (benefice)" = "SUBTOTAL",
  Dettes = "SECTION",
  "Total Passif" = "SUBTOTAL"
)
json_path <- file.path(tmp, "fpa-config.json")
jsonlite::write_json(cfg2, json_path, auto_unbox = TRUE)
cfg3 <- fpa_load_config(json_path)
rep <- fpa_diagnose(cfg3)
stopifnot(isTRUE(rep$blocking))
codes <- vapply(rep$items, function(it) it$code, character(1))
stopifnot("liasse_as_long" %in% codes)
stopifnot("bs_map_drops_amounts" %in% codes)
out <- paste(capture.output(fpa_diagnose_print(rep, json_path, tmp)), collapse = "\n")
stopifnot(grepl("BLOQUANT", out))
stopifnot(grepl("compte de resultat annuel", out, ignore.case = TRUE))
stopifnot(grepl("ASSISTANT", out))
stopifnot(grepl("4.3", out, fixed = TRUE))
stopifnot(grepl("Calcul non lance", out, fixed = TRUE))
stopifnot(!grepl("Erreur :", out, fixed = TRUE))

# --- Excel itself unbalanced ---
bs_bad <- bs
bs_bad$Montant[bs_bad$Poste == "Total Passif"] <- 100000
utils::write.csv(bs_bad, file.path(tmp, "bs-bad.csv"), row.names = FALSE, fileEncoding = "UTF-8")
cfg3$paths$opening_bs <- file.path(tmp, "bs-bad.csv")
rep2 <- fpa_diagnose(cfg3)
codes2 <- vapply(rep2$items, function(it) it$code, character(1))
stopifnot("bs_excel_unbalanced" %in% codes2)
where_excel <- vapply(
  Filter(function(it) identical(it$code, "bs_excel_unbalanced"), rep2$items),
  function(it) it$where,
  character(1)
)
stopifnot(identical(where_excel, "excel"))

message("test-diagnose OK")
