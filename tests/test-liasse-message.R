# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Liasse files must not ask for a month column named periode.
# LAST CHANGE: 2026-09-19
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()

df_n <- data.frame(
  `Poste comptable` = c("Chiffre d'affaires net", "Achats"),
  `Montant N (€)` = c(100, 40),
  `Montant N-1 (€)` = c(90, 35),
  Explication = c("", ""),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
src_wrong <- list(
  file_layout = "long",
  column_map = list(
    "Poste comptable" = "libelle",
    "Montant N (€)" = "periode",
    "Montant N-1 (€)" = "periode"
  )
)
stopifnot(isTRUE(fpa_looks_like_liasse(df_n, src_wrong)))
msg_n <- fpa_read_schema_message(df_n, src_wrong, "montant")
stopifnot(grepl("une ligne par compte", msg_n, ignore.case = TRUE))
stopifnot(!grepl("Colonnes attendues manquantes", msg_n, fixed = TRUE))

df_budget <- data.frame(
  `Poste comptable` = "Chiffre d'affaires net",
  `Montant (€)` = 120000,
  Explication = "",
  check.names = FALSE,
  stringsAsFactors = FALSE
)
src_budget <- list(
  file_layout = "long",
  column_map = list(
    "Poste comptable" = "libelle",
    "Montant (€)" = "montant"
  )
)
stopifnot(isTRUE(fpa_looks_like_liasse(df_budget, src_budget)))
msg_pe <- fpa_read_schema_message(df_budget, src_budget, "periode")
stopifnot(grepl("une ligne par compte", msg_pe, ignore.case = TRUE))
stopifnot(!grepl("libelle, periode, montant", msg_pe, fixed = TRUE))

df_long <- data.frame(
  libelle = "Chiffre d'affaires",
  periode = "2024-01",
  montant = 1000,
  stringsAsFactors = FALSE
)
src_long <- list(
  file_layout = "long",
  column_map = list(libelle = "libelle", periode = "periode", montant = "montant")
)
stopifnot(!isTRUE(fpa_looks_like_liasse(df_long, src_long)))
msg_long <- fpa_read_schema_message(df_long, src_long, "periode")
stopifnot(identical(msg_long, "Colonnes attendues manquantes (libelle, periode, montant)."))

message("test-liasse-message OK")
