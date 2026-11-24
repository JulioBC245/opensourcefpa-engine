# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Plan key "Impots" must map the PCG caption; IS must stay TAX.
source("R/fpa-config.R", encoding = "UTF-8")
source("R/fpa-lines.R", encoding = "UTF-8")

m <- c(
  "Impôts" = "OPEX_OTHER",
  "Impôt sur les bénéfices (IS)" = "TAX",
  "Chiffre d'affaires net" = "REV"
)

stopifnot(identical(
  fpa_lookup_line("Impôts, taxes et versements assimilés", m),
  "OPEX_OTHER"
))
stopifnot(identical(fpa_lookup_line("Impôts", m), "OPEX_OTHER"))
stopifnot(identical(fpa_lookup_line("Impots", m), "OPEX_OTHER"))
stopifnot(identical(
  fpa_lookup_line("Impôt sur les bénéfices (IS)", m),
  "TAX"
))
stopifnot(identical(fpa_lookup_line("Chiffre d'affaires net", m), "REV"))
stopifnot(is.na(fpa_lookup_line("Poste inconnu XYZ", m)))
stopifnot(is.na(fpa_lookup_line("", m)))

mapped <- fpa_lookup_lines(
  c("Impôts, taxes et versements assimilés", "Poste inconnu XYZ"),
  m
)
stopifnot(identical(mapped[[1]], "OPEX_OTHER"), is.na(mapped[[2]]))

message("PASS line-lookup")
