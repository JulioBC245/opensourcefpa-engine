# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Score and product identity. An identity keeps its prior. A measured series mixes evidence and prior.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

stopifnot(abs(fpa_driver_score(0.80, 0.40, "independent", TRUE) - (0.75 * 0.80 + 0.25 * 0.40)) < 1e-9)
stopifnot(abs(fpa_driver_score(0.97, 0.85, "part_whole", TRUE) - 0.85) < 1e-9)
stopifnot(abs(fpa_driver_score(NA, 0.40, "independent", FALSE) - 0.14) < 1e-9)
stopifnot(abs(fpa_driver_prior_value(NA, "part_whole") - 0.85) < 1e-9)
stopifnot(abs(fpa_driver_prior_value(0.70, "part_whole") - 0.70) < 1e-9)

nuitees <- c(10, 20, 30, 40, 15, 25, 35, 12)
tarif <- c(100, 110, 90, 80, 120, 95, 105, 130)
rev <- nuitees * tarif
taux <- c(0.5, 0.4, 0.8, 0.2, 0.6, 0.3, 0.7, 0.9)
capacite <- nuitees / taux
monthly <- list(nuitees = nuitees, tarif = tarif, taux = taux, capacite = capacite, autre = rev + 5)
parts <- fpa_driver_product_ids(monthly, list(REV = rev))
stopifnot(all(c("nuitees", "tarif", "taux", "capacite") %in% parts))
stopifnot(!"autre" %in% parts)

g <- data.frame(
  series_id = c("a", "b", "c"),
  series_name = c("a", "b", "c"),
  target_line = c("REV", "REV", "PAYROLL"),
  evidence_r = c(0.8, 0.3, 0.9),
  evidence_sig = c(TRUE, TRUE, TRUE),
  relation = c("independent", "part_whole", "independent"),
  prior = c(0.4, NA, 0.4),
  lag_status = c("suggested", "suggested", "suggested"),
  redundancy_group = c("", "", ""),
  driver_class = c("forecasting", "explanatory", "forecasting"),
  stringsAsFactors = FALSE
)
out <- fpa_driver_assign_tiers(g, 36)
stopifnot(identical(as.character(out$tier[out$series_id == "b"]), "mechanism"))
stopifnot(abs(out$score[out$series_id == "b"] - 0.85) < 1e-9)
stopifnot(sum(out$tier == "confirmed") <= max(1L, as.integer(round(36 / 7))))
stopifnot("confirmed" %in% out$tier)

message("PASS driver-score")
