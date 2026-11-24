# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Grades: horizon badge, verdicts, stored lag profile redraws without a new search.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

stopifnot(fpa_driver_usable_horizon(1, 90) < 0)
stopifnot(identical(fpa_driver_class(fpa_driver_usable_horizon(1, 90), "suggested"), "explanatory"))
stopifnot(fpa_driver_usable_horizon(2, 20) > 0)
stopifnot(identical(fpa_driver_class(fpa_driver_usable_horizon(2, 20), "suggested"), "forecasting"))
stopifnot(identical(fpa_driver_class(30, "refused_reverse_causality"), "explanatory"))

stopifnot(identical(
  fpa_driver_verdict(36, TRUE, TRUE, "measured", "suggested",
                     "statistically_validated", "forecasting", "independent"),
  "Keep"
))
stopifnot(identical(
  fpa_driver_verdict(36, TRUE, FALSE, "measured", "suggested",
                     "insufficient_evidence", "forecasting", "independent"),
  "Optional"
))
stopifnot(identical(
  fpa_driver_verdict(12, TRUE, TRUE, "measured", "suggested",
                     "insufficient_evidence", "forecasting", "independent"),
  "Needs more history"
))
stopifnot(identical(
  fpa_driver_verdict(36, TRUE, TRUE, "measured", "refused_reverse_causality",
                     "insufficient_evidence", "explanatory", "independent"),
  "Reject"
))
stopifnot(identical(
  fpa_driver_evidence_class("suspect_identity", 36, 0.01, 0.4, "suggested", TRUE),
  "mechanically_calculated"
))
stopifnot(identical(
  fpa_driver_evidence_class("independent", 36, 0.8, NA, "not_distinguishable", FALSE),
  "insufficient_evidence"
))
stopifnot(grepl("36 mois", fpa_driver_data_budget(36)))

# A stored profile is enough to name the peak. No permutation in this step.
prof <- data.frame(
  lag = c(-1L, 0L, 1L),
  r = c(0.2, 0.1, 0.7),
  null_p95 = c(0.35, 0.35, 0.35),
  stringsAsFactors = FALSE
)
path <- file.path(tempdir(), "lag-profile-redraw.csv")
fpa_driver_write_csv(prof, path)
back <- utils::read.csv(path, stringsAsFactors = FALSE)
peak <- fpa_driver_profile_peak(back)
stopifnot(identical(peak$lag, 1L))
stopifnot(is.finite(back$null_p95[[1]]))

impact <- fpa_driver_impact("no-such-series", list())
stopifnot(identical(impact$status, "not_available"))
stopifnot(is.na(impact$eur))

message("PASS driver-grade")
