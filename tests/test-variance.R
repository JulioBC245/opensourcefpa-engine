# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Variance keeps an unexplained residual. Decomposition only with both prices and quantities.
# Wattig: materiality floor, sign convention (unfavourable is negative), headline fallback.
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()

pnl <- data.frame(
  periode = c("2024-01", "2024-02"),
  REV = c(100, 130),
  stringsAsFactors = FALSE
)
budget <- data.frame(
  periode = c("2024-01", "2024-02"),
  functional_label = "REV",
  montant = c(80, 100),
  stringsAsFactors = FALSE
)
plain <- fpa_variance_build(pnl, budget, NULL, list(mechanisms = list()))
stopifnot(nrow(plain) == 2L)
stopifnot(all(abs(plain$unexplained - plain$variance) < 1e-9))
stopifnot(all(is.na(plain$price)))

volumes <- data.frame(
  periode = "2024-02",
  nuitees_vendues = 10, tarif = 13,
  nuitees_budget = 8, tarif_budget = 12.5,
  stringsAsFactors = FALSE
)
cfg <- list(mechanisms = list(capacity_x_util_x_rate = TRUE))
split <- fpa_variance_decompose(plain, volumes, cfg)
row <- split[split$periode == "2024-02", , drop = FALSE]
# price = 8 * (13 - 12.5) = 4; volume = 12.5 * 2 = 25; mix = 2 * 0.5 = 1; sum = 30 = variance
stopifnot(abs(row$price - 4) < 1e-9)
stopifnot(abs(row$volume - 25) < 1e-9)
stopifnot(abs(row$mix - 1) < 1e-9)
stopifnot(abs(row$unexplained) < 1e-9)

# Sign convention: unfavourable revenue (shortfall) is negative.
stopifnot(fpa_variance_signed("REV", -500) < 0)
# Unfavourable expense (overrun: actual - budget > 0) is negative after sign.
stopifnot(fpa_variance_signed("PAYROLL", 500) < 0)
# Favourable expense (underspend) is positive.
stopifnot(fpa_variance_signed("PAYROLL", -500) > 0)

# Materiality: sub-floor rows do not headline.
tiny <- data.frame(
  periode = "2024-01", line_id = "REV",
  actual = 100, budget = 90, variance = 50,
  variance_pct = 50 / 90,
  price = NA_real_, volume = NA_real_, mix = NA_real_,
  rate = NA_real_, headcount = NA_real_, unexplained = 50,
  stringsAsFactors = FALSE
)
stopifnot(identical(fpa_variance_headline(tiny, "REV", abs_floor = 1000), fpa_txt("var_none")))
stopifnot(nrow(fpa_variance_findings(tiny, abs_floor = 1000)) == 0L)

# Above floor: headline names the month and is not the empty fallback.
big <- tiny
big$variance <- 5000
big$unexplained <- 5000
head_txt <- fpa_variance_headline(big, "REV", abs_floor = 1000)
stopifnot(!identical(head_txt, fpa_txt("var_none")))
stopifnot(grepl("2024-01", head_txt, fixed = TRUE))
stopifnot(nrow(fpa_variance_findings(big, abs_floor = 1000)) == 1L)

# Sign note is stated once.
stopifnot(nzchar(fpa_variance_sign_note()))

# Compare headline reuses the same voice and respects the floor.
cmp_tiny <- data.frame(periode = "2024-01", oi_a = 10, oi_b = 20, delta_oi = 50, stringsAsFactors = FALSE)
stopifnot(identical(fpa_compare_headline(cmp_tiny, abs_floor = 1000), fpa_txt("close_headline_none")))
cmp_big <- data.frame(periode = "2024-03", oi_a = 10, oi_b = 5010, delta_oi = 5000, stringsAsFactors = FALSE)
cmp_txt <- fpa_compare_headline(cmp_big, abs_floor = 1000)
stopifnot(grepl("2024-03", cmp_txt, fixed = TRUE))

status <- data.frame(
  periode = c("2024-01", "2024-02", "2024-11", "2024-12"),
  status = c("actual", "actual", "forecast", "forecast"),
  stringsAsFactors = FALSE
)
rest_df <- data.frame(
  periode = c("2024-01", "2024-02", "2024-11", "2024-12"),
  line_id = "REV",
  variance = c(5000, 2000, 10, -8),
  stringsAsFactors = FALSE
)
flat <- fpa_variance_rest_of_year(rest_df, "REV", status, abs_floor = 1000)
stopifnot(grepl("2024-11", flat, fixed = TRUE))
stopifnot(grepl("sous le seuil", flat, fixed = TRUE))
rest_df$variance <- c(5000, 2000, 4000, -500)
gap <- fpa_variance_rest_of_year(rest_df, "REV", status, abs_floor = 1000)
stopifnot(grepl("2024-11", gap, fixed = TRUE))
stopifnot(!grepl("sous le seuil", gap, fixed = TRUE))

message("PASS variance")
