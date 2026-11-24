# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Profile: column class, grain disagreement, proposed target, identity, part-whole.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

dates <- format(seq(as.Date("2024-01-01"), by = "month", length.out = 12), "%Y-%m")
df <- data.frame(
  periode = dates,
  temperature_c = c(4, 5, 8, 12, 16, 20, 24, 23, 18, 13, 8, 5),
  commentaire = rep("note", 12),
  gel = rep(1, 12),
  stringsAsFactors = FALSE
)
prof <- fpa_profile_table(df, "src-meteo", "Meteo", declared_grain = "month", availability_lag_days = 2)
stopifnot(identical(prof$candidates$series_name, c("temperature_c", "gel")))
temp <- prof$candidates[prof$candidates$series_name == "temperature_c", ]
stopifnot(identical(temp$target_line, "REV"))
stopifnot(identical(temp$aggregation_rule, "mean"))
stopifnot(identical(temp$target_status, "suggested"))
stopifnot(identical(temp$observed_grain, "month"))
stopifnot(!nzchar(temp$grain_note))
stopifnot(!nzchar(temp$block_reason))
gel <- prof$candidates[prof$candidates$series_name == "gel", ]
stopifnot(nzchar(gel$block_reason))
stopifnot(isTRUE(gel$constant))

daily <- data.frame(
  jour = format(seq(as.Date("2024-01-01"), by = "day", length.out = 14), "%Y-%m-%d"),
  couverts = 1:14,
  stringsAsFactors = FALSE
)
day <- fpa_profile_table(daily, "src-day", "Couverts", declared_grain = "month")
stopifnot(identical(day$candidates$observed_grain, "day"))
stopifnot(grepl("Grain declare", day$candidates$grain_note))

twin <- data.frame(
  periode = dates,
  a = 1:12,
  b = 1:12,
  stringsAsFactors = FALSE
)
id <- fpa_profile_table(twin, "src-twin", "Twin", declared_grain = "month")
stopifnot(any(id$candidates$identity == "suspect_identity"))

parts <- data.frame(
  periode = dates,
  achats = 1:12,
  fret = 2:13,
  total = (1:12) + (2:13),
  stringsAsFactors = FALSE
)
pw <- fpa_profile_table(parts, "src-sum", "Somme", declared_grain = "month")
stopifnot(identical(pw$candidates$identity[pw$candidates$series_name == "total"], "part_whole"))

message("PASS profile")
