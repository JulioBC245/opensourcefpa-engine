# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Driver feed shapes: history_only does not count as rolling.
# LAST CHANGE: 2026-09-20
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()

hist <- list(
  source_id = "src-meteo-h",
  file_role = "ops_other",
  feed_shape = "history_only",
  time_frame = "history",
  grain = "month"
)
stopifnot(!isTRUE(fpa_source_is_rolling(hist)))
stopifnot(isTRUE(fpa_source_uses_correlation(hist)))

live <- list(
  source_id = "src-meteo-l",
  file_role = "ops_other",
  feed_shape = "living",
  time_frame = "rolling",
  grain = "month"
)
stopifnot(isTRUE(fpa_source_is_rolling(live)))
stopifnot(isTRUE(fpa_source_uses_correlation(live)))

roll <- list(
  source_id = "src-meteo-r",
  file_role = "ops_other",
  feed_shape = "split",
  pair_id = "pair-1",
  pair_role = "rolling",
  time_frame = "rolling"
)
stopifnot(isTRUE(fpa_source_is_rolling(roll)))
stopifnot(!isTRUE(fpa_source_uses_correlation(roll)))

cfg <- list(
  sources = list(
    list(source_id = "src-a", file_role = "ops_other", feed_shape = "split",
         pair_id = "pair-x", pair_role = "history", time_frame = "history")
  )
)
notes <- fpa_driver_pair_findings(cfg)
stopifnot(length(notes) == 1)

cfg2 <- list(
  sources = list(
    list(
      source_id = "src-pnl", file_role = "pnl_actuals",
      time_frame = "history", grain = "year"
    ),
    hist
  )
)
findings <- fpa_validate(cfg2, list(), NULL)
stopifnot(any(findings$code == "no_rolling_feed"))

cfg3 <- list(
  sources = list(
    list(
      source_id = "src-pnl", file_role = "pnl_actuals",
      time_frame = "history", grain = "year"
    ),
    live
  )
)
findings3 <- fpa_validate(cfg3, list(), NULL)
stopifnot(!any(findings3$code == "no_rolling_feed"))

message("test-drivers ok")
