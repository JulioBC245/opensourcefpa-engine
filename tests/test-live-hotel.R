# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Live hotel configuration, when the optional private dataset is present.
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg <- fpa_optional_data_config()
if (is.null(cfg)) {
  fpa_skip_if_no_optional_data("live hotel run")
} else {
  res <- fpa_run(cfg, write = FALSE, stop_on_error = TRUE)
  stopifnot(nrow(res$pnl) >= 24)
  stopifnot(isTRUE(res$identity$ok))
  stopifnot(!is.null(res$variance))
  message("PASS live hotel")
}
