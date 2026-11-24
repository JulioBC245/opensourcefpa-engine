# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Live hotel configuration, when the file is on this machine.
# Skips quietly elsewhere so the suite still runs.
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg <- "../fpa-local-test/fpa-config.json"
if (!file.exists(cfg)) {
  message("SKIP live hotel: config absent")
} else {
  res <- fpa_run(cfg, write = FALSE, stop_on_error = TRUE)
  stopifnot(nrow(res$pnl) >= 24)
  stopifnot(isTRUE(res$identity$ok))
  stopifnot(!is.null(res$variance))
  message("PASS live hotel")
}
