# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# The open month keeps only the days still ahead, and that narrower gap carries forward.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-synthese.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

periode <- c("2026-09", "2026-10", "2026-11", "2026-12")
central <- c(50000, 57000, 56000, 54000)
low <- central + c(0, -3100, -4100, -5100)
high <- central + c(0, 3100, 4100, 5100)
got <- fpa_shiny_band_remaining(periode, central, low, high, as.Date("2026-10-20"))
frac <- 11 / 31
say(abs(got$left_frac[[1]] - frac) < 1e-9, "eleven days remain in October")
say(abs((got$low[[2]] - central[[2]]) - (-3100 * frac)) < 1e-6, "October keeps the days still ahead")
say(abs((got$low[[3]] - central[[3]]) - (-3100 * frac - 1000)) < 1e-6, "November carries the narrower October gap")
say(abs((got$low[[4]] - central[[4]]) - (-3100 * frac - 2000)) < 1e-6, "December stays on the same line")
say(abs(got$low[[1]] - central[[1]]) < 1e-6, "the closed month does not open")
full <- fpa_shiny_band_remaining(periode, central, low, high, as.Date("2026-10-01"))
say(abs(full$left_frac[[1]] - 30 / 31) < 1e-9, "the first of the month is almost the full gap")
done <- fpa_shiny_band_remaining(periode, central, low, high, as.Date("2026-10-31"))
say(abs(done$low[[2]] - central[[2]]) < 1e-6, "month-end has spent the October gap")
say(abs((done$low[[3]] - central[[3]]) - (-1000)) < 1e-6, "the next month still adds its own gap")

cat("FAILS", fail, "\n")
quit(status = if (fail) 1 else 0)
