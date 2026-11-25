# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# The monthly result chart bands the month, not the running total.
# Run from the opensourcefpa-engine root: Rscript tests/check-oi-ribbon.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

src <- readLines("app.R", encoding = "UTF-8", warn = FALSE)
start <- grep("^fpa_shiny_oi_ribbon <- function", src)[[1]]
end <- grep("^#' Empty when prudent and ambitieux", src)[[1]]
eval(parse(text = src[start:(end - 1L)]))

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  cat(if (isTRUE(ok)) "PASS " else "FAIL ", label, "\n", sep = "")
}

monthly <- data.frame(
  periode = c("2026-08", "2026-09", "2026-10", "2026-11"),
  etat = c("réalisé", "réalisé", "prévision", "prévision"),
  OI = c(4000, 2000, -500, -800),
  stringsAsFactors = FALSE
)
low <- data.frame(periode = monthly$periode, OI = c(4000, 2000, -2000, -3000), stringsAsFactors = FALSE)
high <- data.frame(periode = monthly$periode, OI = c(4000, 2000, 1000, 1500), stringsAsFactors = FALSE)
rib <- fpa_shiny_oi_ribbon(low, high, monthly)

check("band starts on the last closed month", identical(rib$periode[[1]], "2026-09"))
check("closed month has no width", abs(rib$ymin[[1]] - 2000) < 1e-6 && abs(rib$ymax[[1]] - 2000) < 1e-6)
oct <- rib[rib$periode == "2026-10", , drop = FALSE]
check("October band is the monthly range", abs(oct$ymin - (-2000)) < 1e-6 && abs(oct$ymax - 1000) < 1e-6)
check("band is not the running total", max(abs(c(rib$ymin, rib$ymax))) < 5000)

cat("PASS ", pass, " FAIL ", fail, "\n", sep = "")
quit(status = if (fail) 1 else 0)
