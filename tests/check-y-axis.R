# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Y-axis fit vs zero for Synthèse cash chart.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
say <- function(ok, msg) {
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  cat(if (isTRUE(ok)) "PASS " else "FAIL ", msg, "\n", sep = "")
}

vals <- c(33000, 35000, 40000, 46639)
fit <- fpa_shiny_y_axis(vals, mode = "fit", title = "Solde, EUR", anchors = 0)
zero <- fpa_shiny_y_axis(vals, mode = "zero", title = "Solde, EUR", anchors = 0)

say(fit$range[[1]] > 10000, paste("fit floor above 10k:", round(fit$range[[1]])))
say(fit$range[[1]] < min(vals), "fit pads below the series low")
say(zero$range[[1]] <= 0, paste("zero mode starts at or below 0:", round(zero$range[[1]])))
say(is.finite(fit$dtick) && fit$dtick < 20000, paste("major tick step finer than 20k:", fit$dtick))
n_fit <- (fit$range[[2]] - fit$range[[1]]) / fit$dtick
say(n_fit <= 6, paste("fit labels at most 7:", n_fit + 1))
wide <- fpa_shiny_y_axis(c(30000, 180000), mode = "fit", title = "Solde, EUR")
n_wide <- (wide$range[[2]] - wide$range[[1]]) / wide$dtick
say(n_wide <= 6, paste("wide horizon labels at most 7:", n_wide + 1, "step", wide$dtick))
say(!is.null(fit$minor) && isTRUE(fit$minor$showgrid), "secondary grid lines enabled")
say(is.finite(fit$minor$dtick) && abs(fit$minor$dtick - fit$dtick / 2) < 1e-9, "minor step is half major")

cat("PASS ", pass, " FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
