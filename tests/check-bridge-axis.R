# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Cash bridge axis floor, window length, and the forecast divider.
# Run from the opensourcefpa-engine root: Rscript tests/check-bridge-axis.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

src <- readLines("app.R", encoding = "UTF-8", warn = FALSE)
grab <- function(start_pat, end_pat) {
  start <- grep(start_pat, src)[[1]]
  end <- grep(end_pat, src)[[1]]
  eval(parse(text = src[start:(end - 1L)]), envir = .GlobalEnv)
}
grab("^fpa_shiny_round_axis <- function", "^fpa_shiny_path_sentence <- function")
grab("^fpa_shiny_bridge_periods <- function", "^#' X of the dotted line")
grab("^fpa_shiny_bridge_future_x <- function", "^fpa_shiny_bridge_build <- function")

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  cat(if (isTRUE(ok)) "PASS " else "FAIL ", label, "\n", sep = "")
}

zero <- fpa_shiny_round_axis(c(0, 10000, 45862), "zero")
check("zero floor is 0", identical(zero[[1]], 0))
check("zero ceiling is 50000", identical(zero[[2]], 50000))

fit_low <- fpa_shiny_round_axis(c(10000, 45862), "fit")
check("fit floor is the 10 000 step", identical(fit_low[[1]], 10000))

fit_high <- fpa_shiny_round_axis(c(45862, 50576), "fit")
check("fit floor under 45 862 is 40 000", identical(fit_high[[1]], 40000))
check("fit ceiling over 50 576 is 60 000", identical(fit_high[[2]], 60000))

neg <- fpa_shiny_round_axis(c(-2400, 8000), "zero")
check("a real negative still has room", neg[[1]] < 0 && neg[[1]] >= -10000)

cash <- data.frame(periode = sprintf("2026-%02d", 1:12), stringsAsFactors = FALSE)
q <- fpa_shiny_bridge_periods(cash, "3", NULL, NULL, "2026-10")
check("quarter from October is 3 months", identical(q, c("2026-10", "2026-11", "2026-12")))
six <- fpa_shiny_bridge_periods(cash, "6", NULL, NULL, "2026-07")
check("six months from July", identical(six, sprintf("2026-%02d", 7:12)))
one <- fpa_shiny_bridge_periods(cash, "1", NULL, NULL, "2026-03")
check("one month is the start date", identical(one, "2026-03"))
full <- fpa_shiny_bridge_periods(cash, "all", NULL, NULL, "2026-04")
check("full period runs from the start date", identical(full, sprintf("2026-%02d", 4:12)))

built <- list(
  bars = data.frame(x = c(1, 2, 4, 5)),
  months = data.frame(
    month = c("2026-09", "2026-10"),
    x0 = c(1, 4), x1 = c(2, 5),
    stringsAsFactors = FALSE
  )
)
cut <- fpa_shiny_bridge_future_x(built, "2026-10")
check("divider sits between September and October", abs(cut - 3) < 1e-6)
none <- fpa_shiny_bridge_future_x(built, "2027-01")
check("a closed window has no divider", is.na(none))
ahead <- fpa_shiny_bridge_future_x(built, c("2026-09", "2026-10"))
check("an all-forecast window marks the left edge", is.finite(ahead) && ahead < 1)

cat("PASS ", pass, " FAIL ", fail, "\n", sep = "")
quit(status = if (fail) 1 else 0)
