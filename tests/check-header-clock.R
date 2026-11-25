# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Header clock: fiscal-year progress and days left in the month.
# Run from the opensourcefpa-engine root: Rscript tests/check-header-clock.R

root <- if (file.exists("R/fpa-health.R")) "." else ".."
source(file.path(root, "R", "fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-health.R"), encoding = "UTF-8")

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}

dec <- list(company = list(fiscal_year_end_month = 12L))
c1 <- fpa_header_clock(dec, as.Date("2026-10-07"))
check("7 oct 2026 is day 280 of a 365-day year", identical(c1$done, 280L) && identical(c1$span, 365L))
check("that day is 77 percent of the year", identical(c1$pct, 77L))
check("24 days remain before 31 oct", identical(c1$left, 24L) && identical(c1$month_end, as.Date("2026-10-31")))
check("December close runs 1 jan to 31 dec", identical(c1$start, as.Date("2026-01-01")) && identical(c1$end, as.Date("2026-12-31")))
check("the date reads 7 oct. 2026", identical(fpa_header_fr_date(c1$today), "7 oct. 2026"))
check("the year sentence states the percent", identical(fpa_txt("hdr_year_done", c1$pct), "77 % de l'exercice annuel écoulé"))
check("the month sentence states the days", identical(fpa_txt("hdr_month_left", c1$left), "24 jours avant la fin du mois"))

c0 <- fpa_header_clock(dec, as.Date("2026-10-31"))
check("the last day of the month has zero days left", identical(c0$left, 0L))
c1d <- fpa_header_clock(dec, as.Date("2026-10-30"))
check("the day before month end leaves one day", identical(c1d$left, 1L))

leap <- fpa_header_clock(dec, as.Date("2024-02-29"))
check("29 feb 2024 is the last day, day 60 of 366", identical(leap$left, 0L) && identical(leap$done, 60L) && identical(leap$span, 366L) && identical(leap$pct, 16L))

mar <- list(company = list(fiscal_year_end_month = 3L))
c2 <- fpa_header_clock(mar, as.Date("2026-03-15"))
check("March close on 15 mar 2026 started the previous April", identical(c2$start, as.Date("2025-04-01")) && identical(c2$end, as.Date("2026-03-31")))
check("that day is 96 percent, with 16 days left in March", identical(c2$pct, 96L) && identical(c2$left, 16L))

blank <- fpa_header_clock(list(), as.Date("2026-01-01"))
check("a missing close month is December, and 1 January is 0 percent after rounding", identical(blank$start, as.Date("2026-01-01")) && identical(blank$pct, 0L))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
