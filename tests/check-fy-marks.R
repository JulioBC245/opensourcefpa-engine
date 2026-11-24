# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Fiscal-year marks on a multi-year cash path, and quoted sync arguments.
# Run from fpa-local: Rscript tests/check-fy-marks.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
say <- function(ok, msg) {
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  cat(if (isTRUE(ok)) "PASS " else "FAIL ", msg, "\n", sep = "")
}

span <- sprintf("2026-%02d", 1:12)
span <- c(span, sprintf("2027-%02d", 1:9))
marks <- fpa_fy_mark_dates(span, 12L)
say(nrow(marks) == 1L && identical(as.character(marks$x), "2027-01-01") && identical(marks$label, "2027"),
    "December close: one line at 1 Jan 2027")

long <- c(span, sprintf("2027-%02d", 10:12), sprintf("2028-%02d", 1:3))
marks2 <- fpa_fy_mark_dates(long, 12L)
say(identical(as.character(marks2$x), c("2027-01-01", "2028-01-01")),
    "through Mar 2028: 2027 and 2028")
say(identical(marks2$label, c("2027", "2028")), "labels are the years that start")

one <- fpa_fy_mark_dates(sprintf("2026-%02d", 1:12), 12L)
say(!nrow(one), "a single fiscal year draws no line")

mar <- fpa_fy_mark_dates(c(sprintf("2026-%02d", 1:12), sprintf("2027-%02d", 1:6)), 3L)
say(identical(as.character(mar$x), c("2026-04-01", "2027-04-01")),
    "March close: lines on 1 April")
say(identical(mar$label, c("26-27", "27-28")), "March close labels name the year that closes")

fy <- sprintf("2026-%02d", 1:12)
win <- fpa_fy_window(fy, "fy")
say(identical(win$months[[1]], "2026-01") && identical(win$months[[12]], "2026-12"),
    "default window is the fiscal year")
say(as.Date("2026-01-31") >= win$start && as.Date("2026-12-31") <= win$end && as.Date("2027-01-31") > win$end,
    "January 2027 sits outside the fiscal-year window")
prior <- fpa_fy_window(fy, "prior")
say(identical(prior$months[[1]], "2025-01") && identical(prior$months[[12]], "2025-12"),
    "the other window is the previous fiscal year")

q <- fpa_sync_curl_args(c("-o", "out.json", "http://127.0.0.1/p?a=1&b=2"), 8L)
hit <- q[grepl("a=1", q, fixed = TRUE)]
say(length(hit) == 1L && grepl("b=2", hit, fixed = TRUE) && grepl("&", hit, fixed = TRUE),
    "an address with & stays one argument")
say(any(q == shQuote("-m", type = "cmd")) && any(grepl("8", q, fixed = TRUE)),
    "the call carries a hard stop")

t0 <- proc.time()[[3]]
code <- fpa_sync_curl(c("-o", tempfile(fileext = ".txt"), "-w", "%{http_code}", "http://127.0.0.1:9/"), seconds = 3L)
el <- proc.time()[[3]] - t0
say(el < 8, paste0("closed port returns in ", round(el, 1), "s"))
say(nzchar(as.character(code[[1]] %||% "")), "curl returns a status string")

cat("PASS ", pass, " FAIL ", fail, "\n", sep = "")
if (fail) quit(status = 1)
