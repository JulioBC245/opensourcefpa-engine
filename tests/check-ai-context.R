# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Analyst context uses the fiscal year, the cutoff month, and an honest missing block.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-common.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-ai.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-ai-prompts.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

months <- c("2026-01", "2026-09", "2026-12", "2027-04")
pnl <- data.frame(
  periode = months,
  REV = c(1000, 2000, 3000, 9000),
  OI = c(100, 200, 300, 900),
  NI = c(10, 20, 30, 90),
  stringsAsFactors = FALSE
)
fy <- months[substr(months, 1, 4) == "2026"]
say(identical(fpa_ai_voted_revenue(pnl, fy), 6000), "revenue base is the fiscal year")
say(identical(fpa_ai_voted_revenue(pnl), 15000), "without a window the whole frame is summed")

books <- list(pnl = pnl, cash = data.frame(
  periode = months,
  cash_open = c(1, 2, 3, 4),
  cash_close = c(10, 55, 80, 1),
  NI = 0, D_A = 0, wc_impact = 0, capex = 0, debt_net = 0,
  stringsAsFactors = FALSE
))
ctx <- fpa_ai_context(books, closed = "2026-09", months = fy, cutoff = "2026-09", currency = "EUR")
say(grepl("2026-09", ctx$headline, fixed = TRUE), "headline uses the cutoff month")
say(!grepl("2026-12", ctx$headline, fixed = TRUE), "a later month is not the headline")
say(grepl("55", ctx$cash, fixed = TRUE), "cash uses the cutoff month")
say(!grepl("80", ctx$cash, fixed = TRUE), "year-end cash is not the cutoff cash")
say(identical(ctx$drivers, "Not included in this request."), "a missing block is named as not included")
say(grepl("6000.0000", ctx$scope, fixed = TRUE), "scope states the fiscal-year revenue")

prompt <- fpa_ai_system_prompt("sans", "fr", "€", reconciled = FALSE)
say(grepl("four short parts", prompt, fixed = TRUE), "the answer has four parts")
say(!grepl("reconciled monthly pack", prompt, fixed = TRUE), "the pack is not called reconciled")
say(grepl("Not included in this request.", prompt, fixed = TRUE), "the missing-block phrase is defined")

box <- tempfile("fpa-closed-")
dir.create(file.path(box, "work"), recursive = TRUE)
utils::write.csv(
  data.frame(periode = c("2026-08", "2026-09", "2026-10"), status = c("actual", "actual", "forecast")),
  file.path(box, "work", "period-status.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
say(identical(fpa_ai_closed_through(box), "2026-09"), "actual is the closed status")

realise <- tempfile("fpa-realise-")
dir.create(file.path(realise, "work"), recursive = TRUE)
utils::write.csv(
  data.frame(periode = c("2026-08", "2026-09"), status = c("réalisé", "prévision")),
  file.path(realise, "work", "period-status.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
say(identical(fpa_ai_closed_through(realise), "2026-08"), "réalisé is read as actual")

cat("FAILS", fail, "\n")
quit(status = if (fail) 1 else 0)
