# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
#' G4: a saved decision compared with a later actual, and starred lines for the report.
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(root)

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

cash <- data.frame(
  periode = c("2026-09", "2026-10"),
  cash_close = c(80, 100),
  stringsAsFactors = FALSE
)
pnl <- data.frame(
  periode = c("2026-09", "2026-10"),
  OI = c(10, 12),
  stringsAsFactors = FALSE
)
status <- data.frame(
  periode = c("2026-09", "2026-10"),
  status = c("actual", "forecast"),
  stringsAsFactors = FALSE
)
today <- as.Date("2026-09-27")

obs <- list(type = "observation", periode = "2026-09", snapshot = list(cash_close = 100))
say(!isTRUE(fpa_journal_followup(obs, cash, pnl, status, today)$show), "observation is not a follow-up")

dec <- list(
  type = "decision", periode = "2026-09", review_date = "2026-09-01",
  title = "Garder le plan", created_at = "2026-09-01", favorite = TRUE,
  snapshot = list(cash_close = 100), amount = 100, amount_label = "Trésorerie"
)
fu <- fpa_journal_followup(dec, cash, pnl, status, today)
say(isTRUE(fu$show) && isTRUE(fu$due) && identical(fu$state, "actual"), "due decision on an actual month")
say(is.finite(fu$gap) && abs(fu$gap - (80 - 100)) < 0.01, paste("gap", fu$gap))
txt <- fpa_journal_followup_text(fu)
flat <- gsub(" ", "", txt)
say(grepl("80", flat) && grepl("100", flat) && grepl("-20", flat), txt)

open <- dec
open$periode <- "2026-10"
open$review_date <- "01/11/2026"
open$favorite <- FALSE
open$title <- "Attendre octobre"
open$snapshot <- list(cash_close = 50)
fu2 <- fpa_journal_followup(open, cash, pnl, status, today)
say(identical(fu2$state, "open") && !isTRUE(fu2$due), "forecast month stays open and not due")
txt2 <- fpa_journal_followup_text(fu2)
say(grepl("pas encore", txt2) && !grepl("cart", txt2), txt2)

say(identical(fpa_journal_review_date("27/09/2026"), as.Date("2026-09-27")), "french review date")

fy_dec <- list(
  type = "decision", periode = "2026-09", review_date = "2026-09-01", title = "Objectif RE",
  snapshot = list(
    cash_close = 999, metric = "oi_fy", oi_fy = 20, oi_last = 12,
    ye_month = "2026-10", fy_months = c("2026-09", "2026-10")
  ),
  amount = 5, amount_label = "Écart de trésorerie"
)
fu_fy <- fpa_journal_followup(fy_dec, cash, pnl, status, today)
say(identical(fu_fy$periode, "2026-10") && fu_fy$frozen == 20 && fu_fy$actual == 22,
    paste("fiscal-year OI decision compared on the year sum:", fu_fy$frozen, fu_fy$actual))
month_dec <- fy_dec
month_dec$snapshot$metric <- "oi_ye"
fu_m <- fpa_journal_followup(month_dec, cash, pnl, status, today)
say(fu_m$frozen == 12 && fu_m$actual == 12 && grepl("octobre 2026", fu_m$label),
    paste("close-month OI decision compared on that month:", fu_m$label))
old_dec <- fy_dec
old_dec$snapshot$metric <- NULL
say(fpa_journal_followup(old_dec, cash, pnl, status, today)$frozen == 999, "entry without a metric keeps the cash reading")

lines <- fpa_journal_report_lines(list(dec, open), cash, pnl, status, today)
blob <- paste(lines, collapse = "\n")
say(grepl("Garder le plan", blob), "starred decision in the report")
say(grepl("-20", gsub(" ", "", blob)), "report gap is actual minus frozen")
say(!grepl("Attendre octobre", blob), "unstarred decision stays out of the report")
due <- fpa_journal_due_items(list(dec, open, obs), cash, pnl, status, today)
say(length(due) == 1L && identical(due[[1]]$title, "Garder le plan"), paste("due count", length(due)))
say(grepl("-20", gsub(" ", "", due[[1]]$sentence)), "due line carries the gap")

none <- fpa_journal_report_lines(list(open), cash, pnl, status, today)
say(any(grepl("Aucune d", none)), paste(none, collapse = " | "))

say(identical(fpa_txt("out_out"), "Paiement"), "sens label is Paiement")
say(identical(fpa_txt("out_col_out"), "Paiements"), "week column is Paiements")
say(
  !grepl("d[eé]caiss", fpa_txt("out_help"), ignore.case = TRUE) &&
    !grepl("d[eé]caiss", fpa_txt("out_balance_only"), ignore.case = TRUE),
  "help text has no decaissement"
)

if (fail > 0L) stop(fail, " follow-up check(s) failed", call. = FALSE)
cat("ALL CHECKS PASSED\n")
