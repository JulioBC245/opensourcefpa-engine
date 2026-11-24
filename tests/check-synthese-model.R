# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# One presentation model: cards, bridge, drivers, outlook, extremes.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
say <- function(ok, msg) {
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  cat(if (isTRUE(ok)) "PASS " else "FAIL ", msg, "\n", sep = "")
}

fy <- paste0("2026-", sprintf("%02d", 1:12))
pnl <- data.frame(
  periode = fy,
  REV = c(rep(10000, 9), 4000, 4000, 3645),
  COGS = c(rep(2000, 9), 900, 900, 800),
  PAYROLL = c(rep(3000, 9), 2500, 2500, 2500),
  OPEX_OTHER = c(rep(2000, 9), 1600, 1600, 845),
  OI = c(rep(3000, 9), -1000, -1000, -500),
  NI = c(rep(2500, 9), -1200, -1200, -600),
  stringsAsFactors = FALSE
)
close_v <- 10000 + cumsum(rep(2000, 12))
cash <- data.frame(
  periode = fy,
  cash_open = c(10000, head(close_v, 11)),
  cash_close = close_v,
  NI = pnl$NI,
  D_A = rep(100, 12),
  wc_impact = rep(-50, 12),
  wc_opening_release = 0,
  capex = 0,
  debt_net = 0,
  stringsAsFactors = FALSE
)
status <- data.frame(periode = fy, status = c(rep("actual", 9), rep("forecast", 3)), stringsAsFactors = FALSE)
config <- list(
  company = list(name = "Fixture Co", currency = "EUR"),
  calendar = list(year_end_month = 12L)
)
finance <- list(pnl = pnl, cash = cash, period_status = status, budget = NULL)
outlook <- list(opening = NULL, as_of = "2026-10-11", items = list())

m <- fpa_synthese_model(config, finance, outlook, cash_floor = 5000, today = as.Date("2026-10-11"))
say(isTRUE(m$ok), "model builds on fixture")
say(identical(m$context$company, "Fixture Co"), "context carries company")
say(abs(m$land$rev_y - (90000 + 11645)) < 1, "revenue card matches booked + forecast")
say(abs(m$land$oi_y - (27000 - 2500)) < 1, "OI card matches booked + forecast")
say(abs(m$land$cash_year_end - cash$cash_close[12]) < 1, "year-end cash card matches December close")
say(identical(m$outlook$state, "need_balance"), "null opening is need_balance on the model")
say(identical(m$finding$id, "need_balance"), "lead finding follows outlook")
say(grepl("11 octobre 2026", m$finding$body), "card names today")
say(grepl("28 710", m$finding$body), "card shows the mid-month cash indication")
say(grepl("34 000", m$finding$body) && grepl("décembre 2026", m$finding$body), "card still states the December landing")
on_11 <- fpa_synth_cash_on_date(cash, as.Date("2026-10-11"))
say(abs(on_11 - (28000 + 2000 * 11 / 31)) < 0.01, "11 October is opening plus 11/31 of the month")
say(abs(fpa_synth_cash_on_date(cash, as.Date("2026-10-31")) - 30000) < 0.01, "month end equals the close")
say(nrow(m$bridge) == 8L, "cash bridge has eight rows")
say(all(is.finite(m$bridge$amount)), "bridge amounts are finite")
# Consistent month: open + NI + D_A + wc + release + capex + debt = close
one <- data.frame(
  periode = "2026-01", cash_open = 10000, cash_close = 12550,
  NI = 2500, D_A = 100, wc_impact = -50, wc_opening_release = 0, capex = 0, debt_net = 0,
  stringsAsFactors = FALSE
)
rows1 <- fpa_synth_cash_bridge_rows(one, "2026-01")
say(abs(rows1$amount[[1]] + sum(rows1$amount[2:7]) - rows1$amount[[8]]) < 1.5,
    "opening + flows = closing on a single consistent month")
say(m$cash_floor == 5000, "configured floor is preserved")
say(nrow(m$drivers) >= 1L, "open months yield at least one driver or residual")

outlook2 <- list(opening = 28000, as_of = "2026-09-15", items = list())
m2 <- fpa_synthese_model(config, finance, outlook2, 0, as.Date("2026-10-11"))
say(identical(m2$outlook$state, "balance_only"), "opening alone is balance_only")
say(is.finite(m2$model_cash_at_asof), "model cash at as_of month is finite")

cash0 <- cash
cash0$cash_close <- 0
cash0$cash_open <- 0
finance0 <- finance
finance0$cash <- cash0
m0 <- fpa_synthese_model(config, finance0, NULL, 0, as.Date("2026-10-11"))
say(isTRUE(m0$ok) && is.finite(m0$land$cash_year_end), "zero cash path still builds")

tmp <- tempfile("fpa-snap")
dir.create(file.path(tmp, "work"), recursive = TRUE)
paths <- list(work = file.path(tmp, "work"))
a <- list(timestamp = "2026-10-01", oi_fy = 10000, cash_year_end = 40000)
b <- list(timestamp = "2026-10-11", oi_fy = 15000, cash_year_end = 46000)
fpa_snapshot_write(paths, a)
fpa_snapshot_write(paths, b)
prev <- fpa_snapshot_read_prev(paths)
cur <- fpa_snapshot_read(paths)
say(identical(as.numeric(prev$oi_fy), 10000) && identical(as.numeric(cur$oi_fy), 15000), "prev snapshot retained across write")
diff <- fpa_snapshot_diff(prev, cur)
say(length(diff) >= 1L, "material moves surface from prev to current")

old <- list(id = "j1", type = "decision", title = "Test", note = "n")
norm <- fpa_journal_normalize(old)
say(identical(norm$owner, "") && identical(norm$due, "") && identical(norm$state, "open"),
    "legacy journal entries gain empty owner/due and open state")
norm2 <- fpa_journal_normalize(list(id = "j2", type = "decision", owner = "Ada", due = "2026-11-01", state = "watching"))
say(identical(norm2$owner, "Ada") && identical(norm2$state, "watching"), "owner/due/state round-trip")

cat("PASS ", pass, " FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
