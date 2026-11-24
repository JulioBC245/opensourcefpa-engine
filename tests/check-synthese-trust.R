# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Trust, outlook states, finding priority, origins, run contract.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
say <- function(ok, msg) {
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  cat(if (isTRUE(ok)) "PASS " else "FAIL ", msg, "\n", sep = "")
}

say(identical(fpa_synth_origin("rebuilt"), fpa_txt("synth_origin_rebuilt")), "modeled cash origin is reconstruit")
say(identical(fpa_synth_origin("declared"), fpa_txt("synth_origin_declared")), "bank origin is déclaré")

L <- list(
  ok = TRUE, rev_y = 109145, oi_y = 15123, oi_a = 26280, oi_f = -11157,
  cash_year_end = 46639, cash_year_end_month = "2026-12",
  cash_close = 50000, n_unknown = 0L, bud_covers = FALSE, bud_partial = TRUE
)

need <- fpa_synth_outlook_state(NULL, NULL, as.Date("2026-10-11"))
say(identical(need$state, "need_balance"), "missing file is need_balance")

bal_only <- fpa_synth_outlook_state(
  list(opening = 10000, as_of = "2026-10-11", items = list()),
  NULL, as.Date("2026-10-11")
)
say(identical(bal_only$state, "balance_only"), "opening without items is balance_only")

stale <- fpa_synth_outlook_state(
  list(opening = 10000, as_of = "2026-09-01", items = list(list(amount = 100, kind = "out", date = "2026-09-10", label = "x"))),
  list(low = 9000, low_date = as.Date("2026-09-10")),
  as.Date("2026-10-11")
)
say(identical(stale$state, "stale_horizon"), "dated items older than 7 days are stale_horizon")

partial <- fpa_synth_outlook_state(
  list(opening = 10000, as_of = "2026-10-10", items = list(list(amount = 100, kind = "out", date = "2026-10-20", label = "x"))),
  list(low = 9900, low_date = as.Date("2026-10-20")),
  as.Date("2026-10-11")
)
say(identical(partial$state, "partial"), "fresh dated items are partial schedule")

trust <- fpa_synth_trust(L, bal_only, cash_floor = 0, model_cash_at_asof = 50000)
say(identical(trust$cash, "limited"), "reconstructed cash stays limited")
say(is.finite(trust$declared_model_gap) && abs(trust$declared_model_gap - (10000 - 50000)) < 1, "declared vs model gap is signed")

find_need <- fpa_synth_finding(L, trust, need, 0, NA_real_)
say(identical(find_need$id, "need_balance"), "missing balance is the lead finding")

find_bal <- fpa_synth_finding(L, trust, bal_only, 0, 50000)
say(identical(find_bal$id, "balance_only") || identical(find_bal$id, "cash_gap") || identical(find_bal$id, "cash_gap_quiet"),
    "balance-only or gap when opening exists")

journal_diamond <- list(list(
  source_metric = "outlook_4w",
  amount = 50000,
  note = "test",
  snapshot = list(as_of = "2026-10-15", opening = 50000, opening_source = "declared")
))
latest <- fpa_synth_latest_declared(journal_diamond)
say(is.finite(latest$opening) && identical(latest$as_of, "2026-10-15"), "latest journal diamond is read")

fin_stub <- list(
  pnl = data.frame(periode = "2026-09", REV = 10000, OI = 2000, PAYROLL = 3000, stringsAsFactors = FALSE),
  cash = data.frame(
    periode = c("2026-09", "2026-10"),
    cash_open = c(48000, 49000), cash_close = c(49000, 51000),
    NI = c(1000, 1000), D_A = c(0, 0), wc_impact = c(0, 0),
    wc_opening_release = c(0, 0), capex = c(0, 0), debt_net = c(0, 0),
    stringsAsFactors = FALSE
  ),
  period_status = data.frame(periode = c("2026-09", "2026-10"), status = c("actual", "forecast"), stringsAsFactors = FALSE),
  budget = NULL
)
# Live file missing opening, but a journal diamond exists → gap, not need_balance.
m_gap <- fpa_synthese_model(
  list(company = list(name = "Hotel", currency = "EUR", fiscal_year_end_month = 12L)),
  fin_stub,
  outlook_saved = list(as_of = "2026-10-20", opening = NULL, items = list()),
  cash_floor = 0,
  today = as.Date("2026-10-14"),
  journal_entries = journal_diamond
)
say(!identical(m_gap$finding$id, "need_balance"), "journal diamond leaves need_balance")
say(identical(m_gap$finding$id, "cash_gap") || identical(m_gap$finding$id, "cash_gap_quiet"),
    "journal diamond yields a gap finding")
say(is.finite(m_gap$outlook$opening) && abs(m_gap$outlook$opening - 50000) < 1,
    "outlook opening is taken from the journal diamond")

quiet_out <- list(state = "balance_only", opening = 50500, as_of = "2026-10-15", n_items = 0L)
quiet_trust <- fpa_synth_trust(L, quiet_out, 0, 50000)
find_quiet <- fpa_synth_finding(L, quiet_trust, quiet_out, 0, 50000)
say(identical(find_quiet$id, "cash_gap_quiet"), "a small gap is a quiet note")

jan_cash <- data.frame(
  periode = c("2026-01", "2026-02"),
  wc_opening_release = c(21992, 0),
  stringsAsFactors = FALSE
)
wc_n <- fpa_synth_wc_open_note(jan_cash, c("2026-01", "2026-02"))
say(!is.null(wc_n) && grepl("BFR d'ouverture", wc_n$sentence, fixed = TRUE), "January WC release gets a sentence")

L_floor <- L
L_floor$cash_year_end <- -100
find_floor <- fpa_synth_finding(
  L_floor,
  list(declared_model_gap = NA_real_),
  list(state = "partial", opening = 10000),
  cash_floor = 0,
  model_cash_at_asof = 10000
)
say(identical(find_floor$id, "cash_floor"), "year-end under floor is cash_floor")

ctr <- fpa_run_contract_ok()
say(isTRUE(ctr$ok), paste0("fpa_run contract loaded (", length(fpa_run_required_funs()), " funs)"))
if (!isTRUE(ctr$ok)) cat(" missing: ", paste(ctr$missing, collapse = ", "), "\n", sep = "")

cat("PASS ", pass, " FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
