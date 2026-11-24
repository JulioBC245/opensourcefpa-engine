# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-cash.R — Indirect cash bridge (no plug)
# LAST CHANGE: 2026-09-18 — L1/L2
#
# Bridge: NI + D&A + WC_impact - Capex + Debt_net = change in cash
# Cash_t = Cash_{t-1} + delta_cash_t

#' Build monthly indirect cash bridge.
fpa_build_cash_bridge <- function(pnl, wc_impact, capex, debt_net, opening_cash = 0,
                                 opening_release = NULL) {
  periods <- pnl$periode
  n <- length(periods)
  release <- if (is.null(opening_release)) rep(0, n) else as.numeric(opening_release[periods])
  release[!is.finite(release)] <- 0
  bridge <- data.frame(
    periode = periods,
    NI = pnl$NI,
    D_A = pnl$D_A,
    wc_impact = as.numeric(wc_impact[periods]),
    wc_opening_release = release,
    capex = as.numeric(capex[periods]),
    debt_net = as.numeric(debt_net[periods]),
    stringsAsFactors = FALSE
  )
  bridge$delta_cash <- bridge$NI + bridge$D_A + bridge$wc_impact + bridge$wc_opening_release - bridge$capex + bridge$debt_net
  bridge$cash_open <- NA_real_
  bridge$cash_close <- NA_real_
  cash <- opening_cash
  for (i in seq_len(n)) {
    bridge$cash_open[[i]] <- cash
    cash <- cash + bridge$delta_cash[[i]]
    bridge$cash_close[[i]] <- cash
  }
  bridge$CFO <- bridge$NI + bridge$D_A + bridge$wc_impact + bridge$wc_opening_release
  bridge$CFI <- -bridge$capex
  bridge$CFF <- bridge$debt_net
  bridge
}
