# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-pnl.R — Monthly P&L through net income
# LAST CHANGE: 2026-09-18 — L1/L2

#' Build monthly P&L from imported mapped actuals (+ optional mechanism overlays).
#' Amounts for expense lines are positive costs; NI = REV + OTHER - costs.
fpa_build_pnl <- function(mapped_pnl, periods = NULL, overlays = NULL, da_extra = NULL) {
  if (is.null(mapped_pnl) || !nrow(mapped_pnl)) {
    return(fpa_pnl_empty())
  }
  # Determine periods
  pe_dates <- as.Date(mapped_pnl$period_end)
  mapped_pnl$periode <- format(pe_dates, "%Y-%m")
  if (is.null(periods)) {
    periods <- sort(unique(mapped_pnl$periode[!is.na(mapped_pnl$periode)]))
  }
  ids <- fpa_pnl_input_ids()
  out <- data.frame(periode = periods, stringsAsFactors = FALSE)
  for (id in ids) out[[id]] <- 0
  for (p in periods) {
    sl <- mapped_pnl[mapped_pnl$periode == p, , drop = FALSE]
    for (id in ids) {
      out[out$periode == p, id] <- sum(sl$montant[sl$functional_label == id], na.rm = TRUE)
    }
  }
  # Overlays from volumes / assumptions (replace or add)
  if (!is.null(overlays)) {
    for (nm in names(overlays)) {
      if (!(nm %in% names(out))) next
      ov <- overlays[[nm]]
      for (p in intersect(names(ov), periods)) {
        if (!is.na(ov[[p]])) out[out$periode == p, nm] <- ov[[p]]
      }
    }
  }
  # Extra D&A from capex schedule (mechanical)
  if (!is.null(da_extra)) {
    for (p in intersect(names(da_extra), periods)) {
      out[out$periode == p, "D_A"] <- out[out$periode == p, "D_A"] + da_extra[[p]]
    }
  }
  out <- fpa_pnl_derive(out)
  out
}

#' Add calendar depreciation on forecast months, after the seasonal figure is in place.
fpa_pnl_add_calendar_da <- function(pnl, da_extra, forecast_periods) {
  if (is.null(pnl) || !nrow(pnl) || is.null(da_extra) || !length(forecast_periods)) return(pnl)
  touched <- FALSE
  for (p in intersect(names(da_extra), as.character(forecast_periods))) {
    add <- suppressWarnings(as.numeric(da_extra[[p]]))
    if (length(add) != 1L || !is.finite(add) || abs(add) < 1e-9) next
    i <- match(p, as.character(pnl$periode))
    if (is.na(i)) next
    pnl$D_A[[i]] <- suppressWarnings(as.numeric(pnl$D_A[[i]])) + add
    touched <- TRUE
  }
  if (!touched) return(pnl)
  fpa_pnl_derive(pnl)
}

fpa_pnl_empty <- function() {
  ids <- fpa_pnl_input_ids()
  out <- data.frame(periode = character(), stringsAsFactors = FALSE)
  for (id in ids) out[[id]] <- numeric()
  out$GP <- numeric()
  out$EBITDA <- numeric()
  out$OI <- numeric()
  out$NI <- numeric()
  out
}

#' Derived lines: GP, EBITDA, OI, NI.
fpa_pnl_derive <- function(pnl) {
  pnl$GP <- pnl$REV + pnl$OTHER_OP_INC - pnl$COGS
  pnl$EBITDA <- pnl$GP - pnl$PAYROLL - pnl$OPEX_OTHER
  pnl$OI <- pnl$EBITDA - pnl$D_A
  # FIN and EXCEP treated as net expense when positive
  pnl$NI <- pnl$OI - pnl$FIN - pnl$EXCEP - pnl$TAX
  pnl
}

#' Apply tax as taux.impot * max(OI - FIN - EXCEP, 0) when TAX not imported.
fpa_pnl_apply_tax <- function(pnl, assum_wide, config) {
  if (!fpa_mechanism_on(config, "effective_tax_rate")) return(pnl)
  for (i in seq_len(nrow(pnl))) {
    p <- pnl$periode[[i]]
    # If TAX already imported and non-zero, keep as imported
    if (!is.na(pnl$TAX[[i]]) && abs(pnl$TAX[[i]]) > 1e-9) next
    rate <- tryCatch(
      fpa_assumption_num(assum_wide, p, "taux.impot", default = 0.25),
      error = function(e) 0.25
    )
    base <- max(pnl$OI[[i]] - pnl$FIN[[i]] - pnl$EXCEP[[i]], 0)
    pnl$TAX[[i]] <- base * rate
  }
  fpa_pnl_derive(pnl)
}

#' Apply fixed monthly expense into OPEX_OTHER from charge.fixe.* params.
fpa_pnl_apply_fixed <- function(pnl, assum_wide, config) {
  if (!fpa_mechanism_on(config, "fixed_monthly_expense")) return(pnl)
  if (is.null(assum_wide)) return(pnl)
  fix_cols <- grep("^charge\\.fixe\\.", names(assum_wide), value = TRUE)
  if (!length(fix_cols)) return(pnl)
  for (i in seq_len(nrow(pnl))) {
    p <- pnl$periode[[i]]
    add <- 0
    for (c in fix_cols) {
      add <- add + fpa_assumption_num(assum_wide, p, c, default = 0)
    }
    # If OPEX already has content from import, still add fixed charges as mechanical add-on
    pnl$OPEX_OTHER[[i]] <- pnl$OPEX_OTHER[[i]] + add
  }
  fpa_pnl_derive(pnl)
}

#' Apply variable cost % of REV into COGS when mechanism on and COGS empty.
fpa_pnl_apply_variable <- function(pnl, assum_wide, config) {
  if (!fpa_mechanism_on(config, "variable_cost_pct_or_unit")) return(pnl)
  for (i in seq_len(nrow(pnl))) {
    p <- pnl$periode[[i]]
    if (abs(pnl$COGS[[i]]) > 1e-9) next
    pct <- fpa_assumption_num(assum_wide, p, "cout.variable.pct", default = 0)
    pnl$COGS[[i]] <- pnl$REV[[i]] * pct
  }
  fpa_pnl_derive(pnl)
}

#' Annual totals from monthly P&L for gate 3 reconciliation.
fpa_pnl_annual <- function(pnl) {
  if (!nrow(pnl)) return(NULL)
  pnl$year <- substr(pnl$periode, 1, 4)
  ids <- c(fpa_pnl_input_ids(), "GP", "EBITDA", "OI", "NI")
  agg <- aggregate(pnl[ids], by = list(year = pnl$year), FUN = sum, na.rm = TRUE)
  agg
}

#' Evidence labels for P&L columns.
fpa_pnl_labels <- function(pnl, imported_ids = character()) {
  labs <- character(0)
  for (id in fpa_pnl_input_ids()) {
    if (id %in% imported_ids) labs[[id]] <- FPA_LABEL_IMPORTED
    else labs[[id]] <- FPA_LABEL_MECHANICAL
  }
  labs[["GP"]] <- FPA_LABEL_MECHANICAL
  labs[["EBITDA"]] <- FPA_LABEL_MECHANICAL
  labs[["OI"]] <- FPA_LABEL_MECHANICAL
  labs[["NI"]] <- FPA_LABEL_MECHANICAL
  labs[["TAX"]] <- if ("TAX" %in% imported_ids) FPA_LABEL_IMPORTED else FPA_LABEL_ASSUMPTION
  labs
}
