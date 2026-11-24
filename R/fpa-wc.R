# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-wc.R — Working capital from DSO / DPO / DIO
# LAST CHANGE: 2026-10-05 — days in the month, not 365; opening release split out
#
# AP is not materials alone. A hotel's supplier invoices sit in purchases and in
# other external charges. Wages are not a supplier payable: with no payroll-days
# parameter on file, a 15-day accrual is the month-end convention (net pay and
# social charges due just after the close).
#
# Each balance is this month's flow times the delay, divided by the number of
# days in that month. A delay longer than the month counts more than one month
# of that month's flow. It does not reach back into the previous month.

FPA_WC_PAYROLL_DAYS <- 15

#' Days in the month of a YYYY-MM period. February is 28 or 29.
fpa_wc_month_days <- function(periode) {
  start <- as.Date(paste0(substr(as.character(periode), 1, 7), "-01"))
  if (is.na(start)) return(NA_integer_)
  as.integer(seq(start, by = "month", length.out = 2)[2] - start)
}

#' Build monthly AR, AP, INV from P&L and assumption days.
#' AR_t = REV_t * dso / days_in_month
#' AP_t = (COGS_t + OPEX_OTHER_t) * dpo / days_in_month
#' INV_t = COGS_t * dio / days_in_month
#' OTHER_CL_t = PAYROLL_t * 15 / days_in_month
fpa_build_wc <- function(pnl, assum_wide, config, opening = NULL) {
  periods <- pnl$periode
  ar <- setNames(rep(0, length(periods)), periods)
  ap <- setNames(rep(0, length(periods)), periods)
  inv <- setNames(rep(0, length(periods)), periods)
  ocl <- setNames(rep(0, length(periods)), periods)
  if (!fpa_mechanism_on(config, "dso_dpo_dio")) {
    # Hold opening balances constant if provided
    if (!is.null(opening)) {
      for (p in periods) {
        ar[[p]] <- fpa_open_num(opening, "AR")
        ap[[p]] <- fpa_open_num(opening, "AP")
        inv[[p]] <- fpa_open_num(opening, "INV")
        ocl[[p]] <- fpa_open_num(opening, "OTHER_CL")
      }
    }
    return(list(AR = ar, AP = ap, INV = inv, OTHER_CL = ocl))
  }
  for (i in seq_along(periods)) {
    p <- periods[[i]]
    dso <- fpa_assumption_num(assum_wide, p, "dso", default = 0)
    dpo <- fpa_assumption_num(assum_wide, p, "dpo", default = 0)
    dio <- fpa_assumption_num(assum_wide, p, "dio", default = 0)
    rev <- pnl$REV[[i]]
    cogs <- pnl$COGS[[i]]
    opex <- if ("OPEX_OTHER" %in% names(pnl)) pnl$OPEX_OTHER[[i]] else 0
    pay <- if ("PAYROLL" %in% names(pnl)) pnl$PAYROLL[[i]] else 0
    if (!is.finite(opex)) opex <- 0
    if (!is.finite(pay)) pay <- 0
    md <- fpa_wc_month_days(p)
    if (!is.finite(md) || md < 1) md <- NA_real_
    ar[[p]] <- rev * dso / md
    ap[[p]] <- (cogs + opex) * dpo / md
    inv[[p]] <- cogs * dio / md
    ocl[[p]] <- pay * FPA_WC_PAYROLL_DAYS / md
  }
  list(AR = ar, AP = ap, INV = inv, OTHER_CL = ocl)
}

#' Cash collected in the first month because the opening stock is not the stock
#' this month's own flow would carry. Later months are zero. A matching opening
#' gives zero.
fpa_wc_opening_release <- function(wc, periods, opening = NULL) {
  out <- setNames(rep(0, length(periods)), periods)
  if (!length(periods) || is.null(opening) || !length(opening)) return(out)
  p0 <- periods[[1]]
  ocl <- if (!is.null(wc$OTHER_CL)) wc$OTHER_CL else setNames(rep(0, length(periods)), periods)
  out[[p0]] <- (fpa_open_num(opening, "AR") - wc$AR[[p0]]) +
    (fpa_open_num(opening, "INV") - wc$INV[[p0]]) -
    (fpa_open_num(opening, "AP") - wc$AP[[p0]]) -
    (fpa_open_num(opening, "OTHER_CL") - ocl[[p0]])
  out
}

#' One sentence per opening item whose gap is more than half the opening stock.
fpa_wc_opening_flags <- function(wc, periods, opening = NULL, assum_wide = NULL) {
  if (!length(periods) || is.null(opening) || !length(opening)) return(character(0))
  p0 <- periods[[1]]
  specs <- list(
    list(id = "AR", label_fr = fpa_txt("loc_receivables"), label_en = "receivables", days_id = "dso"),
    list(id = "INV", label_fr = "stocks", label_en = "stock", days_id = "dio"),
    list(id = "AP", label_fr = "dettes fournisseurs", label_en = "payables", days_id = "dpo"),
    list(id = "OTHER_CL", label_fr = "dettes de personnel", label_en = "payroll accruals", days_id = NULL)
  )
  msgs <- character(0)
  for (s in specs) {
    open_v <- fpa_open_num(opening, s$id, default = NA_real_)
    bal <- if (identical(s$id, "OTHER_CL")) wc$OTHER_CL[[p0]] else wc[[s$id]][[p0]]
    if (!is.finite(open_v) || abs(open_v) < 1 || !is.finite(bal)) next
    if (abs(open_v - bal) <= 0.5 * abs(open_v)) next
    days <- if (is.null(s$days_id)) FPA_WC_PAYROLL_DAYS else {
      fpa_assumption_num(assum_wide, p0, s$days_id, default = NA_real_)
    }
    eur <- function(x) format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
    days_txt <- if (is.finite(days)) as.character(round(days)) else "n/d"
    msgs <- c(msgs, if (exists("fpa_txt", mode = "function")) {
      fpa_txt("wc_open_flag", eur(open_v), s$label_fr, days_txt, eur(bal), p0, eur(abs(open_v - bal)), p0)
    } else {
      sprintf(
        "Le bilan d'ouverture porte %s de %s. Avec %s jours de délai, le modèle en attend %s en %s. %s sont dénoués en %s. Vérifier le bilan d'ouverture ou le délai.",
        eur(open_v), s$label_fr, days_txt, eur(bal), p0, eur(abs(open_v - bal)), p0
      )
    })
  }
  msgs
}

#' Delta WC for cash bridge: increase in AR/INV uses cash; increase in AP and
#' personnel accruals provides cash.
#' dWC_cash_impact = -dAR - dINV + dAP + dOTHER_CL
fpa_wc_cash_impact <- function(wc, periods, opening = NULL) {
  out <- setNames(rep(0, length(periods)), periods)
  if (!length(periods)) return(out)
  ocl <- if (!is.null(wc$OTHER_CL)) wc$OTHER_CL else setNames(rep(0, length(periods)), periods)
  open_ar <- fpa_open_num(opening, "AR", default = NA_real_)
  open_ap <- fpa_open_num(opening, "AP", default = NA_real_)
  open_inv <- fpa_open_num(opening, "INV", default = NA_real_)
  open_ocl <- fpa_open_num(opening, "OTHER_CL", default = NA_real_)
  prev_ar <- if (is.na(open_ar)) wc$AR[[periods[[1]]]] else open_ar
  prev_ap <- if (is.na(open_ap)) wc$AP[[periods[[1]]]] else open_ap
  prev_inv <- if (is.na(open_inv)) wc$INV[[periods[[1]]]] else open_inv
  prev_ocl <- if (is.na(open_ocl)) ocl[[periods[[1]]]] else open_ocl
  step <- function(p, ar0, ap0, inv0, ocl0) {
    -(wc$AR[[p]] - ar0) - (wc$INV[[p]] - inv0) + (wc$AP[[p]] - ap0) + (ocl[[p]] - ocl0)
  }
  if (!is.null(opening) && length(opening)) {
    p0 <- periods[[1]]
    out[[p0]] <- step(p0, fpa_open_num(opening, "AR"), fpa_open_num(opening, "AP"),
                      fpa_open_num(opening, "INV"), fpa_open_num(opening, "OTHER_CL"))
    prev_ar <- wc$AR[[p0]]
    prev_ap <- wc$AP[[p0]]
    prev_inv <- wc$INV[[p0]]
    prev_ocl <- ocl[[p0]]
    rest <- periods[-1]
  } else {
    out[[periods[[1]]]] <- 0
    rest <- periods[-1]
  }
  for (p in rest) {
    out[[p]] <- step(p, prev_ar, prev_ap, prev_inv, prev_ocl)
    prev_ar <- wc$AR[[p]]
    prev_ap <- wc$AP[[p]]
    prev_inv <- wc$INV[[p]]
    prev_ocl <- ocl[[p]]
  }
  out
}
