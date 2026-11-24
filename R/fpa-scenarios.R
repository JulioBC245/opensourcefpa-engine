# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-scenarios.R — Low, base and high from the widths in fpa-config.json.
# LAST CHANGE: 2026-09-23 — budget column filled; one-input ranking written beside the cases
# The JSON names the inputs and the percent. This file applies them.
# "low" is the unfavourable case: revenue inputs fall, cost inputs rise,
# customer and stock days rise, supplier days fall. "high" is the reverse.
# "base" is the forecast already computed. Actual months are left unchanged.
# One budget stays the reference. This file does not write a second budget.

fpa_scenario_range_table <- function(config) {
  empty <- data.frame(
    id = character(), label = character(),
    low_pct = numeric(), high_pct = numeric(),
    stringsAsFactors = FALSE
  )
  sr <- config$scenario_ranges
  if (is.null(sr)) return(empty)
  inp <- sr$inputs
  if (is.null(inp) || !length(inp)) return(empty)
  if (is.data.frame(inp)) {
    if (!nrow(inp) || !("id" %in% names(inp))) return(empty)
    labs <- if ("label" %in% names(inp)) as.character(inp$label) else as.character(inp$id)
    return(data.frame(
      id = as.character(inp$id),
      label = labs,
      low_pct = suppressWarnings(as.numeric(inp$low_pct)),
      high_pct = suppressWarnings(as.numeric(inp$high_pct)),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(inp, function(x) {
    data.frame(
      id = as.character(x$id %||% ""),
      label = as.character(x$label %||% x$id %||% ""),
      low_pct = suppressWarnings(as.numeric(x$low_pct %||% NA)),
      high_pct = suppressWarnings(as.numeric(x$high_pct %||% NA)),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out[nzchar(out$id), , drop = FALSE]
}

#' Unfavourable move of the input itself: "up" or "down".
fpa_scenario_unfav_dir <- function(id) {
  if (id %in% c(
    "cout.variable.pct", "effectif", "cout_moyen", "charges.fixes", "dso", "dio"
  )) {
    return("up")
  }
  "down"
}

#' Multiplier on the input. low = unfavourable, high = favourable, base = 1.
fpa_scenario_input_factor <- function(id, pct, case_id) {
  if (!identical(case_id, "low") && !identical(case_id, "high")) return(1)
  pct <- suppressWarnings(as.numeric(pct))
  if (!is.finite(pct) || pct <= 0) return(1)
  pct <- min(pct, 90)
  unfav_up <- identical(fpa_scenario_unfav_dir(id), "up")
  rise <- if (identical(case_id, "low")) unfav_up else !unfav_up
  if (rise) 1 + pct / 100 else 1 - pct / 100
}

fpa_scenario_line_of <- function(id) {
  if (id %in% c(
    "taux_utilisation", "tarif", "quantite", "prix",
    "clients", "depense_moyenne", "croissance.ca"
  )) {
    return("REV")
  }
  if (identical(id, "cout.variable.pct")) return("COGS")
  if (id %in% c("effectif", "cout_moyen")) return("PAYROLL")
  if (identical(id, "charges.fixes")) return("OPEX_OTHER")
  NA_character_
}

#' Shock forecast months. Costs that follow revenue move with it, then the cost width.
fpa_scenario_pnl <- function(pnl, inputs, case_id, actual_periods = character(), config = NULL) {
  out <- pnl
  if (!nrow(out) || !nrow(inputs) || identical(case_id, "base")) return(fpa_pnl_derive(out))
  fc <- !(as.character(out$periode) %in% as.character(actual_periods))
  if (!any(fc)) return(fpa_pnl_derive(out))
  rev_f <- rep(1, nrow(out))
  cogs_f <- rep(1, nrow(out))
  pay_f <- rep(1, nrow(out))
  opex_f <- rep(1, nrow(out))
  for (i in seq_len(nrow(inputs))) {
    id <- inputs$id[[i]]
    pct <- if (identical(case_id, "low")) inputs$low_pct[[i]] else inputs$high_pct[[i]]
    f <- fpa_scenario_input_factor(id, pct, case_id)
    line <- fpa_scenario_line_of(id)
    if (identical(line, "REV")) rev_f <- rev_f * f
    if (identical(line, "COGS")) cogs_f <- cogs_f * f
    if (identical(line, "PAYROLL")) pay_f <- pay_f * f
    if (identical(line, "OPEX_OTHER")) opex_f <- opex_f * f
  }
  base_rev <- suppressWarnings(as.numeric(pnl$REV))
  out$REV[fc] <- base_rev[fc] * rev_f[fc]
  tracks <- !is.null(config) && isTRUE(tryCatch(
    fpa_mechanism_on(config, "variable_cost_pct_or_unit"),
    error = function(e) FALSE
  ))
  ratio <- rep(1, nrow(out))
  if (tracks) {
    ok <- is.finite(base_rev) & abs(base_rev) > 0.5
    ratio[ok] <- out$REV[ok] / base_rev[ok]
  }
  out$COGS[fc] <- suppressWarnings(as.numeric(pnl$COGS))[fc] * ratio[fc] * cogs_f[fc]
  out$PAYROLL[fc] <- suppressWarnings(as.numeric(pnl$PAYROLL))[fc] * pay_f[fc]
  out$OPEX_OTHER[fc] <- suppressWarnings(as.numeric(pnl$OPEX_OTHER))[fc] * opex_f[fc]
  fpa_pnl_derive(out)
}

#' Cash versus the base path: profit difference, plus working capital tied by days.
fpa_scenario_cash_close <- function(pnl_base, pnl_case, cash, inputs, case_id, actual_periods, assum_wide = NULL) {
  if (is.null(cash) || !nrow(cash) || !("cash_close" %in% names(cash))) return(rep(NA_real_, nrow(pnl_case)))
  close <- suppressWarnings(as.numeric(cash$cash_close[match(pnl_case$periode, cash$periode)]))
  if (!nrow(inputs) || identical(case_id, "base")) return(close)
  fc <- !(as.character(pnl_case$periode) %in% as.character(actual_periods))
  dni <- suppressWarnings(as.numeric(pnl_case$NI) - as.numeric(pnl_base$NI))
  dni[!fc | !is.finite(dni)] <- 0
  extra_tied <- rep(0, nrow(pnl_case))
  day_ids <- c(dso = "dso", dpo = "dpo", dio = "dio")
  if (!is.null(assum_wide)) {
    for (i in seq_len(nrow(inputs))) {
      key <- unname(day_ids[inputs$id[[i]]])
      if (is.na(key) || !nzchar(key)) next
      pct <- if (identical(case_id, "low")) inputs$low_pct[[i]] else inputs$high_pct[[i]]
      f <- fpa_scenario_input_factor(inputs$id[[i]], pct, case_id)
      for (r in which(fc)) {
        p <- pnl_case$periode[[r]]
        days <- tryCatch(
          fpa_assumption_num(assum_wide, p, key, default = NA_real_),
          error = function(e) NA_real_
        )
        if (!is.finite(days)) next
        base_amt <- if (identical(key, "dso")) pnl_base$REV[[r]] else pnl_base$COGS[[r]]
        case_amt <- if (identical(key, "dso")) pnl_case$REV[[r]] else pnl_case$COGS[[r]]
        md <- fpa_wc_month_days(p)
        if (!is.finite(md) || md < 1) next
        stock_base <- base_amt * days / md
        stock_case <- case_amt * days * f / md
        # AR and stock tie cash. Payables free it.
        sign <- if (identical(key, "dpo")) -1 else 1
        extra_tied[[r]] <- extra_tied[[r]] + sign * (stock_case - stock_base)
      }
    }
  }
  extra_tied[!is.finite(extra_tied)] <- 0
  # Profit adds cash. A higher stock of receivables uses cash, versus the base path.
  shift <- cumsum(dni) - extra_tied
  close + shift
}

#' Operating profit of the voted budget, one row per month.
#' Accepts the long budget table (functional_label, montant) or a wide table with OI.
fpa_scenario_budget_oi <- function(budget) {
  if (is.null(budget) || !nrow(budget)) return(NULL)
  if (all(c("periode", "OI") %in% names(budget))) {
    return(data.frame(
      periode = as.character(budget$periode),
      OI = suppressWarnings(as.numeric(budget$OI)),
      stringsAsFactors = FALSE
    ))
  }
  need <- c("periode", "functional_label", "montant")
  if (!all(need %in% names(budget))) return(NULL)
  sl <- budget[as.character(budget$functional_label) == "OI", , drop = FALSE]
  if (nrow(sl)) {
    pers <- unique(as.character(sl$periode))
    return(data.frame(
      periode = pers,
      OI = vapply(pers, function(p) {
        sum(suppressWarnings(as.numeric(sl$montant[as.character(sl$periode) == p])), na.rm = TRUE)
      }, numeric(1)),
      stringsAsFactors = FALSE
    ))
  }
  parts <- c("REV", "OTHER_OP_INC", "COGS", "PAYROLL", "OPEX_OTHER", "D_A")
  have <- budget[as.character(budget$functional_label) %in% parts, , drop = FALSE]
  if (!nrow(have)) return(NULL)
  pers <- unique(as.character(have$periode))
  data.frame(
    periode = pers,
    OI = vapply(pers, function(p) {
      rows <- have[as.character(have$periode) == p, , drop = FALSE]
      amt <- function(line) {
        hit <- suppressWarnings(as.numeric(rows$montant[as.character(rows$functional_label) == line]))
        if (!length(hit)) return(0)
        sum(hit, na.rm = TRUE)
      }
      amt("REV") + amt("OTHER_OP_INC") - amt("COGS") - amt("PAYROLL") - amt("OPEX_OTHER") - amt("D_A")
    }, numeric(1)),
    stringsAsFactors = FALSE
  )
}

#' Each input shocked alone. oi_span is haut minus bas on the open months.
fpa_scenario_ranks <- function(pnl, config, actual_periods = character()) {
  empty <- data.frame(
    id = character(), label = character(),
    low_pct = numeric(), high_pct = numeric(),
    oi_low = numeric(), oi_base = numeric(), oi_high = numeric(), oi_span = numeric(),
    stringsAsFactors = FALSE
  )
  inputs <- fpa_scenario_range_table(config)
  if (is.null(pnl) || !nrow(pnl) || !nrow(inputs) || !("OI" %in% names(pnl))) return(empty)
  fc <- !(as.character(pnl$periode) %in% as.character(actual_periods))
  if (!any(fc)) return(empty)
  base_oi <- sum(suppressWarnings(as.numeric(pnl$OI))[fc], na.rm = TRUE)
  rows <- lapply(seq_len(nrow(inputs)), function(i) {
    one <- inputs[i, , drop = FALSE]
    low <- fpa_scenario_pnl(pnl, one, "low", actual_periods, config)
    high <- fpa_scenario_pnl(pnl, one, "high", actual_periods, config)
    oi_low <- sum(suppressWarnings(as.numeric(low$OI))[fc], na.rm = TRUE)
    oi_high <- sum(suppressWarnings(as.numeric(high$OI))[fc], na.rm = TRUE)
    data.frame(
      id = one$id[[1]],
      label = one$label[[1]],
      low_pct = one$low_pct[[1]],
      high_pct = one$high_pct[[1]],
      oi_low = oi_low,
      oi_base = base_oi,
      oi_high = oi_high,
      oi_span = oi_high - oi_low,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  out <- out[order(-abs(out$oi_span), out$id), , drop = FALSE]
  rownames(out) <- NULL
  out
}

#' Sum of operating profit on the open months, for the Synthèse sentence.
#' Budget is published only when every open month has a finite budget. Missing is not zero.
fpa_scenario_open_totals <- function(scenarios, status = NULL) {
  out <- list(
    oi_low = NA_real_, oi_base = NA_real_, oi_high = NA_real_, oi_budget = NA_real_,
    n_open = 0L, n_budget = 0L, n_budget_missing = 0L,
    first = "", last = ""
  )
  if (is.null(scenarios) || !nrow(scenarios) || !("case" %in% names(scenarios))) return(out)
  keep <- rep(TRUE, nrow(scenarios))
  if (!is.null(status) && nrow(status) && all(c("periode", "status") %in% names(status))) {
    open <- as.character(status$periode[as.character(status$status) == "forecast"])
    keep <- as.character(scenarios$periode) %in% open
  }
  if (!any(keep)) return(out)
  sum_case <- function(case_id) {
    sl <- scenarios[keep & as.character(scenarios$case) == case_id, , drop = FALSE]
    if (!nrow(sl)) return(NA_real_)
    sum(suppressWarnings(as.numeric(sl$OI)), na.rm = TRUE)
  }
  out$oi_low <- sum_case("low")
  out$oi_base <- sum_case("base")
  out$oi_high <- sum_case("high")
  base <- scenarios[keep & as.character(scenarios$case) == "base", , drop = FALSE]
  pers <- sort(unique(as.character(base$periode)))
  out$n_open <- length(pers)
  if (length(pers)) {
    out$first <- pers[[1]]
    out$last <- pers[[length(pers)]]
  }
  if (nrow(base) && "oi_vs_budget" %in% names(base)) {
    gap <- suppressWarnings(as.numeric(base$oi_vs_budget))
    oi <- suppressWarnings(as.numeric(base$OI))
    bud <- oi - gap
    covered <- is.finite(gap) & is.finite(oi)
    out$n_budget <- sum(covered)
    out$n_budget_missing <- sum(!covered)
    if (length(bud) && all(covered)) out$oi_budget <- sum(bud)
  } else if (nrow(base)) {
    out$n_budget_missing <- nrow(base)
  }
  out
}

fpa_scenario_sentence <- function(scenarios, status = NULL) {
  tot <- fpa_scenario_open_totals(scenarios, status)
  if (!is.finite(tot$oi_base)) return("")
  eur <- function(x) format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
  win <- ""
  if (nzchar(tot$first) && nzchar(tot$last)) {
    win <- if (identical(tot$first, tot$last)) tot$first else paste0(tot$first, " \u2013 ", tot$last)
  }
  if (is.finite(tot$oi_budget)) {
    if (nzchar(win)) {
      return(fpa_txt("scen_band_win", win, eur(tot$oi_base), eur(tot$oi_low), eur(tot$oi_high), eur(tot$oi_budget)))
    }
    return(fpa_txt("scen_band", eur(tot$oi_base), eur(tot$oi_low), eur(tot$oi_high), eur(tot$oi_budget)))
  }
  if (isTRUE(tot$n_budget_missing > 0L) && isTRUE(tot$n_open > 0L)) {
    if (nzchar(win)) {
      return(fpa_txt(
        "scen_band_partial_budget_win", win, eur(tot$oi_base), eur(tot$oi_low), eur(tot$oi_high),
        tot$n_budget_missing, tot$n_open
      ))
    }
    return(fpa_txt(
      "scen_band_partial_budget", eur(tot$oi_base), eur(tot$oi_low), eur(tot$oi_high),
      tot$n_budget_missing, tot$n_open
    ))
  }
  if (nzchar(win)) {
    return(fpa_txt("scen_band_nobudget_win", win, eur(tot$oi_base), eur(tot$oi_low), eur(tot$oi_high)))
  }
  fpa_txt("scen_band_nobudget", eur(tot$oi_base), eur(tot$oi_low), eur(tot$oi_high))
}

fpa_scenarios_build <- function(pnl, cash = NULL, config = NULL, actual_periods = character(), assum_wide = NULL, budget = NULL) {
  empty <- data.frame()
  if (is.null(pnl) || !nrow(pnl) || is.null(config)) return(empty)
  inputs <- fpa_scenario_range_table(config)
  if (!nrow(inputs)) return(empty)
  pieces <- lapply(c("low", "base", "high"), function(case_id) {
    shocked <- fpa_scenario_pnl(pnl, inputs, case_id, actual_periods, config)
    cash_close <- fpa_scenario_cash_close(
      pnl, shocked, cash, inputs, case_id, actual_periods, assum_wide
    )
    oi_budget <- rep(NA_real_, nrow(shocked))
    budget_oi <- fpa_scenario_budget_oi(budget)
    if (!is.null(budget_oi)) {
      oi_budget <- budget_oi$OI[match(as.character(shocked$periode), budget_oi$periode)]
    }
    data.frame(
      case = case_id,
      periode = shocked$periode,
      REV = shocked$REV,
      COGS = shocked$COGS,
      PAYROLL = shocked$PAYROLL,
      OPEX_OTHER = shocked$OPEX_OTHER,
      OI = shocked$OI,
      NI = shocked$NI,
      cash_close = cash_close,
      oi_vs_budget = shocked$OI - oi_budget,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, pieces)
  rownames(out) <- NULL
  out
}
