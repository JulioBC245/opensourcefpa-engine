# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-run.R — Orchestrator: config → validate → P&L → WC → cash → BS → write
# LAST CHANGE: 2026-09-21 — live P&L from monthly file when present

#' Run the L1/L2 engine.
#' @param config_path Path to fpa-config.json
#' @param write If TRUE, write work/*.csv
#' @param stop_on_error If TRUE, stop when validation has errors
#' @return Named list with pnl, bs, cash, findings, etc.
fpa_run <- function(config_path, write = TRUE, stop_on_error = TRUE,
                    horizon_months = NULL, assumption_override = NULL,
                    line_scale = NULL) {
  config <- fpa_load_config(config_path)
  paths <- fpa_paths(config)
  config <- fpa_apply_local_overlay(config, paths)
  fpa_assumptions_reconcile(config, paths, config_path)
  config <- fpa_load_config(config_path)
  paths <- fpa_paths(config)
  config <- fpa_apply_local_overlay(config, paths)

  # Ensure work dirs
  dir.create(paths$work, showWarnings = FALSE, recursive = TRUE)
  dir.create(paths$reports, showWarnings = FALSE, recursive = TRUE)

  reads <- fpa_read_all_financial(config, paths)
  opening_vec <- NULL
  if (!is.null(reads$opening_bs) && isTRUE(reads$opening_bs$ok)) {
    opening_vec <- fpa_mapped_to_bs_vec(reads$opening_bs$mapped)
  }
  findings <- fpa_validate(config, reads, opening_vec)

  if (stop_on_error && fpa_has_errors(findings)) {
    msg <- fpa_format_findings(findings)
    stop(msg, call. = FALSE)
  }

  # Assumptions / decisions / volumes
  assum <- fpa_read_assumptions(config, paths)
  assum <- fpa_apply_assumption_override(assum, assumption_override)
  dec <- fpa_read_decisions(config, paths)
  vol <- fpa_read_volumes(config, paths)
  drivers <- fpa_read_drivers(config, paths)

  monthly_ok <- !is.null(reads$pnl_monthly) && isTRUE(reads$pnl_monthly$ok) &&
    !is.null(reads$pnl_monthly$mapped) && nrow(reads$pnl_monthly$mapped) > 0
  mapped <- if (monthly_ok) {
    reads$pnl_monthly$mapped
  } else if (!is.null(reads$pnl_actuals) && isTRUE(reads$pnl_actuals$ok)) {
    reads$pnl_actuals$mapped
  } else {
    NULL
  }

  budget_monthly <- fpa_budget_monthly(reads, config)

  grain <- if (monthly_ok) {
    "month"
  } else if (!is.null(reads$pnl_actuals)) {
    reads$pnl_actuals$grain
  } else {
    "unknown"
  }
  if (identical(grain, "year")) {
    # Annual path: build annual P&L only, no monthly BS roll from annual
    pnl <- fpa_build_pnl(mapped)
    # Treat period_end years as periode YYYY
    if (nrow(pnl)) {
      # already YYYY-MM from Dec 31
    }
    result <- list(
      config = config,
      paths = paths,
      findings = findings,
      grain = grain,
      pnl = pnl,
      bs = NULL,
      cash = NULL,
      opening = opening_vec,
      reads = reads,
      budget_monthly = budget_monthly,
      drivers = drivers
    )
    if (write) fpa_write_outputs(result, paths)
    return(result)
  }

  periods <- NULL
  empty_tail <- character()
  if (!is.null(mapped) && nrow(mapped)) {
    trimmed <- fpa_drop_empty_tail(mapped)
    mapped <- trimmed$mapped
    empty_tail <- trimmed$dropped
    periods <- sort(unique(format(as.Date(mapped$period_end), "%Y-%m")))
  }
  actual_periods <- periods
  horizon_override <- horizon_months
  if (is.null(horizon_override) && is.null(config$horizon_months) && length(empty_tail)) {
    # Empty import columns used to stop at fiscal year-end. A seasonal book
    # needs a longer path: 12, 18 or 24 months past the last close (default 18).
    horizon_override <- fpa_horizon_months_read(config)
  }
  horizon_n <- if (is.null(periods)) 0L else fpa_horizon_months(config, periods, horizon_override)
  if (horizon_n > 0L) periods <- fpa_extend_periods(periods, horizon_n)
  forecast_periods <- setdiff(periods %||% character(0), actual_periods %||% character(0))
  closed_through <- if (length(actual_periods)) max(as.character(actual_periods)) else ""
  drivers <- fpa_drivers_apply_clock(drivers, closed_through, forecast_periods)
  filled_months <- unique(unlist(lapply(drivers$groups, function(g) g$filled %||% character(0)), use.names = FALSE))
  if (length(filled_months)) {
    findings <- rbind(findings, fpa_finding(
      "driver_living_filled", "info",
      fpa_txt("drv_living_filled", paste(sort(filled_months), collapse = ", "))
    ))
  }
  dec_dated <- fpa_decisions_dated(dec$data, periods %||% character(0))
  dec$ledger <- fpa_decisions_ledger(dec$data, periods %||% character(0))
  dec$data <- fpa_decisions_open(dec$data, actual_periods)
  first_open <- if (length(forecast_periods)) min(forecast_periods) else NULL

  da_extra <- fpa_da_from_capex(dec_dated, periods %||% character(0), not_before = first_open)
  overlays <- list()
  if (isTRUE(vol$ok) && !is.null(vol$revenue) && length(vol$revenue)) {
    overlays$REV <- vol$revenue
  }
  if (isTRUE(vol$ok) && !is.null(vol$payroll) && length(vol$payroll)) {
    overlays$PAYROLL <- vol$payroll
  }
  if (length(actual_periods)) {
    for (nm in names(overlays)) {
      keep <- intersect(names(overlays[[nm]]), actual_periods)
      if (length(keep)) overlays[[nm]][keep] <- NA_real_
    }
  }

  pnl <- fpa_build_pnl(mapped, periods = periods, overlays = overlays)
  pnl <- fpa_apply_standing_lock(pnl, paths)
  if (isTRUE(assum$ok)) {
    pnl <- fpa_pnl_apply_fixed(pnl, assum$wide, config)
    pnl <- fpa_pnl_apply_variable(pnl, assum$wide, config)
    pnl <- fpa_pnl_apply_tax(pnl, assum$wide, config)
  }
  pnl <- fpa_pnl_apply_growth(pnl, assum$wide, config, forecast_periods)
  pnl <- fpa_pnl_apply_forecast_assumptions(pnl, assum$wide, forecast_periods)
  seasonal_ids <- c("D_A", "FIN")
  for (row in config$budget_phasing$rows %||% list()) {
    if (identical(as.character(row$method %||% ""), "seasonal_index")) {
      seasonal_ids <- c(seasonal_ids, as.character(row$line_id %||% ""))
    }
  }
  pnl <- fpa_pnl_apply_seasonal_forecast(pnl, forecast_periods, unique(seasonal_ids[nzchar(seasonal_ids)]))
  history_pnl <- fpa_history_pnl(config, paths)
  pnl <- fpa_pnl_apply_seasonal_level(
    pnl, history_pnl, forecast_periods, actual_periods,
    c("REV", "OTHER_OP_INC", "COGS", "PAYROLL", "OPEX_OTHER", "D_A", "FIN", "EXCEP")
  )
  driver_rows <- fpa_line_driver_rows(config)
  line_history <- if (nrow(driver_rows)) fpa_line_history(config, paths) else NULL
  line_volume <- fpa_living_fill_vector(
    fpa_volume_by_month(vol$data), closed_through, forecast_periods
  )
  hist_rev <- if (!is.null(history_pnl) && nrow(history_pnl)) {
    setNames(as.numeric(history_pnl$REV), history_pnl$periode)
  } else {
    setNames(numeric(0), character(0))
  }
  line_fit <- fpa_line_driver_fit(
    driver_rows, line_history, line_volume, hist_rev,
    before = if (length(forecast_periods)) min(forecast_periods) else NULL
  )
  # A month with no volume row is a repeat of the statement. Do not refit its costs
  # from a night count copied forward, or the repeat no longer matches the month.
  volume_months <- character()
  if (isTRUE(vol$ok) && !is.null(vol$data) && nrow(vol$data) && "periode" %in% names(vol$data)) {
    volume_months <- unique(as.character(vol$data$periode))
  }
  driven_periods <- if (length(volume_months)) {
    intersect(as.character(forecast_periods), volume_months)
  } else {
    forecast_periods
  }
  pnl <- fpa_pnl_apply_line_drivers(pnl, line_fit, line_history, line_volume, driven_periods)
  pnl <- fpa_pnl_add_calendar_da(pnl, da_extra, forecast_periods)
  if (isTRUE(assum$ok)) {
    pnl <- fpa_pnl_apply_variable(pnl, assum$wide, config)
    pnl <- fpa_pnl_apply_tax(pnl, assum$wide, config)
  }
  imported_ids <- if (!is.null(mapped) && nrow(mapped)) unique(mapped$functional_label) else character(0)
  explained <- c(seasonal_ids, "EXCEP")
  if (fpa_mechanism_on(config, "effective_tax_rate")) explained <- c(explained, "TAX")
  if (fpa_mechanism_on(config, "variable_cost_pct_or_unit")) explained <- c(explained, "COGS")
  if (fpa_mechanism_on(config, "capacity_x_util_x_rate") || fpa_mechanism_on(config, "qty_x_price") ||
      fpa_mechanism_on(config, "revenue_growth_pct")) explained <- c(explained, "REV")
  if (fpa_mechanism_on(config, "headcount_x_cost")) explained <- c(explained, "PAYROLL")
  if (fpa_mechanism_on(config, "fixed_monthly_expense")) explained <- c(explained, "OPEX_OTHER")
  if (fpa_mechanism_on(config, "capex_schedule")) explained <- c(explained, "D_A")
  refusals <- fpa_forecast_refusals(pnl, actual_periods, imported_ids, unique(explained))
  if (length(refusals)) {
    for (msg in refusals) {
      findings <- rbind(findings, fpa_finding("forecast_refused", "error", msg))
    }
    if (stop_on_error) stop(fpa_format_findings(findings), call. = FALSE)
  }
  pnl <- fpa_apply_line_scale(pnl, line_scale, forecast_periods)

  periods <- pnl$periode
  # Manual menu ledger: convention path, never a config key. Applied after WC
  # so a nouveau cash item is not delayed by DSO. Timing holds AR/AP.
  man <- fpa_manual_read(config)
  man$applied <- fpa_manual_applied(config)
  man$ledger <- fpa_manual_ledger(man$data, periods %||% character(0), actual_periods %||% character(0))
  man_used <- fpa_manual_used(man$data, periods %||% character(0), actual_periods %||% character(0))
  if (!isTRUE(man$applied)) {
    man_used <- fpa_manual_empty()
    if (nrow(man$ledger)) {
      hit <- as.character(man$ledger$state) == "used"
      man$ledger$state[hit] <- "excluded"
      man$ledger$reason[hit] <- "hors calcul"
    }
  }
  for (i in seq_len(nrow(man$ledger))) {
    if (!identical(as.character(man$ledger$state[[i]]), "excluded")) next
    reason <- as.character(man$ledger$reason[[i]] %||% "")
    if (identical(reason, "retire") || identical(reason, "meme mois (aucun effet)") || identical(reason, "hors calcul")) next
    findings <- rbind(findings, fpa_finding(
      "manual_excluded", "warning",
      paste0(
        "Saisie ", as.character(man$ledger$id[[i]]), " exclue: ", reason,
        if (nzchar(as.character(man$ledger$libelle[[i]] %||% ""))) {
          paste0(" (", man$ledger$libelle[[i]], ")")
        } else {
          ""
        }
      )
    ))
  }

  wc <- fpa_build_wc(pnl, assum$wide, config, opening = opening_vec)
  pnl <- fpa_manual_apply_nouveau(pnl, man_used, assum$wide, config)
  manual_keys <- fpa_manual_touched_keys(man_used)
  wc <- fpa_manual_apply_decalage(wc, man_used, periods)
  wc_total <- fpa_wc_cash_impact(wc, periods, opening = opening_vec)
  release <- fpa_wc_opening_release(wc, periods, opening_vec)
  wc_impact <- wc_total - release
  for (msg in fpa_wc_opening_flags(wc, periods, opening_vec, assum$wide)) {
    findings <- rbind(findings, fpa_finding("wc_opening", "warning", msg))
  }
  capex <- fpa_capex_by_month(dec_dated, periods)
  debt <- fpa_debt_cash_by_month(dec_dated, periods)
  opening_cash <- fpa_open_num(opening_vec, "CASH")
  cash <- fpa_build_cash_bridge(pnl, wc_impact, capex, debt$net, opening_cash, opening_release = release)
  bs <- fpa_build_bs(pnl, wc, cash, capex, debt, opening_vec %||% list())

  ident <- fpa_assert_identity(bs)
  if (!ident$ok) {
    findings <- rbind(findings, fpa_finding(
      "identity_break", "error",
      paste0(
        "L'identite A = L + E est rompue (pas de plug). ",
        paste(ident$messages, collapse = " | ")
      )
    ))
    if (stop_on_error) stop(fpa_format_findings(findings), call. = FALSE)
  }

  label_cell <- function(line, periode) {
    derived <- line %in% c("GP", "EBITDA", "OI", "NI")
    if (paste(periode, line, sep = "|") %in% manual_keys) return(FPA_LABEL_MANUAL)
    if (periode %in% actual_periods) {
      if (derived) return(FPA_LABEL_MECHANICAL)
      if (line %in% imported_ids) return(FPA_LABEL_IMPORTED)
      if (identical(line, "TAX") && !("TAX" %in% imported_ids)) return(FPA_LABEL_ASSUMPTION)
      return(FPA_LABEL_MECHANICAL)
    }
    if (derived) return(FPA_LABEL_MECHANICAL)
    if (!is.null(assum$wide) && paste0("prevision.", line) %in% names(assum$wide)) {
      return(FPA_LABEL_ASSUMPTION)
    }
    FPA_LABEL_MECHANICAL
  }
  pnl_long <- fpa_label_statement(pnl, label_cell)

  bs_labs <- setNames(
    rep(FPA_LABEL_MECHANICAL, length(fpa_bs_input_ids())),
    fpa_bs_input_ids()
  )
  for (id in intersect(names(opening_vec %||% list()), fpa_bs_input_ids())) {
    bs_labs[[id]] <- FPA_LABEL_IMPORTED
  }
  # Cash and WC and equity roll are mechanical after open
  bs_labs[["CASH"]] <- FPA_LABEL_MECHANICAL
  bs_labs[["AR"]] <- FPA_LABEL_MECHANICAL
  bs_labs[["AP"]] <- FPA_LABEL_MECHANICAL
  bs_labs[["INV"]] <- FPA_LABEL_MECHANICAL
  bs_labs[["EQUITY"]] <- FPA_LABEL_MECHANICAL
  bs_long <- fpa_label_statement(bs, bs_labs)

  cash_long <- data.frame(
    periode = cash$periode,
    line_id = "delta_cash",
    montant = cash$delta_cash,
    evidence_label = FPA_LABEL_MECHANICAL,
    stringsAsFactors = FALSE
  )

  period_status <- fpa_period_status(pnl$periode, actual_periods)
  variance <- fpa_variance_build(pnl, budget_monthly, vol$data, config, status = period_status)
  findings <- rbind(findings, fpa_variance_findings(variance))
  scenarios <- fpa_scenarios_build(
    pnl, cash, config, actual_periods, assum$wide, budget_monthly
  )
  scenario_ranks <- fpa_scenario_ranks(pnl, config, actual_periods)

  result <- list(
    config = config,
    paths = paths,
    findings = findings,
    grain = grain,
    pnl = pnl,
    pnl_long = pnl_long,
    bs = bs,
    bs_long = bs_long,
    cash = cash,
    cash_long = cash_long,
    wc = wc,
    opening = opening_vec,
    identity = ident,
    reads = reads,
    budget_monthly = budget_monthly,
    variance = variance,
    scenarios = scenarios,
    scenario_ranks = scenario_ranks,
    line_drivers = line_fit,
    period_status = period_status,
    assumptions = assum,
    decisions = dec,
    manual = man,
    drivers = drivers
  )
  if (write) fpa_write_outputs(result, paths)
  result
}

fpa_write_outputs <- function(result, paths = result$paths) {
  if (!is.null(result$pnl) && nrow(result$pnl)) {
    utils::write.csv(result$pnl, file.path(paths$work, "pnl-monthly.csv"), row.names = FALSE)
  }
  if (!is.null(result$pnl_long) && nrow(result$pnl_long)) {
    utils::write.csv(result$pnl_long, file.path(paths$work, "pnl-monthly-long.csv"), row.names = FALSE)
  }
  if (!is.null(result$bs) && nrow(result$bs)) {
    utils::write.csv(result$bs, file.path(paths$work, "bs-monthly.csv"), row.names = FALSE)
  }
  if (!is.null(result$bs_long) && nrow(result$bs_long)) {
    utils::write.csv(result$bs_long, file.path(paths$work, "bs-monthly-long.csv"), row.names = FALSE)
  }
  if (!is.null(result$cash) && nrow(result$cash)) {
    utils::write.csv(result$cash, file.path(paths$work, "cash-bridge-monthly.csv"), row.names = FALSE)
  }
  if (!is.null(result$budget_monthly) && nrow(result$budget_monthly)) {
    utils::write.csv(result$budget_monthly, file.path(paths$work, "budget-monthly.csv"), row.names = FALSE)
  }
  if (!is.null(result$variance) && nrow(result$variance)) {
    utils::write.csv(result$variance, file.path(paths$work, "variance-monthly.csv"), row.names = FALSE)
  }
  if (!is.null(result$scenarios) && nrow(result$scenarios)) {
    utils::write.csv(result$scenarios, file.path(paths$work, "scenarios-monthly.csv"), row.names = FALSE)
  }
  if (!is.null(result$scenario_ranks) && nrow(result$scenario_ranks)) {
    utils::write.csv(result$scenario_ranks, file.path(paths$work, "scenario-ranks.csv"), row.names = FALSE)
  }
  if (!is.null(result$line_drivers) && nrow(result$line_drivers)) {
    utils::write.csv(result$line_drivers, file.path(paths$work, "line-drivers.csv"), row.names = FALSE)
  }
  if (!is.null(result$period_status) && nrow(result$period_status)) {
    utils::write.csv(result$period_status, file.path(paths$work, "period-status.csv"), row.names = FALSE)
  }
  if (!is.null(result$findings) && nrow(result$findings)) {
    utils::write.csv(result$findings, file.path(paths$reports, "findings.csv"), row.names = FALSE)
  }
  invisible(TRUE)
}
