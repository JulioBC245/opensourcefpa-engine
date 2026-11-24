# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-decisions.R — investissements-emprunts.csv (capex / debt schedule)
# LAST CHANGE: 2026-09-18 — L1/L2

#' Read decisions calendar. Amounts always positive; type discriminates.
fpa_read_decisions <- function(config, paths = fpa_paths(config)) {
  if (!fpa_mechanism_on(config, "capex_schedule") &&
      !fpa_mechanism_on(config, "debt_schedule")) {
    return(list(ok = TRUE, path = NA_character_, data = fpa_decisions_empty(), error = NULL))
  }
  path <- fpa_resolve_path(config, "decisions", paths)
  if (is.na(path) || !file.exists(path)) {
    return(list(
      ok = FALSE, path = path, data = fpa_decisions_empty(), error = "missing",
      message = "Calendrier investissements/emprunts introuvable."
    ))
  }
  res <- fpa_read_raw_file(path)
  df <- if (is.null(res$error)) fpa_decisions_frame(res$data) else NULL
  if (is.null(df) && grepl("\\.csv$", path, ignore.case = TRUE)) {
    raw <- tryCatch(
      utils::read.csv(
        path, sep = ";", stringsAsFactors = FALSE,
        check.names = FALSE, fileEncoding = "UTF-8"
      ),
      error = function(e) NULL
    )
    df <- fpa_decisions_frame(raw)
  }
  if (is.null(df)) {
    return(list(
      ok = FALSE, path = path, data = fpa_decisions_empty(), error = "schema",
      message = "investissements-emprunts.csv: colonnes date, libelle, montant, type."
    ))
  }
  df$date <- as.Date(df$date)
  df$montant <- abs(suppressWarnings(as.numeric(gsub(",", ".", as.character(df$montant), fixed = TRUE))))
  df$type <- tolower(as.character(df$type))
  df$periode <- format(df$date, "%Y-%m")
  # Optional life_months for capex (gate 7). French files say duree_mois.
  if (!("life_months" %in% names(df))) {
    alias <- intersect(c("duree_mois", "duree", "life"), names(df))
    df$life_months <- if (length(alias)) {
      suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[alias[[1]]]]), fixed = TRUE)))
    } else {
      NA_real_
    }
  }
  if (!("interest_rate" %in% names(df))) df$interest_rate <- NA_real_
  if (!("break_cost" %in% names(df))) df$break_cost <- 0
  list(ok = TRUE, path = path, data = df, error = NULL)
}

#' A comma export stays as it is. A semicolon export (French Excel) is read too.
fpa_decisions_frame <- function(df) {
  if (is.null(df) || !ncol(df)) return(NULL)
  names(df) <- tolower(trimws(names(df)))
  need <- c("date", "libelle", "montant", "type")
  if (!all(need %in% names(df))) return(NULL)
  df
}

#' Events the scenario dock can still change: dated on a forecast month.
fpa_decisions_open <- function(df, actual_periods) {
  if (is.null(df) || !nrow(df) || !length(actual_periods)) return(df)
  df[!as.character(df$periode) %in% as.character(actual_periods), , drop = FALSE]
}

#' Every dated event inside the horizon. Cash in a closed month is reconstructed,
#' so a closed-month event still moves cash, fixed assets and debt.
fpa_decisions_dated <- function(df, periods) {
  led <- fpa_decisions_ledger(df, periods)
  if (!nrow(led)) return(df)
  if (is.null(df) || !nrow(df) || !nrow(led)) return(df)
  df[led$state == "used", , drop = FALSE]
}

#' One row per calendar event: used, or excluded with a reason.
fpa_decisions_ledger <- function(df, periods) {
  empty <- data.frame(
    libelle = character(), periode = character(), type = character(),
    state = character(), reason = character(), stringsAsFactors = FALSE
  )
  if (is.null(df) || !nrow(df)) return(empty)
  known <- c("capex", "dette_tirage", "dette_remboursement")
  rows <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    lib <- as.character(df$libelle[[i]] %||% "")
    per <- as.character(df$periode[[i]] %||% "")
    typ <- as.character(df$type[[i]] %||% "")
    bad_date <- !nzchar(per) || is.na(per) || identical(per, "NA") || is.na(df$date[[i]])
    reason <- if (bad_date) {
      "date illisible"
    } else if (!typ %in% known) {
      "type inconnu"
    } else if (!per %in% as.character(periods)) {
      "hors horizon"
    } else {
      ""
    }
    rows[[i]] <- data.frame(
      libelle = lib, periode = per, type = typ,
      state = if (nzchar(reason)) "excluded" else "used",
      reason = reason, stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

fpa_decisions_empty <- function() {
  data.frame(
    date = as.Date(character()),
    libelle = character(),
    montant = numeric(),
    type = character(),
    periode = character(),
    life_months = numeric(),
    interest_rate = numeric(),
    break_cost = numeric(),
    stringsAsFactors = FALSE
  )
}

#' Capex cash out by YYYY-MM.
fpa_capex_by_month <- function(decisions_df, periods) {
  out <- setNames(rep(0, length(periods)), periods)
  if (is.null(decisions_df) || !nrow(decisions_df)) return(out)
  cap <- decisions_df[decisions_df$type == "capex", , drop = FALSE]
  for (i in seq_len(nrow(cap))) {
    p <- cap$periode[[i]]
    if (p %in% names(out)) out[[p]] <- out[[p]] + cap$montant[[i]]
  }
  out
}

#' Debt draws (+) and repayments (-) by month; refinance nets in same month.
fpa_debt_cash_by_month <- function(decisions_df, periods) {
  draw <- setNames(rep(0, length(periods)), periods)
  repay <- setNames(rep(0, length(periods)), periods)
  break_cost <- setNames(rep(0, length(periods)), periods)
  if (is.null(decisions_df) || !nrow(decisions_df)) {
    return(list(draw = draw, repay = repay, break_cost = break_cost, net = draw - repay - break_cost))
  }
  for (i in seq_len(nrow(decisions_df))) {
    p <- decisions_df$periode[[i]]
    if (!(p %in% names(draw))) next
    t <- decisions_df$type[[i]]
    amt <- decisions_df$montant[[i]]
    if (t == "dette_tirage") draw[[p]] <- draw[[p]] + amt
    else if (t == "dette_remboursement") {
      repay[[p]] <- repay[[p]] + amt
      bc <- decisions_df$break_cost[[i]]
      if (!is.na(bc)) break_cost[[p]] <- break_cost[[p]] + bc
    }
  }
  list(draw = draw, repay = repay, break_cost = break_cost, net = draw - repay - break_cost)
}

#' Monthly D&A from capex events: life_months, starts month after purchase.
fpa_da_from_capex <- function(decisions_df, periods, not_before = NULL) {
  out <- setNames(rep(0, length(periods)), periods)
  if (is.null(decisions_df) || !nrow(decisions_df)) return(out)
  cap <- decisions_df[decisions_df$type == "capex", , drop = FALSE]
  floor_date <- if (!is.null(not_before) && nzchar(as.character(not_before)[1])) {
    as.Date(paste0(substr(as.character(not_before)[1], 1, 7), "-01"))
  } else {
    as.Date(NA)
  }
  for (i in seq_len(nrow(cap))) {
    life <- cap$life_months[[i]]
    if (is.na(life) || life <= 0) next
    monthly <- cap$montant[[i]] / life
    # start next month, or the first open month when the purchase is already closed
    start_date <- seq(as.Date(paste0(cap$periode[[i]], "-01")), by = "month", length.out = 2)[[2]]
    if (!is.na(floor_date) && start_date < floor_date) start_date <- floor_date
    for (k in seq_len(as.integer(life))) {
      d <- seq(start_date, by = "month", length.out = k)[[k]]
      pk <- format(d, "%Y-%m")
      if (pk %in% names(out)) out[[pk]] <- out[[pk]] + monthly
    }
  }
  out
}
