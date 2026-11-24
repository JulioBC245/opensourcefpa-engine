# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-volumes.R — volumes.csv, mechanism-gated columns
# LAST CHANGE: 2026-09-18 — L1/L2

#' Read ops volumes if any ops mechanism is on.
fpa_read_volumes <- function(config, paths = fpa_paths(config)) {
  ops_on <- any(vapply(
    c("qty_x_price", "customers_x_spend", "capacity_x_util_x_rate", "headcount_x_cost"),
    function(id) fpa_mechanism_on(config, id),
    logical(1)
  ))
  if (!ops_on) {
    return(list(ok = TRUE, path = NA_character_, data = NULL, revenue = NULL, payroll = NULL))
  }
  local <- list.files(paths$input, pattern = "^volumes.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
  local <- local[file.exists(local)]
  path <- if (length(local)) {
    local[[which.max(file.info(local)$size)]]
  } else {
    fpa_resolve_path(config, "ops_other", paths)
  }
  if (is.na(path) || !nzchar(as.character(path)) || !file.exists(path)) {
    return(list(ok = FALSE, path = path, data = NULL, error = "missing"))
  }
  res <- fpa_read_raw_file(path)
  if (!is.null(res$error)) return(list(ok = FALSE, path = path, data = NULL, error = res$error))
  df <- res$data
  names(df) <- tolower(gsub(" ", "_", names(df)))
  if (!("periode" %in% names(df))) {
    return(list(ok = FALSE, path = path, data = NULL, error = "schema"))
  }
  df$periode <- as.character(df$periode)
  rev <- fpa_volumes_revenue(df, config)
  pay <- fpa_volumes_payroll(df, config)
  list(ok = TRUE, path = path, data = df, revenue = rev, payroll = pay, error = NULL)
}

fpa_volumes_revenue <- function(df, config) {
  periods <- sort(unique(df$periode))
  out <- setNames(rep(0, length(periods)), periods)
  if (fpa_mechanism_on(config, "capacity_x_util_x_rate") &&
      all(c("capacite", "taux_utilisation", "tarif") %in% names(df))) {
    for (p in periods) {
      sl <- df[df$periode == p, , drop = FALSE]
      out[[p]] <- out[[p]] + sum(
        as.numeric(sl$capacite) * as.numeric(sl$taux_utilisation) * as.numeric(sl$tarif),
        na.rm = TRUE
      )
    }
  }
  if (fpa_mechanism_on(config, "qty_x_price") &&
      all(c("quantite", "prix") %in% names(df))) {
    for (p in periods) {
      sl <- df[df$periode == p, , drop = FALSE]
      out[[p]] <- out[[p]] + sum(as.numeric(sl$quantite) * as.numeric(sl$prix), na.rm = TRUE)
    }
  }
  if (fpa_mechanism_on(config, "customers_x_spend") &&
      all(c("nb_clients", "depense_moyenne") %in% names(df))) {
    for (p in periods) {
      sl <- df[df$periode == p, , drop = FALSE]
      out[[p]] <- out[[p]] + sum(
        as.numeric(sl$nb_clients) * as.numeric(sl$depense_moyenne),
        na.rm = TRUE
      )
    }
  }
  out
}

fpa_volumes_payroll <- function(df, config) {
  periods <- sort(unique(df$periode))
  out <- setNames(rep(0, length(periods)), periods)
  if (!fpa_mechanism_on(config, "headcount_x_cost")) return(out)
  if (!all(c("effectif", "cout_moyen") %in% names(df))) return(out)
  for (p in periods) {
    sl <- df[df$periode == p, , drop = FALSE]
    out[[p]] <- sum(as.numeric(sl$effectif) * as.numeric(sl$cout_moyen), na.rm = TRUE)
  }
  out
}
