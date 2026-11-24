# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
}

# Which setup formula owns which Prudent / Ambitieux lever.
# The rows and the saved percents come from the configuration.
# The French sentences are looked up from those ids.

fpa_match_slug <- function(id) gsub("[^A-Za-z0-9]", "_", as.character(id))

fpa_match_catalog <- function() {
  list(
    list(id = "capacity_x_util_x_rate", name = "match_mech_capacity", off = "match_off_other",
         levers = c("taux_utilisation", "tarif")),
    list(id = "headcount_x_cost", name = "match_mech_head", off = "match_off_other",
         levers = c("effectif", "cout_moyen")),
    list(id = "qty_x_price", name = "match_mech_qty", off = "match_off_qty",
         levers = c("quantite", "prix")),
    list(id = "customers_x_spend", name = "match_mech_customers", off = "match_off_customers",
         levers = c("clients", "depense_moyenne")),
    list(id = "effective_tax_rate", name = "match_mech_tax", off = "match_off_other",
         levers = character(), held = "match_held_tax"),
    list(id = "fixed_monthly_expense", name = "match_mech_fixed", off = "match_off_other",
         levers = "charges.fixes"),
    list(id = "dso_dpo_dio", name = "match_mech_days", off = "match_off_other",
         levers = c("dso", "dpo", "dio"), held = "match_held_days", held_lever = "match_lever_days"),
    list(id = "variable_cost_pct_or_unit", name = "match_mech_variable", off = "match_off_other",
         levers = "cout.variable.pct"),
    list(id = "revenue_growth_pct", name = "match_mech_growth", off = "match_off_growth",
         levers = "croissance.ca"),
    list(id = "capex_schedule", name = "match_mech_capex", off = "match_off_other",
         levers = character(), held = "match_held_capex"),
    list(id = "debt_schedule", name = "match_mech_debt", off = "match_off_other",
         levers = character(), held = "match_held_debt")
  )
}

fpa_match_flags <- function(config) {
  m <- config$mechanisms
  if (is.null(m) || !length(m)) return(list())
  if (is.data.frame(m)) {
    id <- if ("id" %in% names(m)) as.character(m$id) else character()
    on <- if ("enabled" %in% names(m)) as.logical(m$enabled) else rep(TRUE, length(id))
    return(stats::setNames(as.list(on), id))
  }
  lapply(m, function(x) isTRUE(x) || identical(tolower(as.character(x)), "true"))
}

fpa_match_inputs <- function(config) {
  empty <- data.frame(
    id = character(), label = character(), mechanism = character(),
    low_pct = numeric(), high_pct = numeric(), stringsAsFactors = FALSE
  )
  inp <- config$scenario_ranges$inputs
  if (is.null(inp) || !length(inp)) return(empty)
  one <- function(id, label, mechanism, low, high) {
    data.frame(
      id = as.character(id %||% ""),
      label = as.character(label %||% id %||% ""),
      mechanism = as.character(mechanism %||% ""),
      low_pct = suppressWarnings(as.numeric(low %||% NA)),
      high_pct = suppressWarnings(as.numeric(high %||% NA)),
      stringsAsFactors = FALSE
    )
  }
  if (is.data.frame(inp)) {
    if (!nrow(inp) || !("id" %in% names(inp))) return(empty)
    out <- one(
      inp$id,
      if ("label" %in% names(inp)) inp$label else inp$id,
      if ("mechanism" %in% names(inp)) inp$mechanism else "",
      if ("low_pct" %in% names(inp)) inp$low_pct else NA,
      if ("high_pct" %in% names(inp)) inp$high_pct else NA
    )
  } else {
    rows <- lapply(inp, function(x) {
      one(x$id, x$label %||% x$id, x$mechanism, x$low_pct, x$high_pct)
    })
    out <- do.call(rbind, rows)
  }
  out[nzchar(out$id), , drop = FALSE]
}

fpa_match_pct <- function(x) {
  n <- suppressWarnings(as.numeric(x))
  if (length(n) != 1L || !is.finite(n)) return("")
  format(round(n), trim = TRUE, scientific = FALSE)
}

fpa_match_lever_name <- function(id, fallback = "") {
  key <- paste0("match_lever_", fpa_match_slug(id))
  lab <- fpa_txt(key)
  if (nzchar(lab)) return(lab)
  if (nzchar(fallback)) return(fallback)
  as.character(id)
}

fpa_match_effect <- function(id, low, high) {
  key <- paste0("match_do_", fpa_match_slug(id))
  sentence <- fpa_txt(key)
  if (!nzchar(sentence)) sentence <- fpa_txt("match_do_other")
  lo <- fpa_match_pct(low)
  hi <- fpa_match_pct(high)
  if (!nzchar(lo) || !nzchar(hi)) return(sentence)
  paste(sentence, fpa_txt("match_width", lo, hi))
}

fpa_match_mech_name <- function(mech) {
  hit <- Filter(function(x) identical(x$id, mech), fpa_match_catalog())
  if (length(hit)) {
    lab <- fpa_txt(hit[[1]]$name)
    if (nzchar(lab)) return(lab)
  }
  as.character(mech %||% "")
}

#' Rows of the three tables: levers on the percent card, formulas on without a row, formulas off.
fpa_match_rows <- function(config) {
  flags <- fpa_match_flags(config)
  inputs <- fpa_match_inputs(config)
  catalog <- fpa_match_catalog()
  known <- unlist(lapply(catalog, function(x) x$levers), use.names = FALSE)
  on_rows <- list()
  if (nrow(inputs)) {
    for (i in seq_len(nrow(inputs))) {
      id <- inputs$id[[i]]
      mech <- inputs$mechanism[[i]]
      if (!nzchar(mech)) {
        for (item in catalog) if (id %in% item$levers) mech <- item$id
      }
      on_rows[[length(on_rows) + 1L]] <- c(
        fpa_match_mech_name(mech),
        if (nzchar(inputs$label[[i]])) inputs$label[[i]] else fpa_match_lever_name(id),
        fpa_match_effect(id, inputs$low_pct[[i]], inputs$high_pct[[i]])
      )
    }
  }
  shown <- if (nrow(inputs)) inputs$id else character()
  held_rows <- list()
  off_rows <- list()
  for (item in catalog) {
    on <- isTRUE(flags[[item$id]])
    levers <- item$levers
    picked <- intersect(levers, shown)
    if (!on && !length(picked)) {
      off_rows[[length(off_rows) + 1L]] <- c(
        fpa_txt(item$name),
        fpa_txt("match_no_line"),
        fpa_txt(item$off)
      )
      next
    }
    if (!on) next
    left <- setdiff(levers, shown)
    if (!length(levers)) {
      held_rows[[length(held_rows) + 1L]] <- c(
        fpa_txt(item$name),
        fpa_txt("match_no_line"),
        fpa_txt(item$held %||% "match_held_other")
      )
    } else if (!length(picked) && length(left)) {
      lever <- if (!is.null(item$held_lever)) fpa_txt(item$held_lever) else paste(vapply(left, fpa_match_lever_name, character(1)), collapse = ", ")
      held_rows[[length(held_rows) + 1L]] <- c(
        fpa_txt(item$name),
        lever,
        fpa_txt(item$held %||% "match_aside_one")
      )
    } else if (length(left)) {
      for (id in left) {
        held_rows[[length(held_rows) + 1L]] <- c(
          fpa_txt(item$name),
          fpa_match_lever_name(id),
          fpa_txt("match_aside_one")
        )
      }
    }
  }
  list(on = on_rows, held = held_rows, off = off_rows)
}

fpa_match_counts <- function(config) {
  r <- fpa_match_rows(config)
  c(on = length(r$on), held = length(r$held), off = length(r$off))
}

fpa_shiny_match_card <- function(config) {
  r <- fpa_match_rows(config)
  on_rows <- r$on
  held_rows <- r$held
  off_rows <- r$off
  table_of <- function(title, rows) {
    if (!length(rows)) return(NULL)
    shiny::tags$div(
      shiny::tags$h3(title),
      shiny::tags$table(
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(fpa_txt("match_col_formula")),
          shiny::tags$th(fpa_txt("match_col_lever")),
          shiny::tags$th(fpa_txt("match_col_effect"))
        )),
        shiny::tags$tbody(lapply(rows, function(r) {
          shiny::tags$tr(lapply(r, shiny::tags$td))
        }))
      )
    )
  }
  if (!length(on_rows) && !length(held_rows) && !length(off_rows)) {
    return(shiny::tags$p(fpa_txt("match_none")))
  }
  shiny::tagList(
    shiny::tags$p(class = "fpa-match-lead", fpa_txt("match_lead")),
    table_of(fpa_txt("match_sec_on"), on_rows),
    table_of(fpa_txt("match_sec_held"), held_rows),
    table_of(fpa_txt("match_sec_off"), off_rows)
  )
}
