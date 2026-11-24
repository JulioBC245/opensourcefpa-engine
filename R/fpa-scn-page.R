# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0L) y else x
}

# The Scénarios tab: what each path gives on the open months, and where the figures come from.

fpa_scnp_toc <- list(
  c(id = "pnl-fold-case", key = "scnp_toc_pick"),
  c(id = "scn-result", key = "scnp_toc_result"),
  c(id = "pnl-fold-drv", key = "scnp_toc_drv"),
  c(id = "pnl-fold-pct", key = "scnp_toc_pct"),
  c(id = "scn-match", key = "scnp_toc_match"),
  c(id = "scn-src", key = "scnp_toc_src"),
  c(id = "scn-where", key = "scnp_toc_where")
)

fpa_scn_month <- function(periode, short = FALSE) {
  long <- c(fpa_txt("month_1"), fpa_txt("month_2"), fpa_txt("month_3"), fpa_txt("month_4"), fpa_txt("month_5"), fpa_txt("month_6"),
            fpa_txt("month_7"), fpa_txt("month_8"), fpa_txt("month_9"), fpa_txt("month_10"), fpa_txt("month_11"), fpa_txt("month_12"))
  brief <- c(fpa_txt("month_s_1"), fpa_txt("month_s_2"), fpa_txt("month_s_3"), fpa_txt("month_s_4"), fpa_txt("month_s_5"), fpa_txt("month_s_6"), fpa_txt("month_s_7"), fpa_txt("month_s_8"), fpa_txt("month_s_9"), fpa_txt("month_s_10"), fpa_txt("month_s_11"), fpa_txt("month_s_12"))
  per <- as.character(periode[[1]] %||% "")
  m <- suppressWarnings(as.integer(substr(per, 6, 7)))
  y <- substr(per, 1, 4)
  if (length(m) != 1L || is.na(m) || m < 1L || m > 12L) return(per)
  label <- if (short) brief[[m]] else long[[m]]
  if (nzchar(y) && grepl("^[0-9]{4}$", y)) paste(label, y) else label
}

fpa_scn_line_month <- function(df, per, line) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df) || !(line %in% names(df))) return(NA_real_)
  x <- suppressWarnings(as.numeric(df[[line]][as.character(df$periode) == per]))
  if (!length(x) || all(!is.finite(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

fpa_scn_case_of <- function(cases, per) {
  if (!is.null(names(cases)) && per %in% names(cases)) {
    id <- as.character(cases[[per]])
    if (id %in% c("low", "base", "high")) return(id)
  }
  "base"
}

fpa_scn_cash_end <- function(cash, per) {
  if (is.null(cash) || !is.data.frame(cash) || !nrow(cash) || !("cash_close" %in% names(cash))) return(NA_real_)
  hit <- match(per, as.character(cash$periode))
  if (is.na(hit)) return(NA_real_)
  suppressWarnings(as.numeric(cash$cash_close[[hit]]))
}

#' One row per open month: operating income on each path, on the chosen path, and the budget.
fpa_scn_page_rows <- function(v) {
  open <- as.character(v$open %||% character())
  if (!length(open)) return(NULL)
  bud <- fpa_scenario_budget_oi(v$budget)
  pick <- function(df) vapply(open, function(p) fpa_scn_line_month(df, p, "OI"), numeric(1))
  data.frame(
    periode = open,
    case = vapply(open, function(p) fpa_scn_case_of(v$cases, p), character(1)),
    low = pick(v$low),
    base = pick(v$base),
    high = pick(v$high),
    path = pick(v$pnl),
    budget = pick(bud),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

fpa_scn_total <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || all(!is.finite(x))) return(NA_real_)
  sum(x, na.rm = TRUE)
}

#' Budget total only when every month has a finite budget. Missing is not zero.
fpa_scn_budget_total <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || any(!is.finite(x))) return(NA_real_)
  sum(x)
}

fpa_scn_eur <- function(x) {
  if (length(x) != 1L || !is.finite(x)) return("\u2013")
  format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
}

fpa_scn_glyph <- function(id) {
  shiny::HTML(switch(
    as.character(id),
    low = '<svg width="12" height="12" viewBox="0 0 16 16" aria-hidden="true"><path d="M8 13.5 L2 3 H14 Z" fill="currentColor"/></svg>',
    high = '<svg width="12" height="12" viewBox="0 0 16 16" aria-hidden="true"><path d="M8 2.5 L14 13 H2 Z" fill="currentColor"/></svg>',
    '<svg width="12" height="12" viewBox="0 0 16 16" aria-hidden="true"><rect x="2" y="4" width="12" height="2.4" rx="0.4" fill="currentColor"/><rect x="2" y="9.6" width="12" height="2.4" rx="0.4" fill="currentColor"/></svg>'
  ))
}

fpa_scn_case_label <- function(id) {
  switch(as.character(id), low = fpa_txt("scen_bas"), high = fpa_txt("scen_haut"), fpa_txt("scen_central"))
}

#' Tiles and the month table. cash_base is the cash path when every open month is expected.
fpa_scn_page_result <- function(v, cash_base = NULL) {
  rows <- fpa_scn_page_rows(v)
  if (is.null(rows)) return(shiny::tags$p(class = "fpa-scnp-lead", fpa_txt("scnp_none")))
  if (!isTRUE(v$ready)) return(shiny::tags$p(class = "fpa-scnp-lead", fpa_txt("scnp_wait")))
  last <- rows$periode[[nrow(rows)]]
  month_name <- fpa_scn_month(last)
  tot <- vapply(c("low", "base", "high", "path"), function(k) fpa_scn_total(rows[[k]]), numeric(1))
  tot[["budget"]] <- fpa_scn_budget_total(rows$budget)
  n_budget_missing <- sum(!is.finite(suppressWarnings(as.numeric(rows$budget))))
  cash <- c(
    low = fpa_scn_cash_end(v$cash_low, last),
    base = fpa_scn_cash_end(cash_base, last),
    high = fpa_scn_cash_end(v$cash_high, last),
    path = fpa_scn_cash_end(v$cash, last)
  )
  gap <- if (is.finite(tot[["budget"]])) tot[["path"]] - tot[["budget"]] else NA_real_
  off_by_round <- any(vapply(c("low", "base", "high", "path"), function(k) {
    x <- suppressWarnings(as.numeric(rows[[k]]))
    is.finite(tot[[k]]) && sum(round(x), na.rm = TRUE) != round(tot[[k]])
  }, logical(1)))
  if (is.finite(tot[["budget"]])) {
    x_bud <- suppressWarnings(as.numeric(rows$budget))
    if (sum(round(x_bud), na.rm = TRUE) != round(tot[["budget"]])) off_by_round <- TRUE
  }
  neg <- function(x) if (is.finite(x) && x < -0.5) "is-neg" else ""
  tile <- function(label, value, sub) {
    shiny::tags$div(
      class = "fpa-scnp-tile",
      shiny::tags$span(class = "fpa-scnp-tile-k", label),
      shiny::tags$span(class = paste("fpa-scnp-tile-v", neg(value)), paste(fpa_scn_eur(value), "EUR")),
      shiny::tags$span(class = "fpa-scnp-tile-s", sub)
    )
  }
  signed <- function(x) if (is.finite(x) && x > 0.5) paste0("+", fpa_scn_eur(x)) else fpa_scn_eur(x)
  td_num <- function(x, picked = FALSE) {
    shiny::tags$td(
      class = paste("n", neg(x), if (picked) "is-pick"),
      if (picked) shiny::tags$span(class = "fpa-scnp-tick", `aria-hidden` = "true", "\u2713"),
      fpa_scn_eur(x)
    )
  }
  head_case <- function(id) {
    shiny::tags$th(class = paste("n", paste0("is-", id)), fpa_scn_glyph(id), " ", fpa_scn_case_label(id))
  }
  body <- lapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, , drop = FALSE]
    shiny::tags$tr(
      shiny::tags$th(scope = "row", fpa_scn_month(r$periode, short = TRUE)),
      shiny::tags$td(
        class = paste("fpa-scnp-pick", paste0("is-", r$case)),
        fpa_scn_glyph(r$case), " ", fpa_scn_case_label(r$case),
        shiny::tags$span(class = paste("fpa-scnp-pick-v", neg(r$path)), fpa_scn_eur(r$path))
      ),
      td_num(r$low, identical(r$case, "low")),
      td_num(r$base, identical(r$case, "base")),
      td_num(r$high, identical(r$case, "high")),
      td_num(r$budget)
    )
  })
  gap_tile <- if (is.finite(gap)) {
    shiny::tags$div(
      class = "fpa-scnp-tile",
      shiny::tags$span(class = "fpa-scnp-tile-k", fpa_txt("scnp_tile_gap")),
      shiny::tags$span(class = paste("fpa-scnp-tile-v", neg(gap)), paste(signed(gap), "EUR")),
      shiny::tags$span(class = "fpa-scnp-tile-s", fpa_txt("scnp_tile_gap_sub", fpa_scn_eur(tot[["budget"]])))
    )
  } else if (n_budget_missing > 0L) {
    shiny::tags$div(
      class = "fpa-scnp-tile",
      shiny::tags$span(class = "fpa-scnp-tile-k", fpa_txt("scnp_tile_gap")),
      shiny::tags$span(class = "fpa-scnp-tile-v", fpa_txt("scnp_gap_partial", n_budget_missing)),
      shiny::tags$span(class = "fpa-scnp-tile-s", fpa_txt("scnp_tile_gap_partial", n_budget_missing))
    )
  } else {
    shiny::tags$div(
      class = "fpa-scnp-tile",
      shiny::tags$span(class = "fpa-scnp-tile-k", fpa_txt("scnp_tile_gap")),
      shiny::tags$span(class = "fpa-scnp-tile-v", "\u2013"),
      shiny::tags$span(class = "fpa-scnp-tile-s", fpa_txt("scnp_tile_gap_sub", "\u2013"))
    )
  }
  shiny::tagList(
    shiny::tags$div(
      class = "fpa-scnp-tiles",
      tile(fpa_txt("scnp_tile_oi"), tot[["path"]], fpa_txt("scnp_tile_oi_sub", fpa_scn_eur(tot[["low"]]), fpa_scn_eur(tot[["high"]]))),
      tile(fpa_txt("scnp_tile_cash", month_name), cash[["path"]], fpa_txt("scnp_tile_cash_sub", fpa_scn_eur(cash[["low"]]), fpa_scn_eur(cash[["high"]]))),
      gap_tile
    ),
    shiny::tags$p(class = "fpa-scnp-lead", fpa_txt("scnp_result_lead")),
    shiny::tags$div(
      class = "fpa-scnp-table",
      shiny::tags$table(
        shiny::tags$thead(shiny::tags$tr(
          shiny::tags$th(fpa_txt("scnp_col_month")),
          shiny::tags$th(fpa_txt("scnp_col_pick")),
          head_case("low"), head_case("base"), head_case("high"),
          shiny::tags$th(class = "n", fpa_txt("scnp_col_budget"))
        )),
        shiny::tags$tbody(body),
        shiny::tags$tfoot(
          shiny::tags$tr(
            class = "is-total",
            shiny::tags$th(scope = "row", fpa_txt("scnp_row_total")),
            shiny::tags$td(class = paste("fpa-scnp-pick", neg(tot[["path"]])), fpa_scn_eur(tot[["path"]])),
            td_num(tot[["low"]]), td_num(tot[["base"]]), td_num(tot[["high"]]), td_num(tot[["budget"]])
          ),
          shiny::tags$tr(
            class = "is-cash",
            shiny::tags$th(scope = "row", fpa_txt("scnp_row_cash", month_name)),
            shiny::tags$td(class = paste("fpa-scnp-pick", neg(cash[["path"]])), fpa_scn_eur(cash[["path"]])),
            td_num(cash[["low"]]), td_num(cash[["base"]]), td_num(cash[["high"]]),
            shiny::tags$td(class = "n", "\u2013")
          )
        )
      )
    ),
    if (off_by_round) shiny::tags$p(class = "fpa-scnp-round", fpa_txt("scnp_round"))
  )
}

fpa_scn_file_name <- function(path) {
  p <- as.character(path %||% "")
  if (length(p) != 1L || is.na(p) || !nzchar(p) || !file.exists(p)) return(fpa_txt("scnp_src_missing"))
  basename(p)
}

#' Where each figure of the scenario comes from, and what can be changed on this page.
fpa_scn_sources <- function(config, paths, line_drivers = NULL) {
  pnl_file <- fpa_scn_file_name(tryCatch(fpa_resolve_path(config, "pnl_actuals", paths), error = function(e) NA))
  bud_file <- fpa_scn_file_name(tryCatch(fpa_resolve_path(config, "budget", paths), error = function(e) NA))
  vol <- tryCatch(fpa_read_volumes(config, paths), error = function(e) NULL)
  vol_file <- fpa_scn_file_name(if (!is.null(vol)) vol$path else NA)
  span <- tryCatch(fpa_history_span(fpa_history_pnl(config, paths)), error = function(e) list(n = 0L))
  cfg_path <- Sys.getenv("FPA_CONFIG")
  saved <- if (nzchar(cfg_path) && file.exists(cfg_path)) format(file.info(cfg_path)$mtime, fpa_txt("loc_saved_clock")) else ""
  rows <- list(
    c(fpa_txt("scnp_src_closed"), fpa_txt("scnp_src_closed_from", pnl_file), fpa_txt("scnp_src_closed_here")),
    if (!is.null(vol) && !is.null(vol$data)) c(fpa_txt("scnp_src_level"), fpa_txt("scnp_src_level_from", vol_file), fpa_txt("scnp_src_level_here")),
    if (!is.null(line_drivers) && isTRUE(span$n > 0L)) c(fpa_txt("scnp_src_lines"), fpa_txt("scnp_src_lines_from", span$n, span$from, span$to), fpa_txt("scnp_src_lines_here")),
    c(
      fpa_txt("scnp_src_width"),
      if (nzchar(saved)) fpa_txt("scnp_src_width_from", saved) else fpa_txt("scnp_src_width_from_nodate"),
      fpa_txt("scnp_src_width_here")
    ),
    c(fpa_txt("scnp_src_pick"), fpa_txt("scnp_src_pick_from"), fpa_txt("scnp_src_pick_here")),
    c(fpa_txt("scnp_src_budget"), fpa_txt("scnp_src_budget_from", bud_file), fpa_txt("scnp_src_budget_here"))
  )
  rows <- Filter(Negate(is.null), rows)
  shiny::tags$div(
    class = "fpa-match is-flat",
    shiny::tags$table(
      shiny::tags$thead(shiny::tags$tr(
        shiny::tags$th(fpa_txt("scnp_src_col_what")),
        shiny::tags$th(fpa_txt("scnp_src_col_from")),
        shiny::tags$th(fpa_txt("scnp_src_col_here"))
      )),
      shiny::tags$tbody(lapply(rows, function(r) {
        shiny::tags$tr(shiny::tags$td(shiny::tags$strong(r[[1]])), shiny::tags$td(r[[2]]), shiny::tags$td(r[[3]]))
      }))
    )
  )
}

fpa_scn_where_groups <- function() {
  list(
    list(tab = fpa_txt("loc_summary"), items = c("kpi", "trend", "cashline")),
    list(tab = "Compte de résultat", items = c("cone", "wf1", "wfr", "grid")),
    list(tab = "Trésorerie", items = c("bridge", "cashtable")),
    list(tab = "Bilan", items = c("bstable", "tree")),
    list(tab = fpa_txt("loc_variances"), items = c("tiles", "heat", "gap", "land", "parts", "months")),
    list(tab = fpa_txt("loc_health"), items = "health")
  )
}

#' The tabs where a change of path shows, each with a button to open it.
fpa_scn_where <- function() {
  shiny::tagList(
    shiny::tags$p(class = "fpa-scnp-lead", fpa_txt("scnp_where_lead")),
    shiny::tags$div(
      class = "fpa-scnp-where",
      lapply(fpa_scn_where_groups(), function(g) {
        shiny::tags$div(
          class = "fpa-scnp-where-card",
          shiny::tags$div(
            class = "fpa-scnp-where-head",
            shiny::tags$h4(g$tab),
            shiny::tags$button(
              type = "button", class = "fpa-scnp-go", `data-tab` = g$tab,
              onclick = "if (window.fpaGoTab) window.fpaGoTab(this.getAttribute('data-tab'));",
              fpa_txt("scnp_where_go"),
              shiny::HTML('<svg width="12" height="12" viewBox="0 0 16 16" aria-hidden="true"><path d="M5 3 L11 8 L5 13" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"/></svg>')
            )
          ),
          shiny::tags$ul(lapply(g$items, function(id) shiny::tags$li(fpa_txt(paste0("scnp_w_", id)))))
        )
      })
    )
  )
}
