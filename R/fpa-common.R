# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-common.R — Period chips, anomaly counts, snapshot, cash floor.
# LAST CHANGE: 2026-09-30

#' Closed month plus year-end forecast month used by Anomalies and Synthèse.
fpa_anomaly_focus_periods <- function(status) {
  closed <- tryCatch(fpa_health_closed_month(status), error = function(e) "")
  closed <- as.character(closed %||% "")
  if (!nzchar(closed)) return(character())
  unique(c(closed, paste0(substr(closed, 1L, 4L), "-12")))
}

#' Band counts for non-clear anomaly rows (same rule as the Anomalies page).
#' act = books + ratio; watch = driver + season.
fpa_anomaly_counts <- function(scan) {
  empty <- list(act = 0L, watch = 0L, n = 0L)
  if (is.null(scan) || !nrow(scan) || !"level" %in% names(scan)) return(empty)
  hits <- scan[as.character(scan$level) != "clear", , drop = FALSE]
  if (!nrow(hits)) return(empty)
  fam <- as.character(hits$family %||% "")
  list(
    act = as.integer(sum(fam %in% c("books", "ratio"))),
    watch = as.integer(sum(fam %in% c("driver", "season"))),
    n = as.integer(nrow(hits))
  )
}

#' Names the calendar reader already treats as the life of a purchase, in months.
fpa_todo_life_names <- function() c("duree_mois", "duree", "life", "life_months")

#' Sentence for columns declared on a file and left without a map role.
fpa_todo_column_sentence <- function(cols) {
  cols <- trimws(as.character(cols %||% character()))
  cols <- cols[nzchar(cols)]
  if (!length(cols)) return("")
  label <- paste(cols, collapse = ", ")
  if (all(tolower(cols) %in% fpa_todo_life_names())) {
    key <- if (length(cols) == 1L) "todo_col_life" else "todo_cols_life"
    return(fpa_txt(key, label))
  }
  key <- if (length(cols) == 1L) "todo_col_norole" else "todo_cols_norole"
  fpa_txt(key, label)
}

#' Sentence for a file that is on disk and connected to no switched-on rule.
fpa_todo_unused_sentence <- function(file) {
  file <- as.character(file %||% "")
  if (!nzchar(file)) file <- "ce fichier"
  fpa_txt("todo_file_unused", file)
}

#' Column names inside a gap that ends with fpa_txt("loc_without_role_sp")
fpa_todo_bare_cols <- function(gap) {
  body <- sub(" sans rôle\\.$", "", as.character(gap %||% ""))
  cols <- sub("^.* : ", "", body)
  trimws(strsplit(cols, ",", fixed = TRUE)[[1]])
}

#' Stable key for a map column that has no role. The index in the gap list is not stable.
fpa_todo_text_key <- function(text) {
  bytes <- charToRaw(enc2utf8(as.character(text %||% "")))
  if (!length(bytes)) return("0")
  paste(sprintf("%02x", as.integer(bytes)), collapse = "")
}

#' DOM id shared by an À faire anomaly row and the matching line on the Anomalies card.
fpa_anom_row_dom_id <- function(family, periode, line_id) {
  raw <- paste(as.character(family %||% ""), as.character(periode %||% ""), as.character(line_id %||% ""), sep = "-")
  paste0("anom-row-", gsub("[^A-Za-z0-9_-]", "", raw))
}

#' À faire queue in four groups. The header count is the sum of the groups.
#' decisions: journal due items; absent: fpa_chain_absent_sources(); scan: anomaly
#' scan (act rows only are queued, the same rows Anomalies counts as à éclaircir);
#' nodes: fpa_chain_build()$nodes (incomplete rules and files with no rule downstream);
#' gaps: fpa_chain_build()$gaps, of which the columns left without a role are queued.
fpa_todo_groups <- function(decisions = list(), absent = NULL, scan = NULL, nodes = NULL, gaps = character()) {
  files <- list()
  if (!is.null(absent) && nrow(absent)) {
    for (i in seq_len(nrow(absent))) {
      path <- as.character(absent$path[[i]] %||% "")
      files[[length(files) + 1L]] <- list(
        id = paste0("path:", as.character(absent$id[[i]])),
        title = as.character(absent$title[[i]]),
        sentence = if (nzchar(path)) fpa_txt("header_todo_path", path) else fpa_txt("header_todo_path_empty"),
        neg = TRUE
      )
    }
  }
  anoms <- list()
  if (!is.null(scan) && nrow(scan) && all(c("family", "level") %in% names(scan))) {
    hit <- scan[as.character(scan$level) != "clear" & as.character(scan$family) %in% c("books", "ratio"), , drop = FALSE]
    eur <- function(x) {
      x <- suppressWarnings(as.numeric(x[[1]]))
      if (!is.finite(x)) return("\u2013")
      format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
    }
    ratio_txt <- function(x) {
      x <- suppressWarnings(as.numeric(x[[1]]))
      if (!is.finite(x)) return("\u2013")
      if (abs(x) >= 100) format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE) else {
        formatC(x, format = "f", digits = 1, decimal.mark = ",")
      }
    }
    for (i in seq_len(nrow(hit))) {
      lid <- as.character(hit$line_id[[i]])
      fam <- as.character(hit$family[[i]])
      per <- as.character(hit$periode[[i]])
      lab <- if (identical(lid, "BS")) "Bilan" else if (identical(lid, "cash_ratio")) fpa_txt("health_cash") else {
        tryCatch(fpa_line_fr(lid), error = function(e) lid)
      }
      if (!length(lab) || !nzchar(lab)) lab <- lid
      card_key <- switch(fam, books = "anom_books", ratio = "anom_ratio", "")
      card <- if (nzchar(card_key)) fpa_txt(card_key) else lab
      month <- fpa_ym_fr(per)
      num <- function(col) {
        if (!col %in% names(hit)) return(NA_real_)
        suppressWarnings(as.numeric(hit[[col]][[i]]))
      }
      sentence <- if (identical(fam, "ratio") && is.finite(num("value"))) {
        fpa_txt("header_todo_anom_ratio", ratio_txt(num("value")), eur(num("gap")), eur(num("reference")), card)
      } else if (identical(fam, "books") && identical(lid, "BS") && is.finite(num("value"))) {
        fpa_txt("header_todo_anom_books", eur(num("value")), eur(num("reference")), eur(num("gap")), card)
      } else if (identical(fam, "books") && identical(lid, "CASH") && is.finite(num("value"))) {
        fpa_txt("header_todo_anom_cash", eur(num("value")), eur(num("reference")), eur(num("gap")), card)
      } else {
        fpa_txt("header_todo_anom_line", lab, month)
      }
      anoms[[length(anoms) + 1L]] <- list(
        id = paste0("anom:", fpa_anom_row_dom_id(fam, per, lid)),
        title = if (nzchar(card) && nzchar(month)) fpa_txt("header_todo_anom_title", card, month) else lab,
        sentence = sentence,
        neg = FALSE
      )
    }
  }
  maps <- list()
  if (!is.null(nodes) && nrow(nodes)) {
    q <- nodes[as.character(nodes$status) %in% c("incomplet", "sans suite"), , drop = FALSE]
    for (i in seq_len(nrow(q))) {
      sentence <- trimws(as.character(q$detail[[i]]))
      if (identical(as.character(q$status[[i]]), "sans suite")) {
        file <- as.character(q$title[[i]])
        if ("path" %in% names(q)) {
          base <- basename(as.character(q$path[[i]] %||% ""))
          if (nzchar(base) && !identical(base, ".")) file <- base
        }
        sentence <- fpa_todo_unused_sentence(file)
      }
      maps[[length(maps) + 1L]] <- list(
        id = paste0("map:", as.character(q$id[[i]])),
        title = as.character(q$title[[i]]),
        sentence = sentence,
        neg = FALSE
      )
    }
  }
  bare <- grep(" sans rôle\\.$", as.character(gaps %||% character()), value = TRUE)
  for (i in seq_along(bare)) {
    maps[[length(maps) + 1L]] <- list(
      id = paste0("map:col:", fpa_todo_text_key(bare[[i]])),
      title = sub(" : .*$", "", bare[[i]]),
      sentence = fpa_todo_column_sentence(fpa_todo_bare_cols(bare[[i]])),
      neg = FALSE
    )
  }
  groups <- list(decisions = decisions %||% list(), files = files, anomalies = anoms, map = maps)
  list(groups = groups, n = sum(lengths(groups)))
}

#' Drop rows the user has checked off. A decision returns when its review date changes.
fpa_todo_hide_cleared <- function(tg, cleared = list()) {
  gone <- function(it) {
    hit <- NULL
    for (c in cleared %||% list()) {
      if (identical(as.character(c$id %||% ""), as.character(it$id %||% ""))) hit <- c
    }
    if (is.null(hit)) return(FALSE)
    if (identical(as.character(hit$via %||% ""), "scan")) return(FALSE)
    stamp <- as.character(it$stamp %||% "")
    if (nzchar(stamp) && !identical(stamp, as.character(hit$stamp %||% ""))) return(FALSE)
    TRUE
  }
  groups <- lapply(tg$groups, function(rows) Filter(function(it) !gone(it), rows %||% list()))
  list(groups = groups, n = sum(lengths(groups)))
}

fpa_todo_flat <- function(tg) {
  unlist(tg$groups %||% list(), recursive = FALSE) %||% list()
}

#' Pieces of the À faire queue that come from the files and the last calculation.
fpa_todo_parts <- function(config, paths, finance) {
  empty <- list(absent = NULL, scan = NULL, nodes = NULL, gaps = character())
  if (is.null(config)) return(empty)
  scan <- tryCatch({
    hist <- fpa_line_history(config, paths)
    vol <- fpa_shiny_volumes(config, paths)
    map <- fpa_line_map(config, "pnl")
    fpa_anomaly_scan(
      finance$bs, finance$pnl, finance$cash, vol, hist, map,
      fpa_anomaly_focus_periods(finance$period_status)
    )
  }, error = function(e) NULL)
  long <- finance$pnl_long
  if (is.null(long) || !nrow(long)) long <- NULL
  chain <- tryCatch(fpa_chain_build(config, long), error = function(e) NULL)
  list(
    absent = tryCatch(fpa_chain_absent_sources(config), error = function(e) NULL),
    scan = scan,
    nodes = if (!is.null(chain)) chain$nodes else NULL,
    gaps = if (!is.null(chain)) chain$gaps else character()
  )
}

#' Cash reconciliation and material close actions for the À faire badge.
#' Monitoring-only findings stay off this list.
fpa_todo_action_items <- function(pack, entries = list(), today = Sys.Date()) {
  out <- list()
  if (!isTRUE(pack$ok)) return(out)
  fin <- pack$finance
  saved <- tryCatch(fpa_outlook_read(pack$config), error = function(e) NULL)
  roll <- if (is.null(saved)) NULL else {
    tryCatch(fpa_outlook_roll(saved$opening, saved$items, saved$as_of, FALSE), error = function(e) NULL)
  }
  floor <- tryCatch(fpa_cash_floor_read(pack$config), error = function(e) 0)
  model <- tryCatch(
    fpa_synthese_model(
      pack$config, fin, saved, floor, today,
      journal_entries = entries, volumes = NULL
    ),
    error = function(e) NULL
  )
  find <- if (!is.null(model)) model$finding else NULL
  if (!is.null(find) && as.character(find$id %||% "") %in% c(
    "need_balance", "cash_gap", "balance_only"
  )) {
    out[[length(out) + 1L]] <- list(
      id = paste0("synth:", find$id),
      title = as.character(find$title %||% ""),
      sentence = as.character(find$body %||% ""),
      neg = identical(as.character(find$severity %||% ""), "alert"),
      stamp = "",
      go_tab = as.character(find$action_tab %||% "Trésorerie"),
      go_open = as.character(find$action_open %||% "out")
    )
  }
  close_st <- tryCatch(fpa_close_state(pack), error = function(e) NULL)
  if (!is.null(close_st)) {
    steps <- tryCatch(fpa_close_steps(close_st, NULL), error = function(e) list())
    for (nm in names(steps)) {
      step <- steps[[nm]]
      if (!identical(as.character(step$state %||% ""), "now")) next
      if (!nm %in% c("save", "budget", "lock", "data")) next
      out[[length(out) + 1L]] <- list(
        id = paste0("close:", nm),
        title = as.character(step$label %||% ""),
        sentence = as.character(step$detail %||% ""),
        neg = identical(nm, "budget") || identical(nm, "lock"),
        stamp = "",
        go_tab = "Clôture",
        go_open = ""
      )
    }
  }
  out
}

#' Full À faire queue for one loaded pack, before the user has checked anything off.
fpa_todo_queue <- function(pack, entries = list()) {
  if (!isTRUE(pack$ok)) return(fpa_todo_groups(list(), NULL, NULL, NULL))
  items <- fpa_journal_due_items(
    entries, pack$finance$cash, pack$finance$pnl, pack$finance$period_status
  )
  actions <- fpa_todo_action_items(pack, entries)
  items <- c(actions, items)
  st <- fpa_todo_parts(pack$config, pack$paths, pack$finance)
  fpa_todo_groups(items, st$absent, st$scan, st$nodes, st$gaps)
}

fpa_todo_find <- function(tg, id) {
  id <- as.character(id %||% "")
  for (rows in tg$groups) {
    for (it in rows %||% list()) {
      if (identical(as.character(it$id %||% ""), id)) return(it)
    }
  }
  NULL
}

#' Period chip label: réalisé · sept. 2026 or prévision · déc. 2026.
fpa_period_chip_label <- function(kind, periode) {
  per <- as.character(periode %||% "")
  if (!nzchar(per)) return("")
  kind <- as.character(kind %||% "actual")
  is_fcst <- kind %in% c("forecast", "prévision", "prevision", "fcst")
  prefix <- if (is_fcst) "prévision" else "réalisé"
  mon <- tryCatch({
    if (exists("fpa_shiny_month_short", mode = "function")) {
      paste(fpa_shiny_month_short(per), substr(per, 1L, 4L))
    } else {
      mm <- suppressWarnings(as.integer(substr(per, 6L, 7L)))
      labs <- c(fpa_txt("month_s_1"), fpa_txt("month_s_2"), fpa_txt("month_s_3"), fpa_txt("month_s_4"), fpa_txt("month_s_5"), fpa_txt("month_s_6"),
                fpa_txt("month_s_7"), fpa_txt("month_s_8"), fpa_txt("month_s_9"), fpa_txt("month_s_10"), fpa_txt("month_s_11"), fpa_txt("month_s_12"))
      if (is.finite(mm) && mm >= 1L && mm <= 12L) {
        paste(labs[[mm]], substr(per, 1L, 4L))
      } else {
        per
      }
    }
  }, error = function(e) per)
  paste0(prefix, " · ", mon)
}

#' HTML span for a period chip (Shiny tags when available).
fpa_shiny_period_chip <- function(kind, periode) {
  lab <- fpa_period_chip_label(kind, periode)
  if (!nzchar(lab)) return(NULL)
  if (requireNamespace("shiny", quietly = TRUE)) {
    return(shiny::tags$span(class = "fpa-period-chip", lab))
  }
  lab
}

#' Live open-month OI band sentence from pnl_view()-shaped list (low/base/high).
#' Budget only when every open month has a finite budget. Missing is not zero.
fpa_live_oi_band_sentence <- function(view, status = NULL, budget = NULL) {
  if (is.null(view) || !isTRUE(view$ready) || !length(view$open)) return("")
  open <- as.character(view$open)
  sum_oi <- function(df) {
    if (is.null(df) || !nrow(df) || !"OI" %in% names(df)) return(NA_real_)
    sum(suppressWarnings(as.numeric(df$OI[as.character(df$periode) %in% open])), na.rm = TRUE)
  }
  oi_base <- sum_oi(view$pnl %||% view$base)
  oi_low <- sum_oi(view$low)
  oi_high <- sum_oi(view$high)
  if (!all(is.finite(c(oi_base, oi_low, oi_high)))) return("")
  eur <- function(x) format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
  win <- if (length(open) == 1L) open[[1]] else paste0(open[[1]], " \u2013 ", open[[length(open)]])
  bud_oi <- NA_real_
  n_missing <- 0L
  if (!is.null(budget) && nrow(budget)) {
    bud <- tryCatch(fpa_scenario_budget_oi(budget), error = function(e) NULL)
    if (!is.null(bud)) {
      vals <- suppressWarnings(as.numeric(bud$OI[match(open, as.character(bud$periode))]))
      n_missing <- sum(!is.finite(vals))
      if (!n_missing) bud_oi <- sum(vals)
    } else {
      n_missing <- length(open)
    }
  } else if (length(open)) {
    n_missing <- length(open)
  }
  if (is.finite(bud_oi)) {
    return(fpa_txt("scen_band_win", win, eur(oi_base), eur(oi_low), eur(oi_high), eur(bud_oi)))
  }
  if (n_missing > 0L) {
    return(fpa_txt(
      "scen_band_partial_budget_win", win, eur(oi_base), eur(oi_low), eur(oi_high),
      n_missing, length(open)
    ))
  }
  fpa_txt("scen_band_nobudget_win", win, eur(oi_base), eur(oi_low), eur(oi_high))
}

#' Closed-month and year-end cash picks from the bridge + period status.
fpa_cash_period_picks <- function(cash, status = NULL) {
  empty <- list(
    closed_month = "", closed = NA_real_,
    year_end_month = "", year_end = NA_real_,
    low_month = "", low = NA_real_, under = 0L
  )
  if (is.null(cash) || !nrow(cash) || !"cash_close" %in% names(cash)) return(empty)
  pers <- as.character(cash$periode)
  close_v <- suppressWarnings(as.numeric(cash$cash_close))
  closed <- ""
  if (!is.null(status) && nrow(status) && all(c("periode", "status") %in% names(status))) {
    closed <- tryCatch(fpa_health_closed_month(status), error = function(e) "")
    closed <- as.character(closed %||% "")
  }
  if (!nzchar(closed)) {
    # Fall back: last row that is not a future forecast if status missing.
    closed <- pers[[length(pers)]]
  }
  i_closed <- match(closed, pers)
  year <- if (nzchar(closed)) substr(closed, 1L, 4L) else substr(pers[[length(pers)]], 1L, 4L)
  ye <- paste0(year, "-12")
  i_ye <- match(ye, pers)
  if (is.na(i_ye)) i_ye <- length(pers)
  low_i <- if (any(is.finite(close_v))) which.min(close_v) else NA_integer_
  list(
    closed_month = if (!is.na(i_closed)) closed else "",
    closed = if (!is.na(i_closed)) close_v[[i_closed]] else NA_real_,
    year_end_month = as.character(pers[[i_ye]]),
    year_end = close_v[[i_ye]],
    low_month = if (!is.na(low_i)) as.character(pers[[low_i]]) else "",
    low = if (!is.na(low_i)) close_v[[low_i]] else NA_real_,
    under = as.integer(sum(close_v < 0, na.rm = TRUE))
  )
}

#' Snapshot of headline figures before an Actualiser rebuild.
fpa_snapshot_build <- function(pnl, cash, status, anom_counts, stale = character(),
                               cash_floor = 0, oi_year = NA_real_, config = NULL) {
  picks <- fpa_cash_period_picks(cash, status)
  pers <- if (!is.null(pnl) && nrow(pnl)) as.character(pnl$periode) else as.character(cash$periode %||% character(0))
  ye_info <- fpa_year_end_info(config %||% list(), status, pers)
  ye <- ye_info$measured
  oi_fy <- NA_real_
  if (!is.null(pnl) && nrow(pnl) && "OI" %in% names(pnl)) {
    if (!is.finite(oi_year)) oi_year <- fpa_objectif_read(list(pnl = pnl), "oi_ye", ye_info$close)
    oi_fy <- fpa_objectif_read(list(pnl = pnl), "oi_fy", ye_info$close)
  }
  if (!is.null(cash) && nrow(cash) && "cash_close" %in% names(cash)) {
    picks$year_end <- fpa_objectif_read(list(cash = cash), "cash_ye", ye_info$close)
    picks$year_end_month <- ye
  }
  under_floor_month <- ""
  if (is.finite(cash_floor) && cash_floor > 0 && !is.null(cash) && nrow(cash)) {
    close_v <- suppressWarnings(as.numeric(cash$cash_close))
    bad <- which(is.finite(close_v) & close_v < cash_floor)
    if (length(bad)) under_floor_month <- as.character(cash$periode[[bad[[1]]]])
  }
  list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    oi_year_end = if (is.finite(oi_year)) oi_year else NA_real_,
    oi_fy = if (is.finite(oi_fy)) oi_fy else NA_real_,
    year_end_month = ye,
    fy_close = ye_info$close,
    cash_year_end = picks$year_end,
    cash_closed = picks$closed,
    cash_closed_month = picks$closed_month,
    cash_low = picks$low,
    cash_low_month = picks$low_month,
    anom_act = as.integer(anom_counts$act %||% 0L),
    anom_watch = as.integer(anom_counts$watch %||% 0L),
    stale = as.character(stale %||% character()),
    cash_floor = suppressWarnings(as.numeric(cash_floor %||% 0)),
    under_floor_month = under_floor_month
  )
}

fpa_snapshot_path <- function(paths) {
  if (is.null(paths) || is.null(paths$work)) return("")
  file.path(paths$work, "last-snapshot.json")
}

fpa_snapshot_prev_path <- function(paths) {
  if (is.null(paths) || is.null(paths$work)) return("")
  file.path(paths$work, "prev-snapshot.json")
}

fpa_snapshot_read <- function(paths) {
  fp <- fpa_snapshot_path(paths)
  if (!nzchar(fp) || !file.exists(fp)) return(NULL)
  tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) NULL)
}

fpa_snapshot_read_prev <- function(paths) {
  fp <- fpa_snapshot_prev_path(paths)
  if (!nzchar(fp) || !file.exists(fp)) return(NULL)
  tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) NULL)
}

fpa_snapshot_write <- function(paths, snap) {
  fp <- fpa_snapshot_path(paths)
  if (!nzchar(fp)) return(FALSE)
  dir.create(dirname(fp), recursive = TRUE, showWarnings = FALSE)
  prev <- fpa_snapshot_prev_path(paths)
  if (file.exists(fp) && nzchar(prev)) {
    tryCatch(file.copy(fp, prev, overwrite = TRUE), error = function(e) FALSE)
  }
  jsonlite::write_json(snap, fp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  TRUE
}

#' Ranked material moves between two snapshots. Threshold: max(5% of prior, 1000 EUR).
fpa_snapshot_diff <- function(before, after, pct = 0.05, floor_eur = 1000, top_n = 3L) {
  if (is.null(before) || is.null(after)) return(list())
  num <- function(x) {
    v <- suppressWarnings(as.numeric(x %||% NA_real_))
    if (length(v) != 1L || is.na(v)) return(NA_real_)
    v
  }
  material <- function(old, new) {
    if (!is.finite(old) || !is.finite(new)) return(FALSE)
    abs(new - old) >= max(abs(old) * pct, floor_eur)
  }
  when <- as.character(before$timestamp %||% "")
  rows <- list()
  add <- function(id, label, old, new, tab = fpa_txt("loc_summary"), line = "", periode = "") {
    old <- num(old)
    new <- num(new)
    if (!material(old, new)) return()
    rows[[length(rows) + 1L]] <<- list(
      id = id, label = label, old = old, new = new, delta = new - old,
      when = when, tab = tab, line = line, periode = periode,
      abs_delta = abs(new - old)
    )
  }
  ye_lab <- as.character(after$year_end_month %||% "")
  if (!is.null(before$oi_fy) || !is.null(after$oi_fy)) {
    add("oi_fy", fpa_txt("obj_metric_oi_fy"), before$oi_fy, after$oi_fy, "Compte de résultat", "OI", ye_lab)
  }
  add(
    "oi_year_end",
    if (nzchar(ye_lab)) fpa_txt("obj_metric_oi_month", fpa_ym_fr(ye_lab)) else "Résultat d'exploitation de fin d'année",
    before$oi_year_end, after$oi_year_end, "Compte de résultat", "OI", if (nzchar(ye_lab)) ye_lab else after$cash_closed_month %||% ""
  )
  add("cash_year_end", "Trésorerie de fin d'année", before$cash_year_end, after$cash_year_end, "Trésorerie", "", after$year_end_month %||% as.character(after$cash_year_end %||% ""))
  add("cash_closed", fpa_txt("loc_cash_closed_month"), before$cash_closed, after$cash_closed, "Trésorerie", "", as.character(after$cash_closed_month %||% ""))
  if (!identical(as.character(before$cash_low_month %||% ""), as.character(after$cash_low_month %||% "")) ||
      material(num(before$cash_low), num(after$cash_low))) {
    if (is.finite(num(after$cash_low))) {
      rows[[length(rows) + 1L]] <- list(
        id = "cash_low",
        label = paste0(fpa_txt("loc_cash_low_open"), after$cash_low_month %||% "", ")"),
        old = num(before$cash_low), new = num(after$cash_low),
        delta = num(after$cash_low) - num(before$cash_low),
        when = when, tab = "Trésorerie", line = "", periode = as.character(after$cash_low_month %||% ""),
        abs_delta = abs(num(after$cash_low) - num(before$cash_low))
      )
    }
  }
  da <- as.integer(after$anom_act %||% 0L) - as.integer(before$anom_act %||% 0L)
  dw <- as.integer(after$anom_watch %||% 0L) - as.integer(before$anom_watch %||% 0L)
  if (da != 0L || dw != 0L) {
    rows[[length(rows) + 1L]] <- list(
      id = "anom",
      label = paste0("Anomalies ", as.integer(after$anom_act %||% 0L), fpa_txt("loc_to_clear_dot"), as.integer(after$anom_watch %||% 0L), fpa_txt("loc_to_follow")),
      old = as.numeric(before$anom_act %||% 0) + as.numeric(before$anom_watch %||% 0),
      new = as.numeric(after$anom_act %||% 0) + as.numeric(after$anom_watch %||% 0),
      delta = da + dw, when = when, tab = "Anomalies", line = "", periode = "",
      abs_delta = abs(da) + abs(dw) + 500
    )
  }
  stale_new <- setdiff(as.character(after$stale %||% character()), as.character(before$stale %||% character()))
  if (length(stale_new)) {
    rows[[length(rows) + 1L]] <- list(
      id = "stale",
      label = paste0(fpa_txt("loc_files_newer"), paste(stale_new, collapse = ", ")),
      old = 0, new = length(stale_new), delta = length(stale_new),
      when = when, tab = fpa_txt("loc_summary"), line = "", periode = "",
      abs_delta = 2000
    )
  }
  uf <- as.character(after$under_floor_month %||% "")
  if (nzchar(uf) && !identical(uf, as.character(before$under_floor_month %||% ""))) {
    rows[[length(rows) + 1L]] <- list(
      id = "floor",
      label = paste0(fpa_txt("loc_cash_under_floor"), uf),
      old = num(before$cash_floor), new = num(after$cash_floor),
      delta = 1, when = when, tab = "Impact", line = "", periode = uf,
      abs_delta = 5000
    )
  }
  if (!length(rows)) return(list())
  ord <- order(vapply(rows, function(r) r$abs_delta %||% 0, numeric(1)), decreasing = TRUE)
  rows <- rows[ord]
  if (length(rows) > top_n) rows <- rows[seq_len(top_n)]
  rows
}

#' Local overlay path next to the main config (or config_dir).
fpa_local_overlay_path <- function(config) {
  if (!is.null(config$`.config_path`) && nzchar(config$`.config_path`)) {
    return(file.path(dirname(config$`.config_path`), "fpa-config.local.json"))
  }
  paths <- tryCatch(fpa_paths(config), error = function(e) NULL)
  if (!is.null(paths) && !is.null(paths$config_dir)) {
    return(file.path(paths$config_dir, "fpa-config.local.json"))
  }
  "fpa-config.local.json"
}

fpa_cash_floor_read <- function(config) {
  fp <- fpa_local_overlay_path(config)
  if (!file.exists(fp)) return(0)
  overlay <- tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) list())
  v <- suppressWarnings(as.numeric(overlay$cash_floor %||% 0))
  if (!is.finite(v)) 0 else v
}

fpa_cash_floor_write <- function(config, value) {
  fp <- fpa_local_overlay_path(config)
  overlay <- if (file.exists(fp)) {
    tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) list())
  } else {
    list()
  }
  overlay$cash_floor <- suppressWarnings(as.numeric(value %||% 0))
  if (!is.finite(overlay$cash_floor)) overlay$cash_floor <- 0
  jsonlite::write_json(overlay, fp, auto_unbox = TRUE, pretty = TRUE)
  overlay$cash_floor
}

#' Rolling months past the last close. 12, 18 or 24. Default 18.
fpa_horizon_months_read <- function(config) {
  fp <- fpa_local_overlay_path(config)
  v <- 18L
  if (file.exists(fp)) {
    overlay <- tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) list())
    got <- suppressWarnings(as.integer(overlay$horizon_months %||% NA_integer_))
    if (is.finite(got) && got %in% c(12L, 18L, 24L)) v <- got
  }
  as.integer(v)
}

fpa_horizon_months_write <- function(config, value) {
  fp <- fpa_local_overlay_path(config)
  overlay <- if (file.exists(fp)) {
    tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) list())
  } else {
    list()
  }
  v <- suppressWarnings(as.integer(value %||% 18L))
  if (!is.finite(v) || !(v %in% c(12L, 18L, 24L))) v <- 18L
  overlay$horizon_months <- v
  jsonlite::write_json(overlay, fp, auto_unbox = TRUE, pretty = TRUE)
  as.integer(v)
}

#' One status word. actual covers réalisé, realise and clos. forecast covers prévision.
fpa_period_status_norm <- function(x) {
  raw <- tolower(trimws(as.character(x)))
  raw[is.na(raw)] <- ""
  key <- iconv(raw, from = "", to = "ASCII//TRANSLIT")
  key[is.na(key)] <- raw[is.na(key)]
  key <- gsub("[^a-z]", "", key)
  out <- raw
  out[key %in% c("actual", "realise", "reel", "clos", "closed")] <- "actual"
  out[key %in% c("forecast", "prevision", "prevu", "open", "ouvert")] <- "forecast"
  out
}
