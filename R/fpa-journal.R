# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-journal.R — Durable journal: observation / explored scenario / decision.
# LAST CHANGE: 2026-09-30 — journal with favorites and amount sort

FPA_JOURNAL_TYPES <- c("observation", "explored_scenario", "decision")
FPA_JOURNAL_CAP <- 100L

fpa_journal_path <- function(paths) {
  if (is.null(paths) || is.null(paths$work) || !nzchar(paths$work)) return("")
  file.path(paths$work, "journal.json")
}

fpa_journal_new_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "-",
    substr(paste(sample(c(0:9, letters), 6L, replace = TRUE), collapse = ""), 1L, 6L)
  )
}

#' Normalize one entry; fills defaults without dropping known fields.
fpa_journal_normalize <- function(raw) {
  if (is.null(raw) || !is.list(raw)) return(NULL)
  type <- as.character(raw$type %||% "observation")
  if (!(type %in% FPA_JOURNAL_TYPES)) type <- "observation"
  amt <- suppressWarnings(as.numeric(raw$amount %||% NA_real_))
  if (length(amt) != 1L || is.na(amt)) amt <- NA_real_
  snap <- raw$snapshot
  if (is.null(snap) || !is.list(snap)) snap <- list()
  link <- raw$link
  if (is.null(link) || !is.list(link)) link <- list(tab = "", line = "", periode = "")
  state <- as.character(raw$state %||% "")
  if (!nzchar(state)) state <- "open"
  if (!state %in% c("open", "watching", "done", "withdrawn")) state <- "open"
  list(
    id = as.character(raw$id %||% fpa_journal_new_id()),
    type = type,
    favorite = isTRUE(raw$favorite),
    created_at = as.character(raw$created_at %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    company = as.character(raw$company %||% ""),
    periode = as.character(raw$periode %||% ""),
    source_tab = as.character(raw$source_tab %||% ""),
    source_metric = as.character(raw$source_metric %||% ""),
    title = as.character(raw$title %||% ""),
    note = as.character(raw$note %||% ""),
    review_date = as.character(raw$review_date %||% ""),
    owner = as.character(raw$owner %||% ""),
    due = as.character(raw$due %||% ""),
    state = state,
    withdrawn_at = as.character(raw$withdrawn_at %||% ""),
    amount = amt,
    amount_label = as.character(raw$amount_label %||% ""),
    snapshot = snap,
    link = list(
      tab = as.character(link$tab %||% ""),
      line = as.character(link$line %||% ""),
      periode = as.character(link$periode %||% "")
    )
  )
}

#' TRUE when the entry still feeds live readings (bank diamond, follow-ups).
fpa_journal_is_active <- function(e) {
  !identical(as.character(e$state %||% ""), "withdrawn")
}

fpa_journal_read <- function(paths) {
  fp <- fpa_journal_path(paths)
  if (!nzchar(fp) || !file.exists(fp)) return(list())
  raw <- tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) list())
  if (!is.list(raw)) return(list())
  out <- lapply(raw, fpa_journal_normalize)
  Filter(Negate(is.null), out)
}

fpa_journal_write <- function(paths, entries) {
  fp <- fpa_journal_path(paths)
  if (!nzchar(fp)) return(FALSE)
  dir.create(dirname(fp), recursive = TRUE, showWarnings = FALSE)
  clean <- lapply(entries, fpa_journal_normalize)
  clean <- Filter(Negate(is.null), clean)
  if (length(clean) > FPA_JOURNAL_CAP) {
    clean <- clean[(length(clean) - FPA_JOURNAL_CAP + 1L):length(clean)]
  }
  jsonlite::write_json(clean, fp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  TRUE
}

fpa_journal_get <- function(entries, id) {
  id <- as.character(id %||% "")
  if (!nzchar(id) || !length(entries)) return(NULL)
  for (e in entries) {
    if (identical(as.character(e$id %||% ""), id)) return(e)
  }
  NULL
}

fpa_journal_add <- function(paths, entry) {
  entries <- fpa_journal_read(paths)
  e <- fpa_journal_normalize(entry)
  if (is.null(e)) return(list(ok = FALSE, entries = entries, id = ""))
  if (!nzchar(e$id)) e$id <- fpa_journal_new_id()
  if (!nzchar(e$created_at)) e$created_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  entries[[length(entries) + 1L]] <- e
  fpa_journal_write(paths, entries)
  list(ok = TRUE, entries = fpa_journal_read(paths), id = e$id)
}

fpa_journal_set_favorite <- function(paths, id, favorite = TRUE) {
  entries <- fpa_journal_read(paths)
  id <- as.character(id %||% "")
  for (i in seq_along(entries)) {
    if (identical(as.character(entries[[i]]$id %||% ""), id)) {
      entries[[i]]$favorite <- isTRUE(favorite)
      fpa_journal_write(paths, entries)
      return(list(ok = TRUE, entries = entries))
    }
  }
  list(ok = FALSE, entries = entries)
}

fpa_journal_update_note <- function(paths, id, note) {
  entries <- fpa_journal_read(paths)
  id <- as.character(id %||% "")
  for (i in seq_along(entries)) {
    if (identical(as.character(entries[[i]]$id %||% ""), id)) {
      entries[[i]]$note <- as.character(note %||% "")
      fpa_journal_write(paths, entries)
      return(list(ok = TRUE, entries = entries))
    }
  }
  list(ok = FALSE, entries = entries)
}

#' Keep the entry in the journal; stop it from feeding the live bank reading.
fpa_journal_withdraw <- function(paths, id) {
  entries <- fpa_journal_read(paths)
  id <- as.character(id %||% "")
  for (i in seq_along(entries)) {
    if (identical(as.character(entries[[i]]$id %||% ""), id)) {
      entries[[i]]$state <- "withdrawn"
      entries[[i]]$withdrawn_at <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
      fpa_journal_write(paths, entries)
      return(list(ok = TRUE, entries = entries, entry = entries[[i]]))
    }
  }
  list(ok = FALSE, entries = entries, entry = NULL)
}

#' Put a withdrawn entry back into the live reading.
fpa_journal_restore_keep <- function(paths, id) {
  entries <- fpa_journal_read(paths)
  id <- as.character(id %||% "")
  for (i in seq_along(entries)) {
    if (identical(as.character(entries[[i]]$id %||% ""), id)) {
      entries[[i]]$state <- "open"
      entries[[i]]$withdrawn_at <- ""
      fpa_journal_write(paths, entries)
      return(list(ok = TRUE, entries = entries, entry = entries[[i]]))
    }
  }
  list(ok = FALSE, entries = entries, entry = NULL)
}

#' Filter and sort journal entries for the list UI.
#' @param filter one of all, favorite, observation, explored_scenario, decision
#' @param sort one of newest, oldest, amount_desc, amount_asc, favorite_first
fpa_journal_list <- function(entries, filter = "all", sort = "newest") {
  if (!length(entries)) return(list())
  filter <- as.character(filter %||% "all")
  sort <- as.character(sort %||% "newest")
  keep <- entries
  if (identical(filter, "favorite")) {
    keep <- Filter(function(e) isTRUE(e$favorite), keep)
  } else if (filter %in% FPA_JOURNAL_TYPES) {
    keep <- Filter(function(e) identical(as.character(e$type %||% ""), filter), keep)
  }
  if (!length(keep)) return(list())
  ord <- seq_along(keep)
  if (identical(sort, "oldest")) {
    ord <- order(vapply(keep, function(e) as.character(e$created_at %||% ""), character(1)))
  } else if (identical(sort, "amount_desc")) {
    ord <- order(
      vapply(keep, function(e) {
        v <- suppressWarnings(as.numeric(e$amount))
        if (!is.finite(v)) -Inf else abs(v)
      }, numeric(1)),
      decreasing = TRUE
    )
  } else if (identical(sort, "amount_asc")) {
    ord <- order(vapply(keep, function(e) {
      v <- suppressWarnings(as.numeric(e$amount))
      if (!is.finite(v)) Inf else abs(v)
    }, numeric(1)))
  } else if (identical(sort, "favorite_first")) {
    fav <- vapply(keep, function(e) isTRUE(e$favorite), logical(1))
    ts <- vapply(keep, function(e) as.character(e$created_at %||% ""), character(1))
    ord <- order(!fav, ts, decreasing = c(FALSE, TRUE))
  } else {
    # newest
    ord <- order(vapply(keep, function(e) as.character(e$created_at %||% ""), character(1)), decreasing = TRUE)
  }
  keep[ord]
}

fpa_journal_type_label <- function(type) {
  switch(
    as.character(type %||% ""),
    observation = fpa_txt("journal_type_observation"),
    explored_scenario = fpa_txt("journal_type_scenario"),
    decision = fpa_txt("journal_type_decision"),
    as.character(type %||% "")
  )
}

#' Build entry skeleton from pack context + capture payload.
fpa_journal_draft <- function(pack, type = "observation", title = "", note = "",
                              periode = "", source_tab = "", source_metric = "",
                              amount = NA_real_, amount_label = "",
                              snapshot = list(), link = list(),
                              review_date = "", favorite = FALSE) {
  company <- ""
  if (!is.null(pack) && isTRUE(pack$ok)) {
    company <- as.character((pack$config$company %||% list())$name %||% "")
  }
  if (!nzchar(periode) && !is.null(pack) && isTRUE(pack$ok)) {
    periode <- tryCatch(fpa_health_closed_month(pack$finance$period_status), error = function(e) "")
  }
  fpa_journal_normalize(list(
    id = fpa_journal_new_id(),
    type = type,
    favorite = favorite,
    created_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    company = company,
    periode = periode,
    source_tab = source_tab,
    source_metric = source_metric,
    title = title,
    note = note,
    review_date = review_date,
    amount = amount,
    amount_label = amount_label,
    snapshot = snapshot,
    link = link
  ))
}

fpa_journal_from_scenario <- function(pack, scn_entry, note = "", type = "explored_scenario") {
  e <- scn_entry
  amt <- suppressWarnings(as.numeric(e$cash_delta %||% NA_real_))
  fpa_journal_draft(
    pack,
    type = type,
    title = as.character(e$moves %||% fpa_txt("journal_title_scenario")),
    note = note,
    periode = as.character(e$low_month %||% ""),
    source_tab = "Impact",
    source_metric = "scenario",
    amount = amt,
    amount_label = fpa_txt("journal_amt_cash_gap"),
    snapshot = list(
      at = e$at,
      moves = e$moves,
      cash_close = e$cash_close,
      cash_delta = e$cash_delta,
      oi = e$oi,
      oi_delta = e$oi_delta,
      oi_fy = e$oi_fy,
      oi_last = e$oi_last,
      base_oi_fy = e$base_oi_fy,
      base_oi_last = e$base_oi_last,
      ye_month = e$ye_month,
      fy_months = e$fy_months,
      metric = as.character((e$objectif %||% list())$metric %||% ""),
      base_cash = e$base_cash,
      base_oi = e$base_oi,
      floor = e$floor,
      under_months = e$under_months,
      deltas = e$deltas
    ),
    link = list(tab = "Impact", line = "", periode = "")
  )
}

fpa_journal_from_anomaly <- function(pack, family, line_id, periode, gap, sentence, note = "") {
  fpa_journal_draft(
    pack,
    type = "observation",
    title = sentence,
    note = note,
    periode = periode,
    source_tab = "Anomalies",
    source_metric = paste(family, line_id, sep = "/"),
    amount = suppressWarnings(as.numeric(gap)),
    amount_label = if (identical(as.character(family), "ratio")) fpa_txt("journal_amt_cash") else fpa_txt("journal_amt_gap"),
    snapshot = list(
      family = family,
      line_id = line_id,
      periode = periode,
      gap = gap,
      sentence = sentence
    ),
    link = list(tab = "Anomalies", line = as.character(line_id %||% ""), periode = as.character(periode %||% ""))
  )
}

fpa_journal_from_kpi <- function(pack, card_id, card_k, card_v, card_s, note = "") {
  amt <- suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", as.character(card_v %||% ""))))
  fpa_journal_draft(
    pack,
    type = "observation",
    title = paste(card_k, card_v),
    note = note,
    source_tab = fpa_txt("loc_summary"),
    source_metric = as.character(card_id %||% ""),
    amount = if (is.finite(amt)) amt else NA_real_,
    amount_label = as.character(card_k %||% ""),
    snapshot = list(id = card_id, k = card_k, v = card_v, s = card_s),
    link = list(tab = fpa_txt("loc_summary"), line = "", periode = "")
  )
}

fpa_journal_from_fifo <- function(pack, text, note = "") {
  fpa_journal_draft(
    pack,
    type = "observation",
    title = as.character(text %||% ""),
    note = note,
    source_tab = "Compte de résultat",
    source_metric = "cascade",
    amount = NA_real_,
    amount_label = "",
    snapshot = list(text = text),
    link = list(tab = "Compte de résultat", line = "", periode = "")
  )
}

#' Review date as typed: YYYY-MM-DD, DD/MM/YYYY, or YYYY-MM.
fpa_journal_review_date <- function(x) {
  raw <- trimws(as.character(x %||% ""))
  if (!length(raw) || !nzchar(raw[[1]])) return(as.Date(NA))
  raw <- raw[[1]]
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", raw)) return(as.Date(raw))
  if (grepl("^\\d{2}/\\d{2}/\\d{4}$", raw)) return(as.Date(raw, "%d/%m/%Y"))
  if (grepl("^\\d{4}-\\d{2}$", raw)) return(as.Date(paste0(raw, "-01")))
  as.Date(NA)
}

fpa_journal_actual_at <- function(df, periode, col) {
  if (is.null(df) || !nrow(df) || !col %in% names(df) || !"periode" %in% names(df)) return(NA_real_)
  i <- match(as.character(periode), as.character(df$periode))
  if (is.na(i)) return(NA_real_)
  suppressWarnings(as.numeric(df[[col]][[i]]))
}

fpa_journal_month_actual <- function(status, periode) {
  if (is.null(status) || !nrow(status) || !all(c("periode", "status") %in% names(status))) return(FALSE)
  i <- match(as.character(periode), as.character(status$periode))
  if (is.na(i)) return(FALSE)
  identical(as.character(status$status[[i]]), "actual")
}

#' Decision versus the actual now on file. The frozen number is not rewritten.
fpa_journal_followup <- function(entry, cash = NULL, pnl = NULL, status = NULL, today = Sys.Date()) {
  blank <- list(
    show = FALSE, due = FALSE, state = "skip", frozen = NA_real_, actual = NA_real_,
    gap = NA_real_, label = "", periode = "", review = as.Date(NA)
  )
  if (is.null(entry) || !identical(as.character(entry$type %||% ""), "decision")) return(blank)
  periode <- as.character(entry$periode %||% "")
  snap <- entry$snapshot %||% list()
  frozen_cash <- suppressWarnings(as.numeric(snap$cash_close %||% NA_real_))
  frozen_oi <- suppressWarnings(as.numeric(snap$oi %||% NA_real_))
  frozen_amt <- suppressWarnings(as.numeric(entry$amount %||% NA_real_))
  label <- ""
  frozen <- NA_real_
  actual <- NA_real_
  metric <- as.character(snap$metric %||% "")
  ye_month <- as.character(snap$ye_month %||% "")
  fy_months <- as.character(unlist(snap$fy_months %||% list()))
  if (identical(metric, "oi_fy") && is.finite(suppressWarnings(as.numeric(snap$oi_fy %||% NA_real_)))) {
    label <- fpa_txt("obj_metric_oi_fy")
    frozen <- as.numeric(snap$oi_fy)
    if (nzchar(ye_month)) periode <- ye_month
    if (!is.null(pnl) && nrow(pnl) && all(c("periode", "OI") %in% names(pnl)) && length(fy_months)) {
      keep <- as.character(pnl$periode) %in% fy_months
      if (any(keep)) actual <- sum(suppressWarnings(as.numeric(pnl$OI[keep])), na.rm = TRUE)
    }
  } else if (identical(metric, "oi_ye") && is.finite(suppressWarnings(as.numeric(snap$oi_last %||% NA_real_)))) {
    label <- if (nzchar(ye_month)) fpa_txt("obj_metric_oi_month", fpa_ym_fr(ye_month)) else "Résultat d'exploitation"
    frozen <- as.numeric(snap$oi_last)
    if (nzchar(ye_month)) periode <- ye_month
    actual <- fpa_journal_actual_at(pnl, periode, "OI")
  } else if (is.finite(frozen_cash)) {
    label <- "Trésorerie"
    frozen <- frozen_cash
    actual <- fpa_journal_actual_at(cash, periode, "cash_close")
  } else if (is.finite(frozen_oi)) {
    label <- "Résultat d'exploitation"
    frozen <- frozen_oi
    actual <- fpa_journal_actual_at(pnl, periode, "OI")
  } else if (is.finite(frozen_amt)) {
    label <- as.character(entry$amount_label %||% "")
    frozen <- frozen_amt
    lab <- tolower(label)
    if (grepl("trésorerie|treso|cash", lab)) actual <- fpa_journal_actual_at(cash, periode, "cash_close")
    else if (grepl("exploitation|\\boi\\b", lab)) actual <- fpa_journal_actual_at(pnl, periode, "OI")
  }
  review <- fpa_journal_review_date(entry$review_date)
  today <- as.Date(today)
  due <- is.finite(review) && is.finite(today) && review <= today
  is_actual <- nzchar(periode) && fpa_journal_month_actual(status, periode)
  state <- if (!nzchar(periode)) "no_period" else if (is_actual) "actual" else "open"
  gap <- if (is.finite(frozen) && is.finite(actual)) actual - frozen else NA_real_
  blank$show <- TRUE
  blank$due <- due
  blank$state <- state
  blank$frozen <- frozen
  blank$actual <- actual
  blank$gap <- gap
  blank$label <- label
  blank$periode <- periode
  blank$review <- review
  blank
}

fpa_journal_followup_text <- function(fu) {
  if (!isTRUE(fu$show)) return("")
  eur <- fpa_shiny_eur_plain
  if (identical(fu$state, "actual") && is.finite(fu$gap)) {
    return(fpa_txt(
      "journal_fu_actual", fu$periode, eur(fu$actual), eur(fu$frozen), eur(fu$gap)
    ))
  }
  if (identical(fu$state, "open") && is.finite(fu$frozen)) {
    return(fpa_txt("journal_fu_open", fu$periode, eur(fu$frozen)))
  }
  if (is.finite(fu$review) && !isTRUE(fu$due) && is.finite(fu$frozen)) {
    return(fpa_txt("journal_fu_wait", format(fu$review, "%d/%m/%Y"), eur(fu$frozen)))
  }
  if (is.finite(fu$frozen)) {
    return(fpa_txt("journal_fu_open", if (nzchar(fu$periode %||% "")) fu$periode else "—", eur(fu$frozen)))
  }
  ""
}

fpa_todo_cleared_path <- function(paths) {
  if (is.null(paths) || !nzchar(as.character(paths$work %||% ""))) return("")
  file.path(paths$work, "todo-cleared.json")
}

#' Rows the user has taken off À faire. One row per id, latest check wins.
fpa_todo_cleared_read <- function(paths) {
  fp <- fpa_todo_cleared_path(paths)
  if (!nzchar(fp) || !file.exists(fp)) return(list())
  raw <- tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) list())
  if (!is.list(raw) || !length(raw)) return(list())
  if (!is.null(raw$id)) raw <- list(raw)
  Filter(function(e) is.list(e) && nzchar(as.character(e$id %||% "")), raw)
}

fpa_todo_cleared_write <- function(paths, entries) {
  fp <- fpa_todo_cleared_path(paths)
  if (!nzchar(fp)) return(FALSE)
  dir.create(dirname(fp), recursive = TRUE, showWarnings = FALSE)
  if (!length(entries)) {
    writeLines("[]", fp, useBytes = TRUE)
    return(TRUE)
  }
  jsonlite::write_json(entries, fp, auto_unbox = TRUE, pretty = TRUE, null = "null")
  TRUE
}

fpa_todo_cleared_add <- function(paths, id, title = "", sentence = "", via = "check", stamp = "") {
  id <- as.character(id %||% "")
  if (!nzchar(id)) return(fpa_todo_cleared_read(paths))
  entries <- fpa_todo_cleared_read(paths)
  row <- list(
    id = id,
    title = as.character(title %||% ""),
    sentence = as.character(sentence %||% ""),
    at = format(Sys.time(), "%Y-%m-%d %H:%M"),
    via = if (as.character(via %||% "") %in% c("journal", "scan", "check")) as.character(via) else "check",
    stamp = as.character(stamp %||% "")
  )
  hit <- FALSE
  for (i in seq_along(entries)) {
    if (identical(as.character(entries[[i]]$id %||% ""), id)) {
      entries[[i]] <- row
      hit <- TRUE
    }
  }
  if (!hit) entries[[length(entries) + 1L]] <- row
  fpa_todo_cleared_write(paths, entries)
  entries
}

#' Log rows that a fresh scan no longer produces. A row that comes back leaves this log.
#' Returns how many rows were recorded. A manual check is left as the user made it.
fpa_todo_retire_resolved <- function(paths, before_tg, after_tg) {
  after_ids <- vapply(fpa_todo_flat(after_tg), function(it) as.character(it$id %||% ""), character(1))
  if (!length(after_ids)) after_ids <- character()
  for (e in fpa_todo_cleared_read(paths)) {
    if (identical(as.character(e$via %||% ""), "scan") && as.character(e$id %||% "") %in% after_ids) {
      fpa_todo_cleared_remove(paths, e$id)
    }
  }
  before_items <- fpa_todo_flat(fpa_todo_hide_cleared(before_tg, fpa_todo_cleared_read(paths)))
  n <- 0L
  for (it in before_items) {
    id <- as.character(it$id %||% "")
    if (!nzchar(id) || id %in% after_ids) next
    fpa_todo_cleared_add(paths, id, it$title %||% "", it$sentence %||% "", "scan", it$stamp %||% "")
    n <- n + 1L
  }
  n
}

fpa_todo_cleared_remove <- function(paths, id) {
  id <- as.character(id %||% "")
  entries <- Filter(function(e) !identical(as.character(e$id %||% ""), id), fpa_todo_cleared_read(paths))
  fpa_todo_cleared_write(paths, entries)
  entries
}

#' Decisions whose review date has arrived. Same set as « Décisions à revoir ».
fpa_journal_due_items <- function(entries, cash = NULL, pnl = NULL, status = NULL, today = Sys.Date()) {
  out <- list()
  for (e in entries %||% list()) {
    if (!fpa_journal_is_active(e)) next
    fu <- fpa_journal_followup(e, cash, pnl, status, today)
    if (!isTRUE(fu$show) || !isTRUE(fu$due)) next
    sentence <- fpa_journal_followup_text(fu)
    if (!nzchar(sentence)) next
    out[[length(out) + 1L]] <- list(
      id = as.character(e$id %||% ""),
      title = as.character(e$title %||% ""),
      sentence = sentence,
      neg = is.finite(fu$gap) && fu$gap < 0,
      stamp = as.character(e$review_date %||% "")
    )
  }
  out
}

#' Starred decisions, one block of lines for the management report.
fpa_journal_report_lines <- function(entries, cash = NULL, pnl = NULL, status = NULL, today = Sys.Date()) {
  picked <- Filter(function(e) {
    identical(as.character(e$type %||% ""), "decision") && isTRUE(e$favorite)
  }, entries %||% list())
  if (!length(picked)) return(c(fpa_txt("journal_report_head"), fpa_txt("journal_report_none")))
  lines <- fpa_txt("journal_report_head")
  for (e in picked) {
    fu <- fpa_journal_followup(e, cash, pnl, status, today)
    lines <- c(lines, paste(e$created_at %||% "", e$title %||% ""))
    sentence <- fpa_journal_followup_text(fu)
    if (nzchar(sentence)) lines <- c(lines, sentence)
  }
  lines
}

#' Report text has no Shiny formatter in scope when the module is tested alone.
fpa_shiny_eur_plain <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("—")
  format(round(x[[1]]), big.mark = " ", scientific = FALSE, trim = TRUE)
}
