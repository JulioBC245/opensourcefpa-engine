# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-outlook.R — Manual 4-week cash outlook, kept off the monthly model.
# LAST CHANGE: 2026-09-27
# The bank balance and the dated lines are typed. They never replace cash_close.

fpa_outlook_path <- function(config) {
  file.path(dirname(fpa_local_overlay_path(config)), "cash-outlook.json")
}

fpa_outlook_id <- function() {
  paste0("o", format(Sys.time(), "%H%M%S"), sprintf("%03d", sample.int(999L, 1L)))
}

fpa_outlook_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  raw <- as.character(x %||% "")
  if (!length(raw) || !nzchar(raw[[1]])) return(as.Date(NA))
  suppressWarnings(as.Date(raw[[1]]))
}

fpa_outlook_empty <- function() {
  list(as_of = as.character(Sys.Date()), opening = NA_real_, updated = "", items = list())
}

#' Four 7-day windows starting on as_of. Week 1 includes that day.
fpa_outlook_weeks <- function(as_of, n = 4L) {
  as_of <- fpa_outlook_date(as_of)
  n <- as.integer(n %||% 4L)
  if (!is.finite(as_of) || !is.finite(n) || n < 1L) return(list())
  lapply(seq_len(n), function(i) {
    list(
      i = i,
      start = as_of + (i - 1L) * 7L,
      end = as_of + i * 7L - 1L
    )
  })
}

fpa_outlook_normalize_item <- function(it) {
  if (is.null(it)) return(NULL)
  kind <- as.character(it$kind %||% "")
  if (!kind %in% c("in", "out")) return(NULL)
  amt <- abs(suppressWarnings(as.numeric(it$amount %||% NA_real_)))
  d <- fpa_outlook_date(it$date)
  lab <- trimws(as.character(it$label %||% ""))
  if (!nzchar(lab) || !is.finite(amt) || amt <= 0 || !is.finite(d)) return(NULL)
  list(
    id = as.character(it$id %||% fpa_outlook_id()),
    kind = kind,
    label = lab,
    amount = amt,
    date = as.character(d),
    uncertain = isTRUE(it$uncertain) && identical(kind, "in")
  )
}

fpa_outlook_read <- function(config) {
  fp <- fpa_outlook_path(config)
  base <- fpa_outlook_empty()
  if (!file.exists(fp)) return(base)
  raw <- tryCatch(jsonlite::fromJSON(fp, simplifyVector = FALSE), error = function(e) NULL)
  if (is.null(raw)) return(base)
  items <- lapply(raw$items %||% list(), fpa_outlook_normalize_item)
  items <- items[!vapply(items, is.null, logical(1))]
  opening <- suppressWarnings(as.numeric(raw$opening %||% NA_real_))
  if (!is.finite(opening)) opening <- NA_real_
  as_of <- fpa_outlook_date(raw$as_of %||% Sys.Date())
  if (!is.finite(as_of)) as_of <- Sys.Date()
  list(
    as_of = as.character(as_of),
    opening = opening,
    updated = as.character(raw$updated %||% ""),
    items = items
  )
}

fpa_outlook_write <- function(config, state) {
  fp <- fpa_outlook_path(config)
  items <- lapply(state$items %||% list(), fpa_outlook_normalize_item)
  items <- items[!vapply(items, is.null, logical(1))]
  opening <- suppressWarnings(as.numeric(state$opening %||% NA_real_))
  as_of <- fpa_outlook_date(state$as_of %||% Sys.Date())
  if (!is.finite(as_of)) as_of <- Sys.Date()
  doc <- list(
    as_of = as.character(as_of),
    opening = if (is.finite(opening)) opening else NA,
    updated = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    items = items
  )
  jsonlite::write_json(doc, fp, auto_unbox = TRUE, pretty = TRUE, na = "null")
  fpa_outlook_read(config)
}

#' Roll the typed lines. delay_uncertain pushes uncertain receipts by 7 days.
#' Payments on the same day are applied before receipts.
fpa_outlook_roll <- function(opening, items, as_of, delay_uncertain = FALSE, n = 4L) {
  as_of <- fpa_outlook_date(as_of)
  opening <- suppressWarnings(as.numeric(opening))
  weeks <- fpa_outlook_weeks(as_of, n)
  empty_weeks <- lapply(weeks, function(w) {
    w$inflow <- 0
    w$outflow <- 0
    w$close <- NA_real_
    w
  })
  if (!is.finite(as_of) || !is.finite(opening)) {
    return(list(
      ok = FALSE, reason = "need_balance", as_of = as_of, opening = opening,
      delay = isTRUE(delay_uncertain), weeks = empty_weeks,
      low = NA_real_, low_date = as.Date(NA), low_week = NA_integer_,
      low_items = character(0), beyond = 0L, before = 0L
    ))
  }
  horizon_end <- if (length(weeks)) weeks[[length(weeks)]]$end else as_of
  prep <- list()
  beyond <- 0L
  before <- 0L
  for (it in items %||% list()) {
    row <- fpa_outlook_normalize_item(it)
    if (is.null(row)) next
    d <- fpa_outlook_date(row$date)
    if (isTRUE(delay_uncertain) && isTRUE(row$uncertain)) d <- d + 7L
    if (d < as_of) {
      before <- before + 1L
      next
    }
    if (d > horizon_end) {
      beyond <- beyond + 1L
      next
    }
    row$date <- d
    prep[[length(prep) + 1L]] <- row
  }
  if (length(prep)) {
    ord <- order(
      vapply(prep, function(it) as.integer(it$date), integer(1)),
      vapply(prep, function(it) if (identical(it$kind, "out")) 0L else 1L, integer(1))
    )
    prep <- prep[ord]
  }
  bal <- opening
  low <- opening
  low_date <- as_of
  low_labels <- character(0)
  for (it in prep) {
    signed <- if (identical(it$kind, "out")) -it$amount else it$amount
    bal <- bal + signed
    if (bal < low - 1e-9) {
      low <- bal
      low_date <- it$date
      low_labels <- it$label
    } else if (abs(bal - low) <= 1e-9 && identical(it$date, low_date)) {
      low_labels <- unique(c(low_labels, it$label))
    }
    for (i in seq_along(weeks)) {
      w <- weeks[[i]]
      if (it$date >= w$start && it$date <= w$end) {
        if (identical(it$kind, "out")) {
          weeks[[i]]$outflow <- (weeks[[i]]$outflow %||% 0) + it$amount
        } else {
          weeks[[i]]$inflow <- (weeks[[i]]$inflow %||% 0) + it$amount
        }
      }
    }
  }
  running <- opening
  low_week <- NA_integer_
  for (i in seq_along(weeks)) {
    running <- running + (weeks[[i]]$inflow %||% 0) - (weeks[[i]]$outflow %||% 0)
    weeks[[i]]$inflow <- weeks[[i]]$inflow %||% 0
    weeks[[i]]$outflow <- weeks[[i]]$outflow %||% 0
    weeks[[i]]$close <- running
    if (is.finite(low_date) && low_date >= weeks[[i]]$start && low_date <= weeks[[i]]$end) {
      low_week <- i
    }
  }
  if (!length(prep)) low_week <- NA_integer_
  list(
    ok = TRUE,
    reason = if (!length(prep)) "balance_only" else "partial",
    as_of = as_of,
    opening = opening,
    delay = isTRUE(delay_uncertain),
    weeks = weeks,
    low = low,
    low_date = low_date,
    low_week = low_week,
    low_items = low_labels,
    beyond = beyond,
    before = before
  )
}

fpa_journal_from_outlook <- function(pack, roll, note = "") {
  declared <- identical(as.character(roll$reason %||% ""), "balance_only")
  fpa_journal_draft(
    pack,
    type = "observation",
    title = fpa_txt("out_journal_title"),
    note = note,
    periode = as.character((if (declared) roll$as_of else roll$low_date) %||% ""),
    source_tab = "Trésorerie",
    source_metric = "outlook_4w",
    amount = if (declared) roll$opening else roll$low,
    amount_label = if (declared) fpa_txt("out_journal_amt_open") else fpa_txt("out_journal_amt"),
    snapshot = list(
      as_of = as.character(roll$as_of %||% ""),
      opening = roll$opening,
      low = roll$low,
      low_date = as.character(roll$low_date %||% ""),
      delay = isTRUE(roll$delay),
      simple = TRUE,
      opening_source = as.character(roll$opening_source %||% "declared"),
      lines = lapply(roll$lines %||% list(), function(it) {
        list(
          kind = as.character(it$kind %||% "out"),
          label = as.character(it$label %||% ""),
          amount = suppressWarnings(as.numeric(it$amount)),
          date = as.character(fpa_outlook_date(it$date))
        )
      }),
      weeks = lapply(roll$weeks %||% list(), function(w) {
        list(
          i = w$i, end = as.character(w$end),
          inflow = w$inflow, outflow = w$outflow, close = w$close
        )
      })
    ),
    link = list(tab = "Trésorerie")
  )
}

#' Plain lines for a bank-balance journal entry. The raw snapshot stays in the file.
fpa_outlook_journal_bits <- function(snapshot) {
  sn <- snapshot %||% list()
  lines <- sn$lines %||% list()
  bits <- character()
  if (length(lines)) {
    for (it in lines) {
      kind <- if (identical(as.character(it$kind), "in")) fpa_txt("out_in") else fpa_txt("out_out")
      bits <- c(bits, fpa_txt(
        "journal_outlook_move", kind, as.character(it$label %||% ""),
        fpa_synthese_eur(it$amount), fpa_synth_date_fr(it$date)
      ))
    }
  }
  amt <- suppressWarnings(as.numeric(sn$opening))
  when <- fpa_synth_date_fr(sn$as_of)
  src <- as.character(sn$opening_source %||% "")
  if (is.finite(amt) && nzchar(when)) {
    key <- if (identical(src, "modeled")) "journal_outlook_from_model" else "journal_outlook_line"
    bits <- c(bits, fpa_txt(key, fpa_synthese_eur(amt), when))
  }
  weeks <- sn$weeks %||% list()
  if (!length(lines) && length(weeks)) {
    flows <- vapply(weeks, function(w) {
      sum(suppressWarnings(as.numeric(c(w$inflow, w$outflow))), na.rm = TRUE)
    }, numeric(1))
    if (length(flows) && all(is.finite(flows) & abs(flows) < 0.5)) {
      bits <- c(bits, fpa_txt("journal_outlook_flat"))
    }
  }
  bits
}
