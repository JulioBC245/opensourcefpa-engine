# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Statement view maths: percent of a base, month-on-month delta, P&L waterfall.
# The waterfall does not use plotly measure = "total". Totals are bars from zero.

fpa_stmt_ratio <- function(value, base) {
  v <- suppressWarnings(as.numeric(value))
  b <- suppressWarnings(as.numeric(base))
  if (!is.finite(v) || !is.finite(b) || abs(b) < 0.5) return(NA_real_)
  100 * v / b
}

fpa_stmt_delta <- function(value, prev) {
  v <- suppressWarnings(as.numeric(value))
  p <- suppressWarnings(as.numeric(prev))
  if (!is.finite(v) || !is.finite(p)) return(NA_real_)
  v - p
}

#' Normalise a two-handle slider to sorted 1..n indices.
fpa_pnl_range_bounds <- function(rg, n) {
  n <- as.integer(n)[[1]]
  if (!is.finite(n) || n < 1L) return(NULL)
  rg <- suppressWarnings(as.integer(rg))
  if (length(rg) < 2L || any(is.na(rg))) return(NULL)
  i0 <- max(1L, min(rg[[1]], n))
  i1 <- max(1L, min(rg[[2]], n))
  if (i1 < i0) {
    tmp <- i0
    i0 <- i1
    i1 <- tmp
  }
  c(i0, i1)
}

#' Live months replace the same month from history. Other history months stay.
#' The result is one statement, oldest month first.
fpa_pnl_wf_book <- function(live, history = NULL) {
  take <- function(df) {
    if (is.null(df) || !NROW(df) || !"periode" %in% names(df)) return(NULL)
    df <- as.data.frame(df, stringsAsFactors = FALSE)
    df$periode <- as.character(df$periode)
    df <- df[grepl("^[0-9]{4}-[0-9]{2}$", df$periode), , drop = FALSE]
    if (!nrow(df)) return(NULL)
    df
  }
  h <- take(history)
  v <- take(live)
  if (is.null(h) && is.null(v)) return(NULL)
  if (is.null(h)) return(v[order(v$periode), , drop = FALSE])
  if (is.null(v)) return(h[order(h$periode), , drop = FALSE])
  cols <- union(names(h), names(v))
  for (nm in setdiff(cols, names(h))) h[[nm]] <- NA_real_
  for (nm in setdiff(cols, names(v))) v[[nm]] <- NA_real_
  h <- h[, cols, drop = FALSE]
  v <- v[, cols, drop = FALSE]
  h <- h[!h$periode %in% v$periode, , drop = FALSE]
  out <- rbind(h, v)
  out[order(out$periode), , drop = FALSE]
}

#' Calendar years that have at least one month, oldest first.
fpa_pnl_wf_years <- function(book) {
  if (is.null(book) || !nrow(book)) return(character())
  sort(unique(substr(as.character(book$periode), 1, 4)))
}

#' Keep a chosen year when it exists. Otherwise the newest year.
fpa_pnl_wf_year_pick <- function(selected, years) {
  years <- as.character(years)
  sel <- as.character(if (is.null(selected) || !length(selected)) "" else selected[[1]])
  if (nzchar(sel) && sel %in% years) return(sel)
  if (length(years)) years[[length(years)]] else ""
}

#' Months of one calendar year. Slider position 1 is January, 12 is December.
#' `found` is what the book actually holds inside that span.
fpa_pnl_wf_span <- function(book, year, rg) {
  year <- as.character(if (is.null(year) || !length(year)) "" else year[[1]])
  if (!grepl("^[0-9]{4}$", year)) return(NULL)
  bounds <- fpa_pnl_range_bounds(rg, 12L)
  if (is.null(bounds)) return(NULL)
  asked <- sprintf("%s-%02d", year, seq.int(bounds[[1]], bounds[[2]]))
  have <- if (is.null(book) || !nrow(book)) character() else as.character(book$periode)
  list(
    year = year,
    i0 = bounds[[1]],
    i1 = bounds[[2]],
    asked = asked,
    found = asked[asked %in% have],
    missing = asked[!asked %in% have]
  )
}

#' Compact period label, e.g. 2026-10–2026-12.
fpa_pnl_range_label <- function(pers, i0, i1) {
  pers <- as.character(pers)
  if (!length(pers) || is.na(i0) || is.na(i1)) return("")
  a <- pers[[i0]]
  b <- pers[[i1]]
  if (identical(a, b)) a else paste0(a, "\u2013", b)
}

#' Biggest moves in share of revenue (Chiffre d'affaires) between two P&L rows.
#' Each line is `100 * line / REV`. The score is the change in that share
#' (percentage points of CA). Revenue itself is skipped (always 100 %).
fpa_pnl_waterfall_compare <- function(row_prev, row_next, top_n = 3L) {
  if (is.null(row_prev) || is.null(row_next)) return(NULL)
  ids <- c(
    "OTHER_OP_INC", "COGS", "GP", "PAYROLL", "OPEX_OTHER", "EBITDA",
    "D_A", "OI", "FIN", "EXCEP", "TAX", "NI"
  )
  ids <- ids[ids %in% names(row_prev) & ids %in% names(row_next)]
  if (!length(ids)) return(NULL)
  num <- function(row, id) {
    if (!id %in% names(row)) return(NA_real_)
    v <- suppressWarnings(as.numeric(row[[id]][[1]]))
    if (!is.finite(v)) NA_real_ else v
  }
  rev_a <- num(row_prev, "REV")
  rev_b <- num(row_next, "REV")
  if (!is.finite(rev_a) || !is.finite(rev_b) || abs(rev_a) < 0.5 || abs(rev_b) < 0.5) {
    return(NULL)
  }
  out <- list()
  for (id in ids) {
    a <- num(row_prev, id)
    b <- num(row_next, id)
    if (!is.finite(a) || !is.finite(b)) next
    sa <- 100 * a / rev_a
    sb <- 100 * b / rev_b
    delta <- sb - sa
    if (!is.finite(delta) || abs(delta) < 0.05) next
    out[[length(out) + 1L]] <- data.frame(id = id, pct = delta, stringsAsFactors = FALSE)
  }
  if (!length(out)) return(NULL)
  tab <- do.call(rbind, out)
  tab <- tab[order(-abs(tab$pct), tab$id), , drop = FALSE]
  head(tab, as.integer(top_n)[[1]])
}

#' Push one comparison into a FIFO of at most `cap` entries (newest first).
fpa_pnl_fifo_push <- function(mem, entry, cap = 5L) {
  mem <- if (is.null(mem)) list() else as.list(mem)
  mem <- c(list(entry), mem)
  cap <- max(1L, as.integer(cap)[[1]])
  if (length(mem) > cap) mem <- mem[seq_len(cap)]
  mem
}

#' Sum flow lines across months into one waterfall row.
fpa_pnl_waterfall_sum <- function(pnl, periods) {
  if (is.null(pnl) || !nrow(pnl)) return(NULL)
  pers <- as.character(pnl$periode)
  wanted <- as.character(periods)
  if (!length(wanted) || all(is.na(wanted))) wanted <- character(0)
  wanted <- wanted[nzchar(wanted) & !is.na(wanted)]
  wanted <- wanted[wanted %in% pers]
  if (!length(wanted)) return(NULL)
  rows <- pnl[pers %in% wanted, , drop = FALSE]
  ids <- setdiff(names(rows), "periode")
  out <- as.list(vapply(ids, function(id) {
    v <- suppressWarnings(as.numeric(rows[[id]]))
    sum(v[is.finite(v)], na.rm = TRUE)
  }, numeric(1)))
  out$periode <- if (length(wanted) == 1L) {
    wanted[[1]]
  } else {
    paste0(wanted[[1]], " \u2192 ", wanted[[length(wanted)]])
  }
  as.data.frame(out, stringsAsFactors = FALSE)
}

#' Hover lines: margin and share of revenue, in French.
fpa_pnl_waterfall_hover <- function(bars) {
  if (is.null(bars) || !nrow(bars)) return(character(0))
  ref <- function(id) {
    i <- match(id, bars$id)
    if (is.na(i)) return(NA_real_)
    suppressWarnings(as.numeric(bars$value[[i]]))
  }
  rev <- ref("REV")
  gp <- ref("GP")
  ebitda <- ref("EBITDA")
  oi <- ref("OI")
  pct_of <- function(v, denom, lab) {
    if (!is.finite(v) || !is.finite(denom) || abs(denom) < 0.5) return(NULL)
    paste0(
      formatC(100 * abs(v) / abs(denom), format = "f", digits = 1, decimal.mark = ","),
      " % du ", lab
    )
  }
  margin <- function(v, denom, lab) {
    if (!is.finite(v) || !is.finite(denom) || abs(denom) < 0.5) return(NULL)
    paste0(
      lab, " : ",
      formatC(100 * abs(v) / abs(denom), format = "f", digits = 1, decimal.mark = ","),
      " %"
    )
  }
  vapply(seq_len(nrow(bars)), function(i) {
    id <- as.character(bars$id[[i]])
    v <- suppressWarnings(as.numeric(bars$value[[i]]))
    bits <- switch(
      id,
      OTHER_OP_INC = list(pct_of(v, rev, "chiffre d'affaires")),
      COGS = list(pct_of(v, rev, "chiffre d'affaires")),
      GP = list(margin(v, rev, "Marge brute"), pct_of(v, rev, "chiffre d'affaires")),
      PAYROLL = list(pct_of(v, rev, "chiffre d'affaires"), pct_of(v, gp, "la marge brute")),
      OPEX_OTHER = list(pct_of(v, rev, "chiffre d'affaires"), pct_of(v, gp, "la marge brute")),
      EBITDA = list(margin(v, rev, "Marge d'EBE"), pct_of(v, gp, "la marge brute")),
      D_A = list(pct_of(v, rev, "chiffre d'affaires"), pct_of(v, ebitda, "l'EBE")),
      OI = list(margin(v, rev, "Marge d'exploitation"), pct_of(v, ebitda, "l'EBE")),
      FIN = list(pct_of(v, rev, "chiffre d'affaires"), pct_of(v, oi, "le r\u00e9sultat d'exploitation")),
      EXCEP = list(pct_of(v, rev, "chiffre d'affaires")),
      TAX = list(pct_of(v, oi, "le r\u00e9sultat d'exploitation")),
      NI = list(margin(v, rev, "Marge nette"), pct_of(v, oi, "le r\u00e9sultat d'exploitation")),
      list()
    )
    bits <- Filter(Negate(is.null), bits)
    if (!length(bits)) return("")
    paste(unlist(bits), collapse = "<br>")
  }, character(1))
}

#' One month of the income statement as a cascade.
#' Costs stored as positive amounts are signed negative. Totals are levels.
#' `ok` is TRUE when each total matches the running sum within 1 EUR.
fpa_pnl_waterfall_build <- function(row) {
  if (is.null(row) || !nrow(row)) return(NULL)
  num <- function(id) {
    if (!id %in% names(row)) return(0)
    v <- suppressWarnings(as.numeric(row[[id]][[1]]))
    if (!is.finite(v)) 0 else v
  }
  spec <- data.frame(
    id = c(
      "REV", "OTHER_OP_INC", "COGS", "GP",
      "PAYROLL", "OPEX_OTHER", "EBITDA",
      "D_A", "OI", "FIN", "EXCEP", "TAX", "NI"
    ),
    kind = c(
      "total", "step", "step", "total",
      "step", "step", "total",
      "step", "total", "step", "step", "step", "total"
    ),
    sign = c(1, 1, -1, 1, -1, -1, 1, -1, 1, -1, -1, -1, 1),
    stringsAsFactors = FALSE
  )
  running <- 0
  ok <- TRUE
  first <- TRUE
  bars <- list()
  for (i in seq_len(nrow(spec))) {
    level <- spec$sign[[i]] * num(spec$id[[i]])
    if (identical(spec$kind[[i]], "total")) {
      if (!first && abs(running - level) > 1) ok <- FALSE
      bars[[length(bars) + 1L]] <- data.frame(
        id = spec$id[[i]], kind = "total",
        value = level, base = 0, end = level,
        stringsAsFactors = FALSE
      )
      running <- level
      first <- FALSE
    } else {
      if (abs(level) < 0.5) next
      bars[[length(bars) + 1L]] <- data.frame(
        id = spec$id[[i]], kind = "step",
        value = level, base = running, end = running + level,
        stringsAsFactors = FALSE
      )
      running <- running + level
      first <- FALSE
    }
  }
  if (!length(bars)) return(NULL)
  out <- do.call(rbind, bars)
  # Pitch wider than the bar so a dotted connector has a visible gap.
  pitch <- 1.65
  width <- 0.78
  out$x <- (seq_len(nrow(out)) - 1) * pitch
  out$hover_extra <- fpa_pnl_waterfall_hover(out)
  connectors <- NULL
  if (nrow(out) > 1L) {
    connectors <- data.frame(
      x0 = out$x[-nrow(out)] + width / 2,
      x1 = out$x[-1L] - width / 2,
      y = out$end[-nrow(out)],
      stringsAsFactors = FALSE
    )
  }
  list(bars = out, connectors = connectors, width = width, ok = ok)
}
