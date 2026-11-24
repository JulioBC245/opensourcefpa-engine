# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-drivers.R — extra series (meteo, PMS, kWh): history, living, or split
# LAST CHANGE: 2026-10-07 — a living file keeps a month that is present; only a missing open month is filled

#' TRUE if this source advances the rolling forecast.
fpa_source_is_rolling <- function(s) {
  fs <- as.character(s$feed_shape %||% "")
  if (identical(fs, "history_only")) return(FALSE)
  if (identical(fs, "living")) return(TRUE)
  if (identical(fs, "split")) {
    pr <- as.character(s$pair_role %||% "")
    return(identical(pr, "rolling"))
  }
  tf <- as.character(s$time_frame %||% "")
  if (nzchar(tf)) return(identical(tf, "rolling"))
  g <- as.character(s$grain %||% "")
  g %in% c("month", "week", "day", "transaction")
}

#' TRUE if this source feeds correlation / driver measurement.
fpa_source_uses_correlation <- function(s) {
  fs <- as.character(s$feed_shape %||% "")
  if (identical(fs, "history_only") || identical(fs, "living")) return(TRUE)
  if (identical(fs, "split")) {
    return(!identical(as.character(s$pair_role %||% ""), "rolling"))
  }
  uses <- s$uses_for
  if (is.character(uses) && "correlation" %in% uses) return(TRUE)
  identical(as.character(s$file_role %||% ""), "ops_other")
}

fpa_extra_path_for <- function(config, source_id, paths = fpa_paths(config)) {
  extras <- config$extra_files %||% list()
  for (ex in extras) {
    if (identical(as.character(ex$source_id %||% ""), as.character(source_id))) {
      p <- as.character(ex$path %||% "")
      if (nzchar(p)) return(p)
    }
  }
  NA_character_
}

#' Group driver sources: one living file, a split pair, or history-only.
fpa_driver_groups <- function(config) {
  srcs <- config$sources %||% list()
  extras <- lapply(srcs, function(s) {
    role <- as.character(s$file_role %||% "")
    if (role %in% c("ops_other", "assumptions", "decisions")) return(s)
    NULL
  })
  extras <- extras[!vapply(extras, is.null, logical(1))]
  grouped <- list()
  seen <- character(0)
  for (s in extras) {
    pid <- as.character(s$pair_id %||% "")
    key <- if (nzchar(pid)) pid else as.character(s$source_id %||% "")
    if (!nzchar(key) || key %in% seen) next
    seen <- c(seen, key)
    members <- extras
    if (nzchar(pid)) {
      members <- Filter(function(x) identical(as.character(x$pair_id %||% ""), pid), extras)
    } else {
      members <- list(s)
    }
    grouped[[length(grouped) + 1]] <- list(
      key = key,
      shape = as.character(s$feed_shape %||% ""),
      members = members
    )
  }
  grouped
}

fpa_read_source_table <- function(config, s, paths = fpa_paths(config)) {
  sid <- as.character(s$source_id %||% "")
  path <- fpa_extra_path_for(config, sid, paths)
  if (is.na(path) || !nzchar(path)) {
    path <- fpa_resolve_path(config, as.character(s$file_role %||% "ops_other"), paths)
  }
  if (is.na(path) || !file.exists(path)) {
    return(list(ok = FALSE, path = path, data = NULL, error = "missing"))
  }
  res <- fpa_read_raw_file(path)
  if (!is.null(res$error)) {
    return(list(ok = FALSE, path = path, data = NULL, error = res$error))
  }
  df <- res$data
  names(df) <- tolower(gsub(" ", "_", names(df)))
  list(ok = TRUE, path = path, data = df, error = NULL)
}

#' Period column of a driver table: periode, jour, date, or day.
#' Accents are folded, so Période matches periode.
fpa_driver_fold_name <- function(nm) {
  raw <- tolower(gsub(" ", "_", as.character(nm)))
  fold <- iconv(raw, to = "ASCII//TRANSLIT")
  if (is.na(fold) || !nzchar(fold)) fold <- raw
  tolower(gsub("[^a-z0-9_]", "", fold))
}

fpa_driver_period_col <- function(df) {
  if (is.null(df) || !ncol(df)) return(NA_character_)
  folded <- vapply(names(df), fpa_driver_fold_name, character(1))
  for (want in c("periode", "jour", "date", "day")) {
    i <- which(folded == want)
    if (length(i)) return(names(df)[[i[[1]]]])
  }
  NA_character_
}

#' Comparable period key. A parsed date wins over the raw text.
fpa_driver_period_key <- function(x) {
  chr <- trimws(as.character(x))
  parsed <- tryCatch(fpa_parse_period(chr), error = function(e) as.Date(rep(NA, length(chr))))
  key <- chr
  ok <- !is.na(parsed)
  key[ok] <- format(parsed[ok], "%Y-%m-%d")
  key
}

#' Month key YYYY-MM. A parsed date wins over the raw text.
fpa_driver_month_key <- function(x) {
  chr <- trimws(as.character(x))
  parsed <- tryCatch(fpa_parse_period(chr), error = function(e) as.Date(rep(NA, length(chr))))
  key <- ifelse(grepl("^[0-9]{4}-[0-9]{2}", chr), substr(chr, 1, 7), chr)
  ok <- !is.na(parsed)
  key[ok] <- format(parsed[ok], "%Y-%m")
  key
}

#' Closed months measure the link. A later month already in the file is kept.
#' An open month that is missing is filled from the same month last year.
#' A missing month with no prior year is left absent. Zero is not a fill.
fpa_living_clock <- function(df, closed_through, open_months = character(0)) {
  closed_through <- as.character(closed_through %||% "")
  if (is.null(df) || !nrow(df) || !nzchar(closed_through)) {
    return(list(ok = FALSE, history = df, forecast = df, filled = character(0)))
  }
  pcol <- fpa_driver_period_col(df)
  if (is.na(pcol) || !nzchar(pcol)) {
    return(list(ok = FALSE, history = df, forecast = df, filled = character(0)))
  }
  month <- fpa_driver_month_key(df[[pcol]])
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, pcol)
  is_closed <- nzchar(month) & month <= closed_through
  history <- df[is_closed, , drop = FALSE]
  later <- df[!is_closed & nzchar(month), , drop = FALSE]
  kept_row <- if (!nrow(later) || !length(num_cols)) {
    rep(FALSE, nrow(later))
  } else {
    vapply(seq_len(nrow(later)), function(i) {
      vals <- suppressWarnings(as.numeric(unlist(later[i, num_cols, drop = FALSE])))
      any(is.finite(vals))
    }, logical(1))
  }
  kept <- later[kept_row, , drop = FALSE]
  have <- unique(c(month[is_closed], if (nrow(kept)) fpa_driver_month_key(kept[[pcol]]) else character(0)))
  missing <- as.character(open_months)
  missing <- sort(unique(missing[nzchar(missing) & missing > closed_through & !(missing %in% have)]))
  hist_month <- if (nrow(history)) fpa_driver_month_key(history[[pcol]]) else character(0)
  filled <- character(0)
  filled_rows <- list()
  # Ordered pass: month 13+ can copy a month already filled one year earlier.
  pool <- history
  pool_month <- hist_month
  for (p in missing) {
    prev <- format(seq(as.Date(paste0(p, "-01")), by = "-1 year", length.out = 2)[[2]], "%Y-%m")
    hit <- which(pool_month == prev)
    if (!length(hit)) next
    row <- pool[hit[[length(hit)]], , drop = FALSE]
    for (nm in setdiff(names(row), c(pcol, num_cols))) row[[nm]] <- NA
    row[[pcol]] <- p
    filled_rows[[length(filled_rows) + 1L]] <- row
    filled <- c(filled, p)
    pool <- rbind(pool, row)
    pool_month <- c(pool_month, p)
  }
  filled_df <- if (length(filled_rows)) do.call(rbind, filled_rows) else history[0, , drop = FALSE]
  forecast <- rbind(history, kept, filled_df)
  if (nrow(forecast)) {
    forecast <- forecast[order(fpa_driver_month_key(forecast[[pcol]])), , drop = FALSE]
    rownames(forecast) <- NULL
    fm <- fpa_driver_month_key(forecast[[pcol]])
    forecast$`.origin` <- ifelse(fm %in% filled, "filled", ifelse(fm <= closed_through, "closed", "kept"))
  }
  if (nrow(history)) {
    rownames(history) <- NULL
    history$`.origin` <- "closed"
  }
  list(ok = TRUE, history = history, forecast = forecast, filled = filled, period = pcol)
}

#' Named month vector. A present open month stays. A missing open month
#' takes the same month last year when that month is already closed.
fpa_living_fill_vector <- function(values, closed_through, open_months = character(0)) {
  closed_through <- as.character(closed_through %||% "")
  if (is.null(values) || !length(values) || !nzchar(closed_through)) return(values)
  out <- values
  for (p in sort(unique(as.character(open_months)))) {
    if (!nzchar(p) || p <= closed_through) next
    if (p %in% names(out) && is.finite(out[[p]])) next
    prev <- format(seq(as.Date(paste0(p, "-01")), by = "-1 year", length.out = 2)[[2]], "%Y-%m")
    if (prev %in% names(out) && is.finite(out[[prev]])) out[[p]] <- out[[prev]]
  }
  out
}

#' Drop rows after the last closed month. Used before a correlation measure.
fpa_driver_closed_rows <- function(df, closed_through) {
  closed_through <- as.character(closed_through %||% "")
  if (is.null(df) || !nrow(df) || !nzchar(closed_through)) return(df)
  pcol <- fpa_driver_period_col(df)
  if (is.na(pcol) || !nzchar(pcol)) return(df)
  month <- fpa_driver_month_key(df[[pcol]])
  df[!nzchar(month) | month <= closed_through, , drop = FALSE]
}

#' Union of columns. A column that exists on only one file is kept.
fpa_driver_align_pair <- function(history, rolling) {
  if (is.null(history)) history <- rolling[0, , drop = FALSE]
  if (is.null(rolling)) rolling <- history[0, , drop = FALSE]
  cols <- union(names(history), names(rolling))
  add_na <- function(df, nm, like) {
    n <- nrow(df)
    if (is.numeric(like)) {
      df[[nm]] <- rep(NA_real_, n)
    } else if (is.logical(like)) {
      df[[nm]] <- rep(NA, n)
    } else {
      df[[nm]] <- rep(NA_character_, n)
    }
    df
  }
  for (nm in setdiff(cols, names(history))) history <- add_na(history, nm, rolling[[nm]])
  for (nm in setdiff(cols, names(rolling))) rolling <- add_na(rolling, nm, history[[nm]])
  for (nm in cols) {
    if (is.numeric(rolling[[nm]]) && !is.numeric(history[[nm]])) {
      history[[nm]] <- suppressWarnings(as.numeric(history[[nm]]))
    } else if (is.numeric(history[[nm]]) && !is.numeric(rolling[[nm]])) {
      rolling[[nm]] <- suppressWarnings(as.numeric(rolling[[nm]]))
    }
  }
  list(
    history = history[, cols, drop = FALSE],
    rolling = rolling[, cols, drop = FALSE]
  )
}

#' Append history then rolling. A period in both files keeps the rolling row.
fpa_join_split_frames <- function(history, rolling) {
  aligned <- fpa_driver_align_pair(history, rolling)
  key <- fpa_driver_period_col(aligned$history)
  if (is.na(key) || !nzchar(key) || !key %in% names(aligned$rolling)) {
    return(list(ok = FALSE, data = NULL, period = NA_character_))
  }
  hk <- fpa_driver_period_key(aligned$history[[key]])
  rk <- fpa_driver_period_key(aligned$rolling[[key]])
  rolling_keys <- rk[nzchar(rk)]
  keep_h <- !(nzchar(hk) & hk %in% rolling_keys)
  out <- rbind(
    aligned$history[keep_h, , drop = FALSE],
    aligned$rolling
  )
  ord <- order(fpa_driver_period_key(out[[key]]), na.last = TRUE)
  out <- out[ord, , drop = FALSE]
  rownames(out) <- NULL
  list(ok = TRUE, data = out, period = key)
}

fpa_driver_split_path <- function(paths, pair_id) {
  safe <- gsub("[^A-Za-z0-9._-]", "_", as.character(pair_id))
  file.path(paths$work, paste0("driver-", safe, ".csv"))
}

#' Read both members of a split pair and write work/driver-<pair_id>.csv.
fpa_driver_join_split <- function(config, group, paths = fpa_paths(config)) {
  hist_s <- NULL
  roll_s <- NULL
  for (s in group$members) {
    role <- as.character(s$pair_role %||% "")
    if (identical(role, "history")) hist_s <- s
    if (identical(role, "rolling")) roll_s <- s
  }
  if (is.null(hist_s) || is.null(roll_s)) {
    return(list(ok = FALSE, reason = "incomplete"))
  }
  hgot <- fpa_read_source_table(config, hist_s, paths)
  rgot <- fpa_read_source_table(config, roll_s, paths)
  if (!isTRUE(hgot$ok) || !isTRUE(rgot$ok)) {
    return(list(ok = FALSE, reason = "missing"))
  }
  joined <- fpa_join_split_frames(hgot$data, rgot$data)
  if (!isTRUE(joined$ok)) {
    return(list(ok = FALSE, reason = "no_period"))
  }
  dir.create(paths$work, recursive = TRUE, showWarnings = FALSE)
  out_path <- fpa_driver_split_path(paths, group$key)
  utils::write.table(
    joined$data, out_path, sep = ",", dec = ".", row.names = FALSE,
    col.names = TRUE, na = "", qmethod = "double", fileEncoding = "UTF-8"
  )
  frame <- joined$data
  frame$`.clock` <- "joined"
  frame$`.source_id` <- as.character(group$key)
  list(
    ok = TRUE,
    reason = "joined",
    data = frame,
    path = out_path,
    period = joined$period
  )
}

#' Read a driver group into correlation vs rolling frames.
#' A split pair is one joined series. Both frames read that file.
fpa_read_driver_group <- function(config, group, paths = fpa_paths(config)) {
  if (identical(as.character(group$shape %||% ""), "split")) {
    joined <- fpa_driver_join_split(config, group, paths)
    if (isTRUE(joined$ok)) {
      return(list(
        key = group$key,
        shape = group$shape,
        correlation = joined$data,
        rolling = joined$data,
        files = joined$path
      ))
    }
  }
  corr <- NULL
  roll <- NULL
  files <- character(0)
  for (s in group$members) {
    got <- fpa_read_source_table(config, s, paths)
    if (!isTRUE(got$ok) || is.null(got$data)) next
    files <- c(files, got$path)
    df <- got$data
    df$`.clock` <- if (fpa_source_is_rolling(s)) "rolling" else "history"
    df$`.source_id` <- as.character(s$source_id %||% "")
    if (fpa_source_uses_correlation(s) || identical(group$shape, "living")) {
      corr <- if (is.null(corr)) df else rbind(corr, df)
    }
    if (fpa_source_is_rolling(s)) {
      roll <- if (is.null(roll)) df else rbind(roll, df)
    }
  }
  list(
    key = group$key,
    shape = group$shape,
    correlation = corr,
    rolling = roll,
    files = files
  )
}

fpa_read_drivers <- function(config, paths = fpa_paths(config)) {
  groups <- fpa_driver_groups(config)
  out <- lapply(groups, function(g) fpa_read_driver_group(config, g, paths))
  list(ok = TRUE, groups = out)
}

#' Apply the living-file rule to a living file and to a joined split pair.
#' history_only is left as read. The subscriber file on disk is not rewritten.
fpa_drivers_apply_clock <- function(drivers, closed_through, open_months = character(0)) {
  if (is.null(drivers) || is.null(drivers$groups)) return(drivers)
  drivers$groups <- lapply(drivers$groups, function(g) {
    if (!identical(g$shape, "living") && !identical(g$shape, "split")) return(g)
    src <- g$rolling
    if (is.null(src) || !nrow(src)) src <- g$correlation
    if (is.null(src) || !nrow(src)) return(g)
    src <- src[, setdiff(names(src), c(".clock", ".source_id", ".origin")), drop = FALSE]
    clocked <- fpa_living_clock(src, closed_through, open_months)
    if (!isTRUE(clocked$ok)) return(g)
    g$correlation <- clocked$history
    g$rolling <- clocked$forecast
    g$filled <- clocked$filled
    g
  })
  drivers
}

fpa_driver_pair_findings <- function(config, paths = NULL) {
  findings <- list()
  groups <- fpa_driver_groups(config)
  for (g in groups) {
    if (!identical(g$shape, "split")) next
    roles <- vapply(g$members, function(s) as.character(s$pair_role %||% ""), character(1))
    if (("history" %in% roles) && ("rolling" %in% roles)) {
      if (is.null(paths)) paths <- fpa_paths(config)
      joined <- fpa_driver_join_split(config, g, paths)
      if (identical(joined$reason, "no_period")) {
        findings[[length(findings) + 1]] <- fpa_finding(
          "driver_pair_no_period", "warning",
          fpa_txt("drv_split_no_period")
        )
      }
      next
    }
    findings[[length(findings) + 1]] <- fpa_finding(
      "driver_pair_incomplete", "warning",
      paste0(
        "Un driver est declare en deux fichiers, mais il manque l'historique ou le suivi. ",
        "La correlation et le rolling ne peuvent pas se completer."
      )
    )
  }
  findings
}
