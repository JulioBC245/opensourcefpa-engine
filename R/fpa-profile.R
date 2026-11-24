# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-profile.R — Discover driver series from declared files. Headers are not a verdict.
# LAST CHANGE: 2026-10-07 — a split pair is profiled from the joined file, once

#' Classify one column from its name and values.
#' Returns period, numeric, dimension, identifier, or comment.
fpa_profile_col_kind <- function(name, values) {
  fold <- fpa_fold_label(name)
  if (grepl("commentaire|explication|\\bnote\\b|comment", fold)) return("comment")
  chr <- trimws(as.character(values))
  chr[chr %in% c("", "NA")] <- NA_character_
  parsed <- fpa_parse_period(chr)
  bare_month <- grepl("^(0?[1-9]|1[0-2])$", chr)
  bare_month[is.na(bare_month)] <- FALSE
  parsed[bare_month] <- as.Date(NA)
  known <- mean(!is.na(parsed))
  distinct <- length(unique(parsed[!is.na(parsed)]))
  if (is.finite(known) && known >= 0.6 && distinct >= 2L) return("period")
  num <- suppressWarnings(as.numeric(gsub(",", ".", chr, fixed = TRUE)))
  num_share <- mean(is.finite(num))
  if (is.finite(num_share) && num_share >= 0.7) return("numeric")
  present <- chr[!is.na(chr)]
  u <- unique(present)
  if (length(present) && length(u) <= 12L && length(u) < length(present) / 2) {
    return("dimension")
  }
  "identifier"
}

#' Observed spacing of parsed dates: day, week, month, year, or irregular.
fpa_profile_grain <- function(dates) {
  d <- sort(unique(as.Date(dates)))
  d <- d[!is.na(d)]
  if (length(d) < 3L) return("unknown")
  med <- median(as.numeric(diff(d)))
  if (med <= 2) return("day")
  if (med <= 10) return("week")
  if (med <= 40) return("month")
  if (med >= 300) return("year")
  "irregular"
}

#' Layout: long (one period column) or wide_periods (periods in the headers).
fpa_profile_layout <- function(df) {
  if (!ncol(df)) return("empty")
  kinds <- vapply(names(df), function(nm) {
    fpa_profile_col_kind(nm, df[[nm]])
  }, character(1))
  if (any(kinds == "period")) return("long")
  header_dates <- fpa_parse_period(names(df))
  if (sum(!is.na(header_dates)) >= 3L) return("wide_periods")
  "unknown"
}

fpa_profile_series_id <- function(source_id, name) {
  raw <- paste0(as.character(source_id), "__", as.character(name))
  gsub("[^A-Za-z0-9_-]", "_", raw)
}

#' Propose the first affected P&L line. A hypothesis, not a grade.
fpa_profile_propose_target <- function(name) {
  f <- fpa_fold_label(name)
  if (grepl("temp|meteo|weather|precip|pluie|climat", f)) {
    return(list(
      line = "REV",
      reason = "La meteo touche d'abord la demande."
    ))
  }
  if (grepl("nuitee|couvert|booking|reservation|occupation|volume|tarif|chiffre", f)) {
    return(list(
      line = "REV",
      reason = "Serie d'activite commerciale. Premiere ligne touchee proposee: chiffre d'affaires."
    ))
  }
  if (grepl("effectif|salaire|payroll|heure sup|main d oeuvre", f)) {
    return(list(
      line = "PAYROLL",
      reason = "Serie de main d'oeuvre. Premiere ligne touchee proposee: masse salariale."
    ))
  }
  if (grepl("matiere|scrap|rebut|kwh|energie|commodity|achat", f)) {
    return(list(
      line = "COGS",
      reason = "Serie de cout. Premiere ligne touchee proposee: achats."
    ))
  }
  if (grepl("charge externe|loyer|opex|fourniture", f)) {
    return(list(
      line = "OPEX_OTHER",
      reason = "Serie de charge d'exploitation."
    ))
  }
  list(
    line = "REV",
    reason = fpa_txt("loc_no_finer_line")
  )
}

#' Propose an aggregation. Statistics must not choose this later.
fpa_profile_propose_aggregation <- function(name) {
  f <- fpa_fold_label(name)
  if (grepl("temp|taux|prix|tarif|occupation|pourcent", f)) return("mean")
  if (grepl("stock|encours|carnet|pipeline", f)) return("closing")
  if (grepl("seuil|count above|nb jour", f)) return("count_above")
  if (grepl("jour ouvre|working day", f)) return("working_day")
  "sum"
}

fpa_profile_propose_unit <- function(name) {
  f <- fpa_fold_label(name)
  if (grepl("\\bc\\b|celsius|temperature", f)) return("C")
  if (grepl("mm|precip|pluie", f)) return("mm")
  if (grepl("eur|euro|tarif|prix|cout", f)) return("EUR")
  if (grepl("taux|pourcent|pct|occupation", f)) return("ratio")
  if (grepl("nuitee|couvert|booking", f)) return("count")
  if (grepl("kwh|energie", f)) return("kWh")
  ""
}

#' Robust outlier flags. Flag only: values stay in the series.
fpa_profile_outliers <- function(x) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  flag <- rep(FALSE, length(x))
  if (sum(ok) < 5L) return(flag)
  med <- median(x[ok])
  m <- mad(x[ok])
  if (!is.finite(m) || m < 1e-12) return(flag)
  flag[ok] <- abs(x[ok] - med) > 3 * m
  flag
}

fpa_profile_acf1 <- function(x) {
  x <- as.numeric(x)
  x <- x[is.finite(x)]
  if (length(x) < 4L) return(NA_real_)
  if (sd(x) < 1e-12) return(NA_real_)
  suppressWarnings(cor(x[-length(x)], x[-1]))
}

fpa_profile_season_strength <- function(x, dates) {
  x <- as.numeric(x)
  dates <- as.Date(dates)
  ok <- is.finite(x) & !is.na(dates)
  if (sum(ok) < 12L) return(NA_real_)
  m <- as.integer(format(dates[ok], "%m"))
  means <- tapply(x[ok], m, mean)
  if (length(means) < 6L) return(NA_real_)
  v_between <- var(as.numeric(means))
  v_total <- var(x[ok])
  if (!is.finite(v_total) || v_total < 1e-12) return(NA_real_)
  max(0, min(1, v_between / v_total))
}

fpa_profile_trend <- function(x) {
  x <- as.numeric(x)
  ok <- is.finite(x)
  if (sum(ok) < 4L || sd(x[ok]) < 1e-12) return(NA_real_)
  fit <- stats::lm(x[ok] ~ seq_along(x)[ok])
  unname(coef(fit)[[2]])
}

#' Diagnostics for one numeric vector already aligned to dates.
fpa_profile_measure <- function(values, dates) {
  x <- as.numeric(values)
  n <- sum(is.finite(x))
  flags <- fpa_profile_outliers(x)
  zero_share <- if (n) mean(x[is.finite(x)] == 0) else NA_real_
  na_share <- mean(!is.finite(x))
  constant <- n >= 2L && is.finite(sd(x[is.finite(x)])) && sd(x[is.finite(x)]) < 1e-8
  list(
    n = n,
    na_share = na_share,
    zero_share = zero_share,
    constant = isTRUE(constant),
    outlier_n = sum(flags),
    acf1 = fpa_profile_acf1(x),
    season_strength = fpa_profile_season_strength(x, dates),
    trend = fpa_profile_trend(x)
  )
}

fpa_profile_block_reason <- function(measure) {
  if (measure$n < 8L) return("trop peu de points (moins de 8)")
  if (isTRUE(measure$constant)) return("serie constante")
  ""
}

#' Mark a series that copies another, or that is exactly the sum of two others.
fpa_profile_identity <- function(measures) {
  ids <- names(measures)
  status <- setNames(rep("independent", length(ids)), ids)
  if (length(ids) < 2L) return(status)
  mats <- lapply(measures, function(s) as.numeric(s$values))
  n <- length(mats[[1]])
  for (i in seq_along(ids)) {
    for (j in seq_along(ids)) {
      if (j <= i) next
      a <- mats[[i]]
      b <- mats[[j]]
      ok <- is.finite(a) & is.finite(b)
      if (sum(ok) < 8L) next
      if (sd(a[ok]) < 1e-12 || sd(b[ok]) < 1e-12) next
      fit <- stats::lm(b[ok] ~ a[ok])
      scale <- max(abs(b[ok]))
      if (!is.finite(scale) || scale < 1e-8) next
      if (max(abs(stats::residuals(fit))) <= 1e-4 * scale) {
        status[[ids[[j]]]] <- "suspect_identity"
      }
    }
  }
  if (length(ids) >= 3L) {
    for (i in seq_along(ids)) {
      others <- ids[-i]
      for (a in seq_along(others)) {
        for (b in seq_along(others)) {
          if (b <= a) next
          s <- mats[[others[[a]]]] + mats[[others[[b]]]]
          v <- mats[[i]]
          ok <- is.finite(s) & is.finite(v)
          if (sum(ok) < 8L) next
          scale <- max(abs(v[ok]), na.rm = TRUE)
          if (!is.finite(scale) || scale < 1e-8) next
          if (max(abs(v[ok] - s[ok])) <= 1e-6 * scale) {
            status[[ids[[i]]]] <- "part_whole"
          }
        }
      }
    }
  }
  status
}

fpa_profile_empty <- function() {
  data.frame(
    series_id = character(),
    source_id = character(),
    source_name = character(),
    series_name = character(),
    kind = character(),
    layout = character(),
    declared_grain = character(),
    observed_grain = character(),
    grain_note = character(),
    unit = character(),
    aggregation_rule = character(),
    target_line = character(),
    target_reason = character(),
    target_status = character(),
    n = integer(),
    na_share = numeric(),
    zero_share = numeric(),
    constant = logical(),
    outlier_n = integer(),
    acf1 = numeric(),
    season_strength = numeric(),
    trend = numeric(),
    identity = character(),
    block_reason = character(),
    availability_lag_days = integer(),
    stringsAsFactors = FALSE
  )
}

fpa_profile_candidate_row <- function(...) {
  base <- fpa_profile_empty()
  row <- list(...)
  for (nm in names(base)) {
    if (is.null(row[[nm]])) row[[nm]] <- base[[nm]]
  }
  as.data.frame(row[names(base)], stringsAsFactors = FALSE)
}

#' Turn a raw table into candidate rows plus named series (dates, values).
fpa_profile_table <- function(df, source_id, source_name = "",
                              declared_grain = "", availability_lag_days = NA_integer_) {
  empty <- list(candidates = fpa_profile_empty(), series = list())
  if (is.null(df) || !ncol(df)) return(empty)
  layout <- fpa_profile_layout(df)
  kinds <- vapply(names(df), function(nm) fpa_profile_col_kind(nm, df[[nm]]), character(1))
  pulled <- list()

  if (identical(layout, "long")) {
    pcol <- names(df)[which(kinds == "period")[[1]]]
    dates <- fpa_parse_period(df[[pcol]])
    num_cols <- names(df)[kinds == "numeric"]
    for (nm in num_cols) {
      vals <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[nm]]), fixed = TRUE)))
      sid <- fpa_profile_series_id(source_id, nm)
      pulled[[sid]] <- list(dates = dates, values = vals, name = nm)
    }
  } else if (identical(layout, "wide_periods")) {
    header_dates <- fpa_parse_period(names(df))
    pcols <- which(!is.na(header_dates))
    label_col <- which(kinds != "numeric")
    label_col <- if (length(label_col)) label_col[[1]] else NA_integer_
    if (is.na(label_col)) {
      sid <- fpa_profile_series_id(source_id, "montant")
      pulled[[sid]] <- list(
        dates = header_dates[pcols],
        values = suppressWarnings(as.numeric(df[1, pcols])),
        name = "montant"
      )
    } else {
      for (i in seq_len(nrow(df))) {
        lab <- trimws(as.character(df[[label_col]][[i]]))
        if (!nzchar(lab)) next
        sid <- fpa_profile_series_id(source_id, lab)
        pulled[[sid]] <- list(
          dates = header_dates[pcols],
          values = suppressWarnings(as.numeric(unlist(df[i, pcols]))),
          name = lab
        )
      }
    }
  } else {
    return(empty)
  }

  if (!length(pulled)) return(empty)
  ident <- fpa_profile_identity(pulled)
  observed <- fpa_profile_grain(pulled[[1]]$dates)
  grain_note <- ""
  if (nzchar(declared_grain) && !identical(observed, "unknown") &&
      !identical(tolower(declared_grain), observed)) {
    grain_note <- paste0(
      "Grain declare ", declared_grain, ", grain observe ", observed, "."
    )
  }

  rows <- lapply(names(pulled), function(sid) {
    s <- pulled[[sid]]
    meas <- fpa_profile_measure(s$values, s$dates)
    prop <- fpa_profile_propose_target(s$name)
    fpa_profile_candidate_row(
      series_id = sid,
      source_id = as.character(source_id),
      source_name = as.character(source_name),
      series_name = as.character(s$name),
      kind = "numeric",
      layout = layout,
      declared_grain = as.character(declared_grain),
      observed_grain = observed,
      grain_note = grain_note,
      unit = fpa_profile_propose_unit(s$name),
      aggregation_rule = fpa_profile_propose_aggregation(s$name),
      target_line = prop$line,
      target_reason = prop$reason,
      target_status = "suggested",
      n = as.integer(meas$n),
      na_share = meas$na_share,
      zero_share = meas$zero_share,
      constant = isTRUE(meas$constant),
      outlier_n = as.integer(meas$outlier_n),
      acf1 = meas$acf1,
      season_strength = meas$season_strength,
      trend = meas$trend,
      identity = ident[[sid]] %||% "independent",
      block_reason = fpa_profile_block_reason(meas),
      availability_lag_days = as.integer(availability_lag_days %||% NA_integer_)
    )
  })
  list(
    candidates = do.call(rbind, rows),
    series = pulled
  )
}

FPA_SUM_KINDS <- c("volume", "capacity", "customers", "headcount", "hours", "area", "flow")

#' Kind of one column for the segment rule: the wizard's declaration, else a name guess.
fpa_segment_kind <- function(config, column) {
  for (b in config$driver_bindings %||% list()) {
    col <- as.character(b$column %||% "")
    if (!nzchar(col)) col <- sub("^.*__", "", as.character(b$series_id %||% ""))
    if (identical(col, as.character(column))) {
      k <- as.character(b$kind %||% "")
      if (nzchar(k)) return(k)
    }
  }
  if (identical(fpa_profile_propose_aggregation(column), "mean")) "rate" else "volume"
}

#' Rows sharing a month (one per site, store, product) become one row per month.
#' Volumes and other counts are summed; rates are averaged weighted by the declared
#' volume (capacity for a utilisation). Without a weight the rate is a plain mean,
#' and the note says so. A file with one row per month is returned unchanged.
fpa_segment_collapse <- function(df, config = NULL) {
  if (is.null(df) || !nrow(df) || !ncol(df)) return(df)
  kinds <- vapply(names(df), function(nm) fpa_profile_col_kind(nm, df[[nm]]), character(1))
  pcol <- names(df)[kinds == "period"]
  if (!length(pcol)) return(df)
  pcol <- pcol[[1]]
  key <- format(fpa_parse_period(as.character(df[[pcol]])), "%Y-%m")
  ok <- !is.na(key)
  if (!anyDuplicated(key[ok])) return(df)
  num_cols <- setdiff(names(df)[kinds == "numeric"], pcol)
  seg_cols <- setdiff(names(df)[kinds %in% c("dimension", "identifier")], pcol)
  num <- lapply(num_cols, function(nm) suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[nm]]), fixed = TRUE))))
  names(num) <- num_cols
  ckind <- vapply(num_cols, function(nm) fpa_segment_kind(config, nm), character(1))
  pick_first <- function(want) {
    hit <- num_cols[ckind %in% want]
    if (length(hit)) hit[[1]] else NA_character_
  }
  vol_w <- pick_first(c("volume", "customers"))
  cap_w <- pick_first("capacity")
  keys <- sort(unique(key[ok]))
  out <- data.frame(periode = keys, stringsAsFactors = FALSE)
  names(out)[[1]] <- pcol
  plain <- character()
  for (nm in num_cols) {
    x <- num[[nm]]
    w_name <- if (ckind[[nm]] == "utilisation" && !is.na(cap_w)) cap_w else vol_w
    summed <- ckind[[nm]] %in% FPA_SUM_KINDS
    if (!summed && (is.na(w_name) || identical(w_name, nm))) plain <- c(plain, nm)
    out[[nm]] <- vapply(keys, function(k) {
      i <- which(ok & key == k & is.finite(x))
      if (!length(i)) return(NA_real_)
      if (summed) return(sum(x[i]))
      if (!is.na(w_name) && !identical(w_name, nm)) {
        w <- num[[w_name]][i]
        good <- is.finite(w) & w > 0
        if (any(good)) return(sum(x[i][good] * w[good]) / sum(w[good]))
      }
      mean(x[i])
    }, numeric(1))
  }
  per_month <- max(table(key[ok]))
  attr(out, "segments") <- list(
    rows_per_month = as.integer(per_month),
    by = seg_cols,
    weight = if (is.na(vol_w)) "" else vol_w,
    plain_mean = plain
  )
  out
}

#' One sentence for the screen when a file was collapsed.
fpa_segment_note <- function(seg) {
  if (is.null(seg) || !length(seg$rows_per_month) || seg$rows_per_month <= 1L) return("")
  by <- if (length(seg$by)) paste(seg$by, collapse = ", ") else "segment"
  txt <- fpa_txt("seg_note_head", seg$rows_per_month, by)
  if (nzchar(seg$weight %||% "")) txt <- paste(txt, fpa_txt("seg_note_weight", seg$weight))
  if (length(seg$plain_mean)) {
    txt <- paste(txt, fpa_txt("seg_note_plain", paste(seg$plain_mean, collapse = ", ")))
  }
  txt
}

#' Sources that can be drivers: operational extras, not the statements themselves.
fpa_profile_driver_sources <- function(config) {
  out <- list()
  for (s in config$sources %||% list()) {
    role <- as.character(s$file_role %||% "")
    if (!identical(role, "ops_other")) next
    out[[length(out) + 1]] <- s
  }
  out
}

#' Profile every declared driver file. P&L, budget, and opening BS are not drivers.
fpa_profile_drivers <- function(config, paths = fpa_paths(config), closed_through = "") {
  sources <- fpa_profile_driver_sources(config)
  groups <- fpa_driver_groups(config)
  all_rows <- list()
  all_series <- list()
  segments <- list()
  seen_pair <- character(0)
  for (s in sources) {
    fs <- as.character(s$feed_shape %||% "")
    pid <- as.character(s$pair_id %||% "")
    if (identical(fs, "split") && nzchar(pid)) {
      if (pid %in% seen_pair) next
      g <- NULL
      for (gg in groups) if (identical(gg$key, pid)) g <- gg
      joined <- if (is.null(g)) list(ok = FALSE) else fpa_driver_join_split(config, g, paths)
      if (isTRUE(joined$ok)) {
        seen_pair <- c(seen_pair, pid)
        owner <- s
        for (m in g$members) {
          if (identical(as.character(m$pair_role %||% ""), "rolling")) owner <- m
        }
        sid <- as.character(owner$source_id %||% pid)
        raw <- fpa_read_raw_file(joined$path)
        df <- if (!is.null(raw$data)) raw$data else joined$data
        df <- fpa_driver_closed_rows(df, closed_through)
        df <- fpa_segment_collapse(df, config)
        if (!is.null(attr(df, "segments"))) segments[[sid]] <- attr(df, "segments")
        prof <- fpa_profile_table(
          df,
          source_id = sid,
          source_name = as.character(owner$source_name %||% sid),
          declared_grain = as.character(owner$grain %||% ""),
          availability_lag_days = owner$availability_lag_days %||% NA_integer_
        )
        if (nrow(prof$candidates)) all_rows[[length(all_rows) + 1]] <- prof$candidates
        all_series <- c(all_series, prof$series)
        next
      }
    }
    sid <- as.character(s$source_id %||% "")
    got <- fpa_read_source_table(config, s, paths)
    if (!isTRUE(got$ok) || is.null(got$data)) next
    # fpa_read_source_table lowercases names. Re-read raw to keep headers.
    raw <- fpa_read_raw_file(got$path)
    df <- if (!is.null(raw$data)) raw$data else got$data
    df <- fpa_driver_closed_rows(df, closed_through)
    df <- fpa_segment_collapse(df, config)
    if (!is.null(attr(df, "segments"))) segments[[sid]] <- attr(df, "segments")
    prof <- fpa_profile_table(
      df,
      source_id = sid,
      source_name = as.character(s$source_name %||% sid),
      declared_grain = as.character(s$grain %||% ""),
      availability_lag_days = s$availability_lag_days %||% NA_integer_
    )
    if (nrow(prof$candidates)) all_rows[[length(all_rows) + 1]] <- prof$candidates
    all_series <- c(all_series, prof$series)
  }
  candidates <- if (length(all_rows)) do.call(rbind, all_rows) else fpa_profile_empty()
  rownames(candidates) <- NULL
  list(candidates = candidates, series = all_series, segments = segments)
}
