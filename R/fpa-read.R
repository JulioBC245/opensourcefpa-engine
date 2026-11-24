# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-read.R — Read declared files, apply column_map, pivot wide on period_end
# LAST CHANGE: 2026-09-30 — raw popup reads a CSV with its own separator
# Grain honesty: never interpolate annual into monthly.

#' CSV vs Excel from the filename, then from the first bytes.
#' The wizard no longer asks for a format: .xlsx/.xls/.xlsm -> Excel, else CSV.
#' If the extension is wrong, PKZIP or OLE magic still picks Excel.
fpa_sniff_kind <- function(path) {
  ext <- tolower(tools::file_ext(path))
  bytes <- "unknown"
  if (file.exists(path)) {
    raw <- tryCatch({
      con <- file(path, "rb")
      on.exit(close(con), add = TRUE)
      readBin(con, what = "raw", n = 8L)
    }, error = function(e) raw(0))
    if (length(raw) >= 2L && identical(raw[1:2], as.raw(c(0x50, 0x4b)))) {
      bytes <- "excel"
    } else if (length(raw) >= 4L && identical(raw[1:4], as.raw(c(0xd0, 0xcf, 0x11, 0xe0)))) {
      bytes <- "excel"
    } else if (length(raw) > 0L) {
      bytes <- "csv"
    }
  }
  if (ext %in% c("xlsx", "xls", "xlsm") || identical(bytes, "excel")) return("excel")
  "csv"
}

fpa_try_excel <- function(path) {
  if (!requireNamespace("readxl", quietly = TRUE)) {
    stop("readxl manquant")
  }
  as.data.frame(readxl::read_excel(path), stringsAsFactors = FALSE)
}

fpa_try_csv <- function(path) {
  tryCatch(
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) {
      utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "latin1")
    }
  )
}

#' CSV read that keeps the file's own separator and encoding.
#' The separator is sniffed from the header line (`;` when it outnumbers `,`).
#' Both are returned as attributes so a writer can save the file the way it came.
fpa_csv_read_sniff <- function(path, nrows = -1L) {
  if (!nzchar(path %||% "") || !file.exists(path)) return(NULL)
  raw <- tryCatch(readBin(path, what = "raw", n = min(file.info(path)$size, 262144)), error = function(e) raw(0))
  if (!length(raw)) return(NULL)
  raw <- raw[raw != as.raw(0)]
  enc <- if (validUTF8(rawToChar(raw))) "UTF-8" else "latin1"
  nl <- which(raw == as.raw(10L))
  head_raw <- if (length(nl)) raw[seq_len(nl[[1]] - 1L)] else raw
  n_semi <- sum(head_raw == as.raw(59L))
  n_comma <- sum(head_raw == as.raw(44L))
  sep <- if (n_semi > n_comma) ";" else ","
  df <- tryCatch(
    utils::read.csv(
      path, sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
      fileEncoding = if (identical(enc, "UTF-8")) "UTF-8-BOM" else "latin1", nrows = nrows
    ),
    error = function(e) NULL
  )
  if (is.null(df)) return(NULL)
  attr(df, "fpa_sep") <- sep
  attr(df, "fpa_encoding") <- enc
  df
}

#' Write a data.frame back with the separator and encoding it was read with.
fpa_csv_write_same <- function(df, path, sep = attr(df, "fpa_sep") %||% ",",
                               encoding = attr(df, "fpa_encoding") %||% "UTF-8") {
  needs <- which(vapply(df, function(col) {
    is.character(col) && any(grepl(paste0("[", sep, "\"\n]"), col[!is.na(col)]))
  }, logical(1)))
  utils::write.table(
    df, path, sep = sep, dec = ".", row.names = FALSE, col.names = TRUE,
    quote = if (length(needs)) unname(needs) else FALSE, na = "", qmethod = "double",
    fileEncoding = if (identical(encoding, "latin1")) "latin1" else "UTF-8"
  )
  invisible(TRUE)
}

#' Read a CSV or Excel file into a data.frame.
fpa_read_raw_file <- function(path) {
  if (!file.exists(path)) {
    return(structure(list(error = "missing", path = path, data = NULL), class = "fpa_read_result"))
  }
  kind <- fpa_sniff_kind(path)
  first <- if (identical(kind, "excel")) fpa_try_excel else fpa_try_csv
  second <- if (identical(kind, "excel")) fpa_try_csv else fpa_try_excel
  df <- tryCatch(
    first(path),
    error = function(e1) {
      tryCatch(
        second(path),
        error = function(e2) {
          structure(
            list(
              error = "read_fail",
              path = path,
              message = conditionMessage(e1),
              data = NULL
            ),
            class = "fpa_read_result"
          )
        }
      )
    }
  )
  if (inherits(df, "fpa_read_result")) return(df)
  structure(list(error = NULL, path = path, data = df), class = "fpa_read_result")
}

#' Frame shown in the raw-file popup. A CSV keeps its own separator.
#' fpa_try_csv stays comma-only; this preview does not use it for CSV.
fpa_raw_preview_frame <- function(path) {
  if (identical(tolower(tools::file_ext(path)), "csv")) {
    df <- fpa_csv_read_sniff(path)
    if (is.null(df)) {
      return(structure(
        list(error = "read_fail", path = path, message = "", data = NULL),
        class = "fpa_read_result"
      ))
    }
    return(structure(list(error = NULL, path = path, data = df), class = "fpa_read_result"))
  }
  fpa_read_raw_file(path)
}

#' Write edited cells back into a CSV, keeping its separator and encoding.
#' `values` names are "row_col" (1-based). A missing name is left untouched.
#' An empty cell in a numeric column is written blank, not as the word NA.
fpa_raw_edit_apply <- function(path, values) {
  df <- fpa_csv_read_sniff(path)
  if (is.null(df) || !length(values)) return(list(ok = !is.null(df), n = 0L))
  n <- 0L
  keys <- names(values)
  for (key in keys) {
    parts <- strsplit(key, "_", fixed = TRUE)[[1]]
    if (length(parts) != 2L) next
    i <- suppressWarnings(as.integer(parts[[1]]))
    j <- suppressWarnings(as.integer(parts[[2]]))
    if (!is.finite(i) || !is.finite(j) || i < 1L || j < 1L || i > nrow(df) || j > ncol(df)) next
    raw <- trimws(as.character(values[[key]] %||% ""))
    if (is.numeric(df[[j]])) {
      raw <- gsub(" ", "", raw, fixed = TRUE)
      if (!nzchar(raw)) {
        new <- NA_real_
      } else {
        num <- suppressWarnings(as.numeric(gsub(",", ".", raw, fixed = TRUE)))
        if (!is.finite(num)) {
          df[[j]] <- as.character(df[[j]])
          df[[j]][is.na(df[[j]])] <- ""
          new <- raw
        } else {
          new <- num
        }
      }
    } else if (!nzchar(raw)) {
      new <- ""
    } else {
      new <- raw
    }
    old <- df[[j]][[i]]
    same <- if (is.numeric(new) && is.numeric(old)) {
      (is.na(old) && is.na(new)) || (is.finite(old) && is.finite(new) && abs(old - new) < 1e-8)
    } else {
      old_txt <- if (length(old) != 1L || is.na(old)) "" else as.character(old)
      identical(old_txt, as.character(new))
    }
    if (same) next
    df[[j]][[i]] <- new
    n <- n + 1L
  }
  if (n > 0L) fpa_csv_write_same(df, path)
  list(ok = TRUE, n = n)
}

fpa_month_from_name <- function(folded) {
  keys <- c(
    fpa_txt("month_9"), "september", "decembre", "december", fpa_txt("month_11"), "november",
    fpa_txt("month_10"), "october", fpa_txt("month_1"), "january", "fevrier", "february",
    fpa_txt("month_7"), "august", fpa_txt("month_4"), "april", "march", fpa_txt("month_6"), "june", "july",
    fpa_txt("month_3"), "aout", "juil", "janv", "fevr", "sept", "oct", "nov", "dec",
    "apr", "avr", "aug", "fev", "feb", "jan", "mar", "may", fpa_txt("month_5"), "jun",
    "jul", "sep"
  )
  vals <- c(
    9, 9, 12, 12, 11, 11,
    10, 10, 1, 1, 2, 2,
    7, 8, 4, 4, 3, 6, 6, 7,
    3, 8, 7, 1, 2, 9, 10, 11, 12,
    4, 4, 8, 2, 2, 1, 3, 5, 5, 6,
    7, 9
  )
  tokens <- unlist(strsplit(folded, "\\s+"))
  for (i in seq_along(keys)) {
    if (keys[[i]] %in% tokens) return(vals[[i]])
  }
  NA_integer_
}

fpa_month_end <- function(year, month) {
  n <- max(length(year), length(month))
  if (!n) return(as.Date(character(0)))
  year <- rep_len(as.integer(year), n)
  month <- rep_len(as.integer(month), n)
  out <- as.Date(rep(NA, n))
  for (i in seq_len(n)) {
    if (is.na(year[[i]]) || is.na(month[[i]])) next
    d0 <- as.Date(sprintf("%04d-%02d-01", year[[i]], month[[i]]))
    out[[i]] <- seq(d0, by = "month", length.out = 2)[[2]] - 1
  }
  out
}

fpa_expand_year <- function(yy) {
  yy <- as.integer(yy)
  if (is.na(yy)) return(NA_integer_)
  if (yy >= 100L) return(yy)
  if (yy >= 90L) 1900L + yy else 2000L + yy
}

#' Parse a period label: YYYY-MM, YYYYMM, YYMM, 01, Janvier, janv-24, 01/2024.
fpa_parse_period_one <- function(s, year_hint = NA_integer_) {
  s <- trimws(as.character(s))
  if (!nzchar(s)) return(as.Date(NA))
  if (grepl("^\\d{4}-\\d{2}-\\d{2}$", s)) return(as.Date(s))
  if (grepl("^\\d{4}-\\d{2}$", s)) {
    d0 <- as.Date(paste0(s, "-01"))
    return(seq(d0, by = "month", length.out = 2)[[2]] - 1)
  }
  if (grepl("^20\\d{2}$", s)) return(as.Date(paste0(s, "-12-31")))
  if (grepl("^20\\d{2}(0[1-9]|1[0-2])$", s)) {
    year <- as.integer(substr(s, 1, 4))
    month <- as.integer(substr(s, 5, 6))
    return(fpa_month_end(year, month))
  }
  if (grepl("^\\d{2}(0[1-9]|1[0-2])$", s) && nchar(s) == 4L) {
    yy <- as.integer(substr(s, 1, 2))
    month <- as.integer(substr(s, 3, 4))
    if (!is.na(yy) && yy > 12L) return(fpa_month_end(fpa_expand_year(yy), month))
  }
  folded <- fpa_fold_header(s)
  compact <- gsub("\\s+", "", folded)
  m <- regmatches(folded, regexpr("20[0-9]{2}", folded, perl = TRUE))
  year_from_text <- if (length(m) && nzchar(m)) as.integer(m) else NA_integer_
  month <- fpa_month_from_name(folded)
  if (!is.na(month)) {
    year <- year_from_text
    if (is.na(year)) {
      trail <- sub("^.*?([0-9]{2})$", "\\1", compact)
      if (grepl("^[0-9]{2}$", trail) && nchar(compact) > 2L) {
        tval <- as.integer(trail)
        if (!is.na(tval) && tval > 12L) year <- fpa_expand_year(tval)
      }
    }
    if (is.na(year)) year <- year_hint
    if (is.na(year)) return(as.Date(NA))
    return(fpa_month_end(year, month))
  }
  if (grepl("^(0?[1-9]|1[0-2])$", compact)) {
    if (is.na(year_hint)) return(as.Date(NA))
    return(fpa_month_end(year_hint, as.integer(compact)))
  }
  if (grepl("^(20[0-9]{2})[-/. ](0?[1-9]|1[0-2])", folded)) {
    parts <- regmatches(folded, regexec("(20[0-9]{2})[-/. ](0?[1-9]|1[0-2])", folded))[[1]]
    return(fpa_month_end(as.integer(parts[[2]]), as.integer(parts[[3]])))
  }
  if (grepl("^(0?[1-9]|1[0-2])[-/. ](20[0-9]{2})", folded)) {
    parts <- regmatches(folded, regexec("(0?[1-9]|1[0-2])[-/. ](20[0-9]{2})", folded))[[1]]
    return(fpa_month_end(as.integer(parts[[3]]), as.integer(parts[[2]])))
  }
  as.Date(NA)
}

#' Parse YYYY-MM, YYYY-MM-DD, month names, YYMM, or MM to Date (month-end).
fpa_parse_period <- function(x) {
  x <- as.character(x)
  out <- as.Date(rep(NA, length(x)))
  years <- integer(0)
  for (i in seq_along(x)) {
    out[[i]] <- fpa_parse_period_one(x[[i]])
    if (!is.na(out[[i]])) years <- c(years, as.integer(format(out[[i]], "%Y")))
  }
  hint <- if (length(years)) max(years) else as.integer(format(Sys.Date(), "%Y"))
  for (i in seq_along(x)) {
    if (!is.na(out[[i]])) next
    out[[i]] <- fpa_parse_period_one(x[[i]], year_hint = hint)
  }
  out
}

#' Month start Date from period_end Date.
fpa_month_key <- function(period_end) {
  format(as.Date(period_end), "%Y-%m")
}

#' Apply column_map for a long layout: rename known roles to engine names.
fpa_apply_long_map <- function(df, column_map) {
  if (is.null(column_map) || !length(column_map)) return(df)
  ren <- character(0)
  for (hdr in names(column_map)) {
    role <- as.character(column_map[[hdr]])
    if (role %in% c("IGNORE", "commentaire", "")) next
    if (hdr %in% names(df)) {
      ren[[hdr]] <- role
    }
  }
  # Avoid duplicate target names by keeping first
  for (old in names(ren)) {
    new <- ren[[old]]
    if (old == new) next
    if (new %in% names(df) && new != old) {
      # keep engine name already present
      next
    }
    names(df)[names(df) == old] <- new
  }
  df
}

#' Pivot wide amount columns to long using compiled periods[].
#' Joins on period_end, never on user label N alone.
fpa_pivot_wide <- function(df, source) {
  cmap <- source$column_map %||% list()
  periods <- source$periods %||% list()
  # Find libelle column
  label_hdr <- NULL
  for (hdr in names(cmap)) {
    if (identical(cmap[[hdr]], "libelle") && hdr %in% names(df)) {
      label_hdr <- hdr
      break
    }
  }
  if (is.null(label_hdr)) {
    # Heuristic
    for (c in names(df)) {
      if (grepl("libelle|poste|label|ligne", c, ignore.case = TRUE)) {
        label_hdr <- c
        break
      }
    }
  }
  if (is.null(label_hdr)) {
    stop(fpa_txt("loc_label_col_missing"))
  }
  rows <- list()
  if (length(periods)) {
    for (p in periods) {
      hdr <- as.character(p$header %||% "")
      if (!nzchar(hdr) || !(hdr %in% names(df))) next
      pe <- p$period_end
      if (is.null(pe) || !nzchar(as.character(pe))) {
        # Cannot use without period_end
        next
      }
      grain <- as.character(p$grain %||% "year")
      for (i in seq_len(nrow(df))) {
        amt <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[hdr]][[i]]))))
        rows[[length(rows) + 1]] <- data.frame(
          libelle = as.character(df[[label_hdr]][[i]]),
          period_end = as.character(pe),
          grain = grain,
          montant = amt,
          user_label = as.character(p$user_label %||% ""),
          stringsAsFactors = FALSE
        )
      }
    }
  } else {
    # Fall back: columns mapped to periode_montant
    for (hdr in names(cmap)) {
      if (!identical(cmap[[hdr]], "periode_montant")) next
      if (!(hdr %in% names(df))) next
      pe_guess <- NA_character_
      parsed_hdr <- fpa_parse_period_one(hdr)
      if (!is.na(parsed_hdr)) {
        pe_guess <- as.character(parsed_hdr)
      } else {
        m <- regmatches(hdr, regexpr("20[0-9]{2}", hdr))
        if (length(m) && nzchar(m)) pe_guess <- paste0(m, "-12-31")
      }
      grain_guess <- if (grepl("^20[0-9]{2}$", gsub("[^0-9]", "", hdr))) "year" else "month"
      for (i in seq_len(nrow(df))) {
        amt <- suppressWarnings(as.numeric(gsub(",", ".", as.character(df[[hdr]][[i]]))))
        rows[[length(rows) + 1]] <- data.frame(
          libelle = as.character(df[[label_hdr]][[i]]),
          period_end = pe_guess,
          grain = grain_guess,
          montant = amt,
          user_label = hdr,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(
      libelle = character(), period_end = character(), grain = character(),
      montant = numeric(), user_label = character(), stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}

#' Map libelle to functional_label and aggregate.
fpa_map_amounts <- function(long_df, line_map) {
  if (!nrow(long_df)) {
    return(data.frame(
      functional_label = character(), period_end = character(), grain = character(),
      montant = numeric(), stringsAsFactors = FALSE
    ))
  }
  fl <- fpa_lookup_lines(long_df$libelle, line_map)
  long_df$functional_label <- ifelse(is.na(fl) | !nzchar(fl), NA_character_, as.character(fl))
  long_df <- long_df[!is.na(long_df$functional_label), , drop = FALSE]
  if (!nrow(long_df)) {
    return(data.frame(
      functional_label = character(), period_end = character(), grain = character(),
      montant = numeric(), stringsAsFactors = FALSE
    ))
  }
  agg <- aggregate(
    montant ~ functional_label + period_end + grain,
    data = long_df,
    FUN = function(x) sum(x, na.rm = TRUE)
  )
  agg
}

#' Fold a header for annual-P&L / amount detection.
fpa_fold_header <- function(x) {
  fpa_fold_label(x)
}

#' TRUE when a header is an amount column (Montant, Montant N, Amount).
fpa_header_is_amount <- function(nm) {
  f <- fpa_fold_header(nm)
  grepl("montant|amount|valeur", f) && !grepl("explication|commentaire|note", f)
}

#' TRUE when the raw table looks like a French annual P&L (one row per account,
#' years as amount columns), not a month-by-month long file.
fpa_looks_like_liasse <- function(df, src = NULL) {
  nms <- names(df)
  if (!length(nms)) return(FALSE)
  folds <- vapply(nms, fpa_fold_header, character(1), USE.NAMES = FALSE)
  has_label <- any(grepl("poste|libelle|intitule|compte", folds))
  has_month_col <- any(
    folds %in% c("periode", "period", "mois", "date") |
      grepl("^mois | mois$|^periode |^period ", folds)
  )
  amount_n <- sum(vapply(nms, fpa_header_is_amount, logical(1)))
  yearish <- any(grepl("montant n| n 1|n 1|20[0-9]{2}", folds))
  amount_as_periode <- FALSE
  cmap <- if (!is.null(src)) src$column_map else NULL
  if (!is.null(cmap) && length(cmap)) {
    for (hdr in names(cmap)) {
      if (identical(as.character(cmap[[hdr]]), "periode") && fpa_header_is_amount(hdr)) {
        amount_as_periode <- TRUE
      }
    }
  }
  if (isTRUE(amount_as_periode) && !has_month_col) return(TRUE)
  if (has_label && !has_month_col && yearish) return(TRUE)
  # Annual P&L / budget with a single Montant column is this shape.
  # Opening BS is the same shape on purpose (snapshot, no month column).
  role <- if (!is.null(src)) as.character(src$file_role %||% "") else ""
  if (!identical(role, "opening_bs") && has_label && !has_month_col && amount_n >= 1L) {
    return(TRUE)
  }
  FALSE
}

fpa_liasse_schema_message <- function() {
  paste0(
    fpa_txt("loc_annual_pnl"),
    fpa_txt("loc_not_month_rows_sp"),
    fpa_txt("loc_assistant_amount_period"),
    "\"Montants d'une periode\" et indiquez la date de fin d'exercice (N, 2026, mois…). ",
    fpa_txt("loc_period_not_always_month"),
    fpa_txt("loc_dont_period_on_amount")
  )
}

#' Schema error copy: annual P&L shape in French, otherwise the missing-column line.
fpa_read_schema_message <- function(df, src, kind = c("montant", "periode")) {
  kind <- match.arg(kind)
  if (isTRUE(fpa_looks_like_liasse(df, src))) {
    return(fpa_liasse_schema_message())
  }
  if (identical(kind, "periode")) {
    return("Colonnes attendues manquantes (libelle, periode, montant).")
  }
  "Colonnes attendues manquantes (libelle, montant)."
}

#' Detect dominant grain from a mapped amount table.
fpa_detect_grain <- function(mapped) {
  if (!nrow(mapped)) return("unknown")
  g <- unique(mapped$grain)
  g <- g[!is.na(g) & nzchar(g)]
  if ("snapshot" %in% g) return("snapshot")
  if ("month" %in% g) return("month")
  if ("year" %in% g) return("year")
  # Infer from period_end spacing (skip non-dates)
  raw <- as.character(mapped$period_end)
  ok <- grepl("^\\d{4}-\\d{2}-\\d{2}$", raw)
  pe <- as.Date(raw[ok])
  pe <- sort(unique(pe[!is.na(pe)]))
  if (length(pe) >= 2) {
    gaps <- as.numeric(diff(pe))
    if (median(gaps, na.rm = TRUE) <= 40) return("month")
  }
  if (length(pe) >= 1) return("year")
  "unknown"
}

#' Read one financial source (pnl_actuals, opening_bs, budget) into mapped long form.
fpa_read_financial_source <- function(config, role, paths = fpa_paths(config)) {
  src <- fpa_source_for_role(config, role)
  path <- fpa_resolve_path(config, role, paths)
  res <- fpa_read_raw_file(path)
  if (!is.null(res$error)) {
    return(list(
      role = role,
      path = path,
      ok = FALSE,
      error = res$error,
      message = res$message %||% NULL,
      mapped = NULL,
      grain = "unknown",
      raw = NULL
    ))
  }
  df <- res$data
  layout <- if (!is.null(src)) as.character(src$file_layout %||% "long") else "long"
  line_map <- if (role == "opening_bs") {
    fpa_line_map(config, "bs")
  } else if (role == "budget") {
    fpa_line_map(config, "budget")
  } else {
    fpa_line_map(config, "pnl")
  }

  if (identical(layout, "wide") || any(unlist(src$column_map) == "periode_montant", na.rm = TRUE)) {
    long_df <- fpa_pivot_wide(df, src %||% list())
  } else {
    df2 <- fpa_apply_long_map(df, src$column_map %||% list())
    # Expect libelle + periode/montant or period_end
    if (!("libelle" %in% names(df2))) {
      # try first text column
      for (c in names(df2)) {
        if (is.character(df2[[c]]) || is.factor(df2[[c]])) {
          names(df2)[names(df2) == c] <- "libelle"
          break
        }
      }
    }
    pe_col <- NULL
    for (c in c("period_end", "periode", "period", "mois")) {
      if (c %in% names(df2)) {
        pe_col <- c
        break
      }
    }
    amt_col <- NULL
    for (c in c("montant", "amount", "valeur")) {
      if (c %in% names(df2)) {
        amt_col <- c
        break
      }
    }
    if (is.null(amt_col) || !("libelle" %in% names(df2))) {
      return(list(
        role = role, path = path, ok = FALSE, error = "schema",
        message = fpa_read_schema_message(df, src, "montant"),
        mapped = NULL, grain = "unknown", raw = df
      ))
    }
    # Opening BS snapshot: no period column is OK
    if (is.null(pe_col) && identical(role, "opening_bs")) {
      long_df <- data.frame(
        libelle = as.character(df2$libelle),
        period_end = "opening",
        grain = "snapshot",
        montant = suppressWarnings(as.numeric(gsub(",", ".", as.character(df2[[amt_col]])))),
        user_label = "",
        stringsAsFactors = FALSE
      )
    } else if (is.null(pe_col)) {
      return(list(
        role = role, path = path, ok = FALSE, error = "schema",
        message = fpa_read_schema_message(df, src, "periode"),
        mapped = NULL, grain = "unknown", raw = df
      ))
    } else {
      pe <- fpa_parse_period(df2[[pe_col]])
      long_df <- data.frame(
        libelle = as.character(df2$libelle),
        period_end = as.character(pe),
        grain = "month",
        montant = suppressWarnings(as.numeric(gsub(",", ".", as.character(df2[[amt_col]])))),
        user_label = "",
        stringsAsFactors = FALSE
      )
      if (role == "opening_bs" && all(is.na(pe) | !nzchar(as.character(pe)))) {
        long_df$period_end <- "opening"
        long_df$grain <- "snapshot"
      }
    }
  }

  mapped <- fpa_map_amounts(long_df, line_map)
  grain <- fpa_detect_grain(mapped)
  list(
    role = role,
    path = path,
    ok = TRUE,
    error = NULL,
    message = NULL,
    mapped = mapped,
    grain = grain,
    raw = df,
    long = long_df
  )
}

#' Read all core financial roles present in paths.
fpa_read_all_financial <- function(config, paths = fpa_paths(config)) {
  roles <- c("pnl_actuals", "opening_bs", "budget")
  monthly <- as.character(config$paths$pnl_monthly %||% "")
  if (nzchar(monthly)) roles <- c(roles, "pnl_monthly")
  out <- list()
  for (r in roles) {
    if (is.null(config$paths[[r]]) || !nzchar(as.character(config$paths[[r]]))) next
    out[[r]] <- fpa_read_financial_source(config, r, paths)
  }
  out
}
