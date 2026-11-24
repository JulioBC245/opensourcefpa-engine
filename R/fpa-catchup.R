# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-catchup.R — One missing period, typed into the source CSV.
# LAST CHANGE: 2026-09-22
# Excel workbooks and dates that live only in the configuration are not written.
# A save appends rows. It does not rewrite the lines already in the file.

fpa_catchup_needed <- function(frame, today = Sys.Date()) {
  today <- as.Date(today)
  if (identical(as.character(frame), "rolling")) {
    first <- as.Date(paste0(format(today, "%Y-%m"), "-01"))
    return(seq(first, by = "-1 month", length.out = 2)[2])
  }
  if (identical(as.character(frame), "history")) {
    return(as.Date(paste0(as.integer(format(today, "%Y")) - 1L, "-12-01")))
  }
  as.Date(paste0(format(today, "%Y"), "-12-01"))
}

fpa_catchup_ym <- function(x) {
  raw <- trimws(as.character(x))
  out <- rep("", length(raw))
  ok <- !is.na(raw) & grepl("^[0-9]{4}-[0-9]{2}", raw)
  out[ok] <- substr(raw[ok], 1L, 7L)
  out
}

fpa_catchup_source <- function(config, id) {
  want <- sub("^src:", "", as.character(id))
  for (s in config$sources %||% list()) {
    if (identical(as.character(s$source_id %||% ""), want)) return(s)
  }
  NULL
}

fpa_catchup_sep <- function(header) {
  semis <- length(unlist(strsplit(header, ";", fixed = TRUE))) - 1L
  commas <- length(unlist(strsplit(header, ",", fixed = TRUE))) - 1L
  if (semis > commas) ";" else ","
}

fpa_catchup_read <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (!nzchar(path) || !file.exists(path)) return(list(ok = FALSE, reason = "missing"))
  if (file.access(path, 2) != 0) return(list(ok = FALSE, reason = "locked"))
  if (ext %in% c("xlsx", "xlsm")) {
    if (!requireNamespace("openxlsx", quietly = TRUE)) return(list(ok = FALSE, reason = "excel"))
    df <- tryCatch(
      as.data.frame(openxlsx::read.xlsx(path, sheet = 1), stringsAsFactors = FALSE),
      error = function(e) NULL
    )
    if (is.null(df) || !ncol(df)) return(list(ok = FALSE, reason = "excel"))
    return(list(ok = TRUE, df = df, sep = ",", lines = NULL, excel = TRUE))
  }
  if (!ext %in% c("csv", "txt")) return(list(ok = FALSE, reason = "excel"))
  lines <- readLines(path, encoding = "UTF-8", warn = FALSE)
  lines <- lines[nzchar(lines)]
  if (length(lines) < 2L) return(list(ok = FALSE, reason = "empty"))
  sep <- fpa_catchup_sep(lines[[1]])
  df <- utils::read.csv(
    text = paste(lines, collapse = "\n"),
    sep = sep, stringsAsFactors = FALSE, check.names = FALSE,
    fileEncoding = "UTF-8"
  )
  list(ok = TRUE, df = df, sep = sep, lines = lines, excel = FALSE)
}

fpa_catchup_date_col <- function(names) {
  key <- tolower(names)
  hit <- which(key %in% c("periode", "period", "date", "mois"))
  if (!length(hit)) return(NA_integer_)
  hit[[1]]
}

fpa_catchup_is_num <- function(x) {
  raw <- trimws(as.character(x))
  raw <- raw[nzchar(raw) & !is.na(raw) & toupper(raw) != "NA"]
  if (!length(raw)) return(FALSE)
  raw <- gsub(",", ".", raw, fixed = TRUE)
  all(grepl("^-?[0-9]+(\\.[0-9]+)?$", raw))
}

fpa_catchup_show <- function(x) {
  if (!length(x) || all(is.na(x))) return("")
  raw <- as.character(x[[1]])
  if (!nzchar(raw) || identical(raw, "NA")) return("")
  raw
}

fpa_catchup_blank <- function() {
  data.frame(
    id = character(), label = character(), key = character(),
    h1 = character(), h2 = character(),
    stringsAsFactors = FALSE
  )
}

#' Last two periods already in the file, plus the month that clears the late mark.
#' ok FALSE when this screen must not write the file.
fpa_catchup_preview <- function(config, source_id, today = Sys.Date()) {
  fail <- function(reason) {
    list(ok = FALSE, reason = reason, title = "", path = "", needed = "", hist = character(), rows = fpa_catchup_blank(), sep = ",")
  }
  source <- fpa_catchup_source(config, source_id)
  if (is.null(source)) return(fail("missing"))
  title <- as.character(source$source_name %||% source$source_id %||% "")
  path <- fpa_chain_source_path(config, source)
  frame <- fpa_chain_frame(source)
  needed <- format(fpa_catchup_needed(frame, today), "%Y-%m")
  base <- fail("empty")
  base$title <- title
  base$path <- path
  base$needed <- needed
  if (!nzchar(path)) return(base)
  got <- fpa_catchup_read(path)
  if (!isTRUE(got$ok)) {
    base$reason <- got$reason
    return(base)
  }
  df <- got$df
  date_i <- fpa_catchup_date_col(names(df))
  if (is.na(date_i)) {
    if (!isTRUE(got$excel)) {
      base$reason <- "nodate"
      return(base)
    }
    num <- vapply(df, fpa_catchup_is_num, logical(1))
    label_i <- which(!num)[1]
    value_i <- which(num)
    if (is.na(label_i) || !length(value_i)) {
      base$reason <- "nodate"
      return(base)
    }
    ends <- vapply(source$periods %||% list(), function(p) as.character(p$period_end %||% ""), character(1))
    ends <- ends[nzchar(ends)]
    ends <- format(as.Date(ends), "%Y-%m")
    ends <- ends[!is.na(ends)]
    value_name <- names(df)[[value_i[[length(value_i)]]]]
    hist <- if (length(ends)) tail(ends, 1) else value_name
    rows <- fpa_catchup_blank()
    labels <- as.character(df[[label_i]])
    vals <- df[[value_name]]
    for (i in seq_len(nrow(df))) {
      if (!nzchar(labels[[i]]) || is.na(labels[[i]])) next
      rows <- rbind(rows, data.frame(
        id = paste0("y", i),
        label = labels[[i]],
        key = as.character(i + 1L),
        h1 = fpa_catchup_show(vals[[i]]),
        h2 = "",
        stringsAsFactors = FALSE
      ))
    }
    if (!nrow(rows)) {
      base$reason <- "nodate"
      return(base)
    }
    return(list(
      ok = TRUE, reason = "", title = title, path = path, needed = needed,
      hist = hist, rows = rows, sep = ",", layout = "yearcol", excel = TRUE,
      date_name = "", value_name = value_name, key_names = names(df)[[label_i]],
      col_names = names(df), n_body = nrow(df)
    ))
  }
  date_name <- names(df)[[date_i]]
  ym <- fpa_catchup_ym(df[[date_i]])
  periods <- sort(unique(ym[nzchar(ym)]))
  if (!length(periods)) {
    base$reason <- "nodate"
    return(base)
  }
  hist <- tail(periods[periods < needed], 2)
  if (!length(hist)) hist <- tail(periods, 2)
  num <- vapply(df, fpa_catchup_is_num, logical(1))
  num[[date_i]] <- FALSE
  others <- setdiff(seq_along(df), c(date_i, which(num)))
  long <- length(which(num)) == 1L && length(others) >= 1L
  rows <- fpa_catchup_blank()
  if (long) {
    value_name <- names(df)[[which(num)]]
    key_names <- names(df)[others]
    df$.ym <- fpa_catchup_ym(df[[date_i]])
    keys <- unique(df[key_names])
    for (i in seq_len(nrow(keys))) {
      key <- keys[i, , drop = FALSE]
      hit <- df
      for (k in key_names) hit <- hit[as.character(hit[[k]]) == as.character(key[[k]]), , drop = FALSE]
      lab <- paste(vapply(key_names, function(k) as.character(key[[k]]), character(1)), collapse = " · ")
      key_flat <- paste(vapply(key_names, function(k) as.character(key[[k]]), character(1)), collapse = "\001")
      cell <- function(period) {
        v <- hit[[value_name]][hit$.ym == period]
        fpa_catchup_show(if (length(v)) v[[length(v)]] else NA)
      }
      rows <- rbind(rows, data.frame(
        id = paste0("k", i),
        label = lab,
        key = key_flat,
        h1 = if (length(hist) >= 1L) cell(hist[[1]]) else "",
        h2 = if (length(hist) >= 2L) cell(hist[[2]]) else "",
        stringsAsFactors = FALSE
      ))
    }
    layout <- "long"
  } else {
    value_names <- names(df)[num]
    df$.ym <- fpa_catchup_ym(df[[date_i]])
    cell <- function(col, period) {
      v <- df[[col]][df$.ym == period]
      fpa_catchup_show(if (length(v)) v[[length(v)]] else NA)
    }
    for (i in seq_along(value_names)) {
      col <- value_names[[i]]
      rows <- rbind(rows, data.frame(
        id = paste0("c", i),
        label = col,
        key = col,
        h1 = if (length(hist) >= 1L) cell(col, hist[[1]]) else "",
        h2 = if (length(hist) >= 2L) cell(col, hist[[2]]) else "",
        stringsAsFactors = FALSE
      ))
    }
    layout <- "wide"
    value_name <- ""
    key_names <- character()
  }
  if (!nrow(rows)) {
    base$reason <- "nodate"
    return(base)
  }
  list(
    ok = TRUE, reason = "", title = title, path = path, needed = needed,
    hist = hist, rows = rows, sep = got$sep, layout = layout, excel = isTRUE(got$excel),
    date_name = date_name, value_name = value_name, key_names = key_names,
    col_names = names(got$df), n_body = nrow(got$df)
  )
}

fpa_catchup_number <- function(x) {
  raw <- trimws(as.character(x))
  if (!length(raw) || !nzchar(raw[[1]])) return(NA_character_)
  raw <- gsub(",", ".", raw[[1]], fixed = TRUE)
  raw <- gsub(" ", "", raw, fixed = TRUE)
  if (!grepl("^-?[0-9]+(\\.[0-9]+)?$", raw)) return(NA_character_)
  raw
}

fpa_catchup_field <- function(x, sep) {
  raw <- as.character(x)
  if (!length(raw) || is.na(raw) || !nzchar(raw)) return("")
  if (grepl("^-?[0-9]+(\\.[0-9]+)?$", raw) && !grepl(sep, raw, fixed = TRUE)) return(raw)
  paste0("\"", gsub("\"", "\"\"", raw, fixed = TRUE), "\"")
}

#' Append filled cells for the needed month. Existing lines stay byte for byte.
fpa_catchup_save_excel <- function(sheet, values) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) return(list(ok = FALSE, n = 0L))
  entered <- vapply(sheet$rows$id, function(id) fpa_catchup_number(values[[id]]), character(1))
  keep <- !is.na(entered)
  if (!any(keep)) return(list(ok = TRUE, n = 0L))
  wb <- tryCatch(openxlsx::loadWorkbook(sheet$path), error = function(e) NULL)
  if (is.null(wb)) return(list(ok = FALSE, n = 0L))
  sh <- openxlsx::getSheetNames(sheet$path)[[1]]
  if (identical(sheet$layout, "yearcol")) {
    headers <- as.character(unlist(openxlsx::read.xlsx(sheet$path, sheet = sh, rows = 1, colNames = FALSE), use.names = FALSE))
    headers <- headers[!is.na(headers)]
    year <- substr(sheet$needed, 1L, 4L)
    col_i <- match(year, headers)
    if (is.na(col_i)) col_i <- length(headers) + 1L
    openxlsx::writeData(wb, sh, x = year, startCol = col_i, startRow = 1, colNames = FALSE)
    n <- 0L
    for (i in which(keep)) {
      excel_row <- as.integer(sheet$rows$key[[i]])
      if (!length(excel_row) || is.na(excel_row)) next
      openxlsx::writeData(
        wb, sh, x = as.numeric(entered[[i]]),
        startCol = col_i, startRow = excel_row, colNames = FALSE
      )
      n <- n + 1L
    }
    openxlsx::saveWorkbook(wb, sheet$path, overwrite = TRUE)
    return(list(ok = TRUE, n = n))
  }
  list(ok = FALSE, n = 0L)
}

fpa_catchup_save <- function(sheet, values) {
  if (is.null(sheet) || !isTRUE(sheet$ok)) return(list(ok = FALSE, n = 0L))
  if (isTRUE(sheet$excel)) return(fpa_catchup_save_excel(sheet, values))
  got <- fpa_catchup_read(sheet$path)
  if (!isTRUE(got$ok)) return(list(ok = FALSE, n = 0L))
  df <- got$df
  entered <- vapply(sheet$rows$id, function(id) fpa_catchup_number(values[[id]]), character(1))
  keep <- !is.na(entered)
  if (!any(keep)) return(list(ok = TRUE, n = 0L))
  date_i <- match(sheet$date_name, names(df))
  ym <- fpa_catchup_ym(df[[date_i]])
  new_lines <- character()
  if (identical(sheet$layout, "long")) {
    value_i <- match(sheet$value_name, names(df))
    for (i in which(keep)) {
      parts <- strsplit(sheet$rows$key[[i]], "\001", fixed = TRUE)[[1]]
      if (length(parts) != length(sheet$key_names)) next
      hit <- rep(TRUE, nrow(df))
      for (k in seq_along(sheet$key_names)) {
        hit <- hit & as.character(df[[sheet$key_names[[k]]]]) == parts[[k]]
      }
      if (any(hit & ym == sheet$needed)) next
      donor <- df[hit, , drop = FALSE]
      donor <- if (nrow(donor)) donor[nrow(donor), , drop = FALSE] else df[1, , drop = FALSE]
      donor[[date_i]] <- sheet$needed
      donor[[value_i]] <- unname(entered[[i]])
      for (k in seq_along(sheet$key_names)) donor[[sheet$key_names[[k]]]] <- parts[[k]]
      fields <- vapply(seq_along(donor), function(j) fpa_catchup_field(donor[[j]], sheet$sep), character(1))
      new_lines <- c(new_lines, paste(fields, collapse = sheet$sep))
    }
  } else {
    if (any(ym == sheet$needed)) return(list(ok = TRUE, n = 0L))
    donor <- df[nrow(df), , drop = FALSE]
    donor[[date_i]] <- sheet$needed
    for (i in seq_len(nrow(sheet$rows))) {
      col <- sheet$rows$label[[i]]
      if (!col %in% names(donor)) next
      donor[[col]] <- if (keep[[i]]) unname(entered[[i]]) else ""
    }
    fields <- vapply(seq_along(donor), function(j) fpa_catchup_field(donor[[j]], sheet$sep), character(1))
    new_lines <- c(new_lines, paste(fields, collapse = sheet$sep))
  }
  new_lines <- new_lines[nzchar(new_lines)]
  if (!length(new_lines)) return(list(ok = TRUE, n = 0L))
  old <- got$lines
  con <- file(sheet$path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(c(old, new_lines), con, useBytes = FALSE)
  list(ok = TRUE, n = length(new_lines))
}
