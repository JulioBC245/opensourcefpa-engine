# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-cell-read.R — one forecast month, with the figures that build it.
# LAST CHANGE: 2026-09-28 — cash bridge and balance sheet use the same cell trail.

#' Money for the OI cell card. Whole euros stay bare. Cents always use two digits.
fpa_cell_amt <- function(x, digits = NULL) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(x)) return("\u2013")
  if (is.null(digits)) {
    cents <- round(x, 2)
    if (abs(cents - round(cents)) < 1e-9) {
      txt <- format(round(abs(cents)), big.mark = " ", scientific = FALSE, trim = TRUE)
    } else {
      txt <- formatC(abs(cents), format = "f", digits = 2L, decimal.mark = ",", big.mark = " ")
    }
  } else if (identical(as.integer(digits), 0L)) {
    txt <- format(round(abs(x)), big.mark = " ", scientific = FALSE, trim = TRUE)
  } else {
    txt <- formatC(abs(x), format = "f", digits = as.integer(digits), decimal.mark = ",", big.mark = " ")
  }
  if (x < -0.005) paste0("\u2212", txt) else txt
}

fpa_cell_screen <- function(x) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(x)) return("\u2013")
  format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE)
}

fpa_cell_factor <- function(x) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(x)) return("\u2013")
  if (abs(x - round(x)) < 1e-6) {
    return(format(round(x), big.mark = " ", scientific = FALSE, trim = TRUE))
  }
  txt <- formatC(x, format = "f", digits = 6, decimal.mark = ",", big.mark = " ")
  txt <- sub("0+$", "", txt)
  sub(",$", "", txt)
}

fpa_cell_times <- function(...) {
  paste(c(...), collapse = " \u00d7 ")
}

#' Operating income for one forecast month. Actual months return ok = FALSE.
fpa_oi_cell_read <- function(config, periode, paths = fpa_paths(config)) {
  no <- list(ok = FALSE)
  periode <- as.character(periode %||% "")
  if (!grepl("^[0-9]{4}-[0-9]{2}$", periode)) return(no)
  pnl_path <- file.path(paths$work, "pnl-monthly.csv")
  status_path <- file.path(paths$work, "period-status.csv")
  if (!file.exists(pnl_path) || !file.exists(status_path)) return(no)
  pnl <- utils::read.csv(pnl_path, stringsAsFactors = FALSE)
  status <- utils::read.csv(status_path, stringsAsFactors = FALSE)
  if (!all(c("periode", "status") %in% names(status))) return(no)
  st <- as.character(status$status[as.character(status$periode) == periode])
  if (!length(st) || !identical(st[[1]], "forecast")) return(no)
  row <- pnl[as.character(pnl$periode) == periode, , drop = FALSE]
  need <- c("REV", "OTHER_OP_INC", "COGS", "PAYROLL", "OPEX_OTHER", "D_A", "FIN", "EXCEP", "TAX", "OI")
  if (!nrow(row) || !all(need %in% names(row))) return(no)
  num <- function(id) suppressWarnings(as.numeric(row[[id]][[1]]))
  rev <- num("REV")
  other <- num("OTHER_OP_INC")
  cogs <- num("COGS")
  pay <- num("PAYROLL")
  opex <- num("OPEX_OTHER")
  da <- num("D_A")
  fin <- num("FIN")
  excep <- num("EXCEP")
  tax <- num("TAX")
  gp <- rev + other - cogs
  ebitda <- gp - pay - opex
  oi <- ebitda - da
  if (!is.finite(oi) || abs(oi - num("OI")) > 0.05) return(no)

  vol <- tryCatch(fpa_read_volumes(config, paths), error = function(e) NULL)
  vol_file <- if (!is.null(vol) && isTRUE(vol$ok)) basename(as.character(vol$path)) else ""
  sl <- if (!is.null(vol) && isTRUE(vol$ok) && !is.null(vol$data)) {
    vol$data[as.character(vol$data$periode) == periode, , drop = FALSE]
  } else {
    NULL
  }
  nights <- if (!is.null(vol) && isTRUE(vol$ok)) {
    vm <- fpa_volume_by_month(vol$data)
    if (periode %in% names(vm)) as.numeric(vm[[periode]]) else NA_real_
  } else {
    NA_real_
  }

  fit_path <- file.path(paths$work, "line-drivers.csv")
  fit <- if (file.exists(fit_path)) {
    utils::read.csv(fit_path, stringsAsFactors = FALSE)
  } else {
    NULL
  }
  hist <- tryCatch(fpa_line_history(config, paths), error = function(e) NULL)
  hpnl <- tryCatch(fpa_history_pnl(config, paths), error = function(e) NULL)
  hist_files <- list.files(paths$input, pattern = "pnl.*\\.csv$", full.names = TRUE, ignore.case = TRUE)
  hist_file <- if (length(hist_files)) basename(hist_files[[which.max(file.info(hist_files)$size)]]) else ""
  prev_p <- sprintf("%04d-%02d", as.integer(substr(periode, 1, 4)) - 1L, as.integer(substr(periode, 6, 7)))
  prev <- if (!is.null(hpnl) && nrow(hpnl) && prev_p %in% as.character(hpnl$periode)) {
    hpnl[as.character(hpnl$periode) == prev_p, , drop = FALSE]
  } else {
    NULL
  }
  prev_amt <- function(id) {
    if (is.null(prev) || !(id %in% names(prev))) return(NA_real_)
    suppressWarnings(as.numeric(prev[[id]][[1]]))
  }

  one_vol <- !is.null(sl) && nrow(sl) == 1L
  rev_factors <- list()
  pay_factors <- list()
  rev_eq <- ""
  if (one_vol && all(c("capacite", "taux_utilisation", "tarif") %in% names(sl))) {
    built <- as.numeric(sl$capacite) * as.numeric(sl$taux_utilisation) * as.numeric(sl$tarif)
    if (is.finite(built) && abs(built - rev) < 0.05) {
      rev_factors <- list(
        list(op = "", label = fpa_txt("oi_fac_cap"), value = fpa_cell_factor(sl$capacite)),
        list(op = "\u00d7", label = fpa_txt("oi_fac_util"), value = fpa_cell_factor(sl$taux_utilisation)),
        list(op = "\u00d7", label = fpa_txt("oi_fac_rate"), value = fpa_cell_amt(sl$tarif))
      )
      rev_eq <- fpa_cell_times(
        fpa_cell_factor(sl$capacite),
        fpa_cell_factor(sl$taux_utilisation),
        fpa_cell_amt(sl$tarif)
      )
    }
  }
  pay_eq <- ""
  if (one_vol && all(c("effectif", "cout_moyen") %in% names(sl))) {
    built <- as.numeric(sl$effectif) * as.numeric(sl$cout_moyen)
    if (is.finite(built) && abs(built - pay) < 0.05) {
      pay_factors <- list(
        list(op = "", label = fpa_txt("oi_fac_head"), value = fpa_cell_factor(sl$effectif)),
        list(op = "\u00d7", label = fpa_txt("oi_fac_cost"), value = fpa_cell_amt(sl$cout_moyen))
      )
      pay_eq <- fpa_cell_times(fpa_cell_factor(sl$effectif), fpa_cell_amt(sl$cout_moyen))
    }
  }

  driven_value <- function(bucket) {
    if (is.null(fit) || !nrow(fit) || !("functional_label" %in% names(fit))) return(list())
    lines <- fit[fit$functional_label == bucket & fit$method != "seasonal", , drop = FALSE]
    if (!nrow(lines) || !is.finite(nights)) return(list())
    lapply(seq_len(nrow(lines)), function(k) {
      line <- lines[k, , drop = FALSE]
      list(
        label = as.character(line$user_label[[1]]),
        driver = as.character(line$driver[[1]]),
        fixed = as.numeric(line$fixed[[1]]),
        per_volume = as.numeric(line$per_volume[[1]]),
        value = fpa_line_driver_value(line, nights, rev)
      )
    })
  }
  cogs_bits <- driven_value("COGS")
  opex_bits <- driven_value("OPEX_OTHER")
  cogs_add <- sum(vapply(cogs_bits, function(b) b$value, numeric(1)))
  opex_add <- sum(vapply(opex_bits, function(b) b$value, numeric(1)))
  if (!length(cogs_bits)) cogs_add <- NA_real_
  if (!length(opex_bits)) opex_add <- NA_real_
  cogs_eq <- ""
  cogs_note <- ""
  cogs_factors <- list()
  if (length(cogs_bits) == 1L && is.finite(cogs_add) && abs(cogs_add - cogs) < 0.05) {
    bit <- cogs_bits[[1]]
    cogs_factors <- list(
      list(op = "", label = fpa_txt("oi_fac_nights"), value = fpa_cell_factor(nights)),
      list(op = "\u00d7", label = fpa_txt("oi_fac_unit"), value = fpa_cell_amt(bit$per_volume))
    )
    cogs_eq <- fpa_cell_times(fpa_cell_factor(nights), fpa_cell_amt(bit$per_volume))
    cogs_note <- fpa_txt("oi_read_cogs", bit$label, vol_file)
  }
  opex_driven <- NULL
  opex_rest <- NA_real_
  opex_rest_note <- ""
  if (length(opex_bits) == 1L && is.finite(opex_add)) {
    bit <- opex_bits[[1]]
    opex_rest <- opex - bit$value
    opex_driven <- list(
      label = bit$label,
      equation = paste0(
        fpa_cell_amt(bit$fixed), " + ",
        fpa_cell_amt(bit$per_volume), " \u00d7 ", fpa_cell_factor(nights)
      ),
      factors = list(
        list(op = "", label = fpa_txt("oi_fac_fixed"), value = fpa_cell_amt(bit$fixed)),
        list(op = "+", label = fpa_txt("oi_fac_unit"), value = fpa_cell_amt(bit$per_volume)),
        list(op = "\u00d7", label = fpa_txt("oi_fac_nights"), value = fpa_cell_factor(nights))
      ),
      amount = bit$value,
      note = fpa_txt("oi_read_opex_driven", bit$label, vol_file)
    )
    rest_labs <- fit$user_label[fit$functional_label == "OPEX_OTHER" & fit$method == "seasonal"]
    rest_labs <- rest_labs[nzchar(rest_labs)]
    if (length(rest_labs) && is.finite(opex_rest) && abs(opex_rest) >= 0.5) {
      opex_rest_note <- fpa_txt("oi_read_opex_rest", paste(rest_labs, collapse = ", "))
    }
  }

  other_note <- fpa_txt("oi_read_other_plain")
  prev_other <- prev_amt("OTHER_OP_INC")
  if (nzchar(hist_file) && is.finite(prev_other)) {
    other_note <- fpa_txt("oi_read_other", hist_file, prev_p, fpa_cell_amt(prev_other))
  }
  da_note <- fpa_txt("oi_read_da_plain")
  prev_da <- prev_amt("D_A")
  if (nzchar(hist_file) && is.finite(prev_da)) {
    da_note <- fpa_txt("oi_read_da", prev_p, hist_file, fpa_cell_amt(prev_da))
  }
  extra_da <- fpa_cell_calendar_da(config, paths, periode)
  cal_file <- fpa_cell_calendar(config, paths)$file
  if (is.finite(extra_da) && extra_da >= 0.5 && nzchar(cal_file)) {
    da_note <- fpa_txt("oi_read_da_added", fpa_cell_amt(extra_da), cal_file)
  }
  rev_note <- if (length(rev_factors) && nzchar(vol_file)) fpa_txt("oi_read_rev", vol_file, periode) else ""
  pay_note <- if (length(pay_factors) && nzchar(vol_file)) fpa_txt("oi_read_pay", vol_file) else ""

  rate <- NA_real_
  assum <- tryCatch(fpa_read_assumptions(config, paths), error = function(e) NULL)
  if (!is.null(assum) && isTRUE(assum$ok)) {
    rate <- tryCatch(
      fpa_assumption_num(assum$wide, periode, "taux.impot", default = NA_real_),
      error = function(e) NA_real_
    )
  }
  rate_txt <- if (is.finite(rate)) {
    paste0(formatC(rate * 100, format = "f", digits = 0), " %")
  } else {
    ""
  }

  list(
    ok = TRUE,
    periode = periode,
    oi = oi,
    screen = fpa_cell_screen(oi),
    precise = fpa_cell_amt(oi, digits = 2),
    gp = gp,
    ebitda = ebitda,
    rev = rev,
    other = other,
    cogs = cogs,
    pay = pay,
    opex = opex,
    da = da,
    fin = fin,
    excep = excep,
    tax = tax,
    ni = oi - fin - excep - tax,
    nights = nights,
    rev_eq = rev_eq,
    rev_factors = rev_factors,
    rev_note = rev_note,
    pay_eq = pay_eq,
    pay_factors = pay_factors,
    pay_note = pay_note,
    cogs_eq = cogs_eq,
    cogs_factors = cogs_factors,
    cogs_note = cogs_note,
    other_note = other_note,
    opex_driven = opex_driven,
    opex_rest = opex_rest,
    opex_rest_note = opex_rest_note,
    da_note = da_note,
    tax_note = if (nzchar(rate_txt)) fpa_txt("oi_read_tax", rate_txt) else "",
    vol_file = vol_file,
    hist_file = hist_file
  )
}

fpa_file_edit_number <- function(raw) {
  raw <- trimws(as.character(raw))
  raw[!nzchar(raw)] <- NA_character_
  raw <- gsub(" ", "", raw, fixed = TRUE)
  raw <- gsub(",", ".", raw, fixed = TRUE)
  suppressWarnings(as.numeric(raw))
}

#' A csv in input/. Numeric cells open only on months marked forecast.
fpa_file_edit_sheet <- function(config, name, paths = fpa_paths(config)) {
  name <- basename(as.character(name %||% ""))
  bad <- list(ok = FALSE, csv = FALSE, name = name, edits = list())
  if (!grepl("^[A-Za-z0-9._-]+$", name)) return(bad)
  path <- file.path(paths$input, name)
  if (!file.exists(path)) return(bad)
  if (!identical(tolower(tools::file_ext(path)), "csv")) {
    return(list(ok = TRUE, csv = FALSE, name = name, path = path, edits = list()))
  }
  df <- fpa_csv_read_sniff(path)
  if (is.null(df)) return(bad)
  sep <- attr(df, "fpa_sep") %||% ","
  encoding <- attr(df, "fpa_encoding") %||% "UTF-8"
  forecast <- character()
  status_path <- file.path(paths$work, "period-status.csv")
  if (file.exists(status_path)) {
    st <- utils::read.csv(status_path, stringsAsFactors = FALSE)
    if (all(c("periode", "status") %in% names(st))) {
      forecast <- as.character(st$periode[as.character(st$status) == "forecast"])
    }
  }
  date_i <- which(tolower(names(df)) %in% c("periode", "period", "date", "mois"))[1]
  ym <- rep("", nrow(df))
  if (length(date_i) == 1L && !is.na(date_i)) {
    raw <- as.character(df[[date_i]])
    hit <- grepl("^[0-9]{4}-[0-9]{2}", raw)
    ym[hit] <- substr(raw[hit], 1L, 7L)
  }
  open_row <- ym %in% forecast
  num_col <- vapply(seq_along(df), function(j) {
    if (length(date_i) == 1L && !is.na(date_i) && j == date_i) return(FALSE)
    raw <- as.character(df[[j]])
    keep <- !is.na(df[[j]]) & nzchar(trimws(raw)) & raw != "NA"
    if (!any(keep)) return(FALSE)
    parsed <- fpa_file_edit_number(raw[keep])
    all(is.finite(parsed))
  }, logical(1))
  edits <- list()
  if (any(open_row) && any(num_col)) {
    for (i in which(open_row)) {
      for (j in which(num_col)) {
        shown <- df[[j]][[i]]
        txt <- if (is.na(shown) || !nzchar(as.character(shown))) {
          ""
        } else {
          num <- suppressWarnings(as.numeric(shown))
          if (is.finite(num)) format(num, scientific = FALSE, trim = TRUE) else as.character(shown)
        }
        edits[[length(edits) + 1L]] <- list(
          id = sprintf("fe_%d_%d", i, j),
          row = i,
          col = j,
          value = txt
        )
      }
    }
  }
  list(
    ok = TRUE, csv = TRUE, name = name, path = path, df = df,
    sep = sep, encoding = encoding,
    ym = ym, open_row = open_row, num_col = num_col, edits = edits
  )
}

#' Write back only the forecast cells listed on the sheet. Past rows are ignored.
fpa_file_edit_apply <- function(sheet, values) {
  if (is.null(sheet) || !isTRUE(sheet$ok) || !isTRUE(sheet$csv)) return(list(ok = FALSE, n = 0L))
  df <- sheet$df
  n <- 0L
  for (e in sheet$edits) {
    raw <- values[[e$id]]
    if (is.null(raw)) next
    num <- fpa_file_edit_number(raw)
    if (!is.finite(num)) next
    old <- fpa_file_edit_number(df[[e$col]][[e$row]])
    if (is.finite(old) && abs(old - num) < 1e-8) next
    df[[e$col]][[e$row]] <- num
    n <- n + 1L
  }
  if (n > 0L) {
    fpa_csv_write_same(df, sheet$path, sep = sheet$sep %||% ",", encoding = sheet$encoding %||% "UTF-8")
  }
  list(ok = TRUE, n = n)
}

fpa_cell_tie <- function(a, b, tol = 0.05) {
  a <- suppressWarnings(as.numeric(a[[1]]))
  b <- suppressWarnings(as.numeric(b[[1]]))
  is.finite(a) && is.finite(b) && abs(a - b) <= tol
}

fpa_cell_csv <- function(paths, name) {
  p <- file.path(paths$work, name)
  if (!file.exists(p)) return(NULL)
  utils::read.csv(p, stringsAsFactors = FALSE)
}

fpa_cell_is_forecast <- function(paths, periode) {
  st <- fpa_cell_csv(paths, "period-status.csv")
  if (is.null(st) || !all(c("periode", "status") %in% names(st))) return(FALSE)
  hit <- as.character(st$status[as.character(st$periode) == periode])
  length(hit) > 0L && identical(hit[[1]], "forecast")
}

fpa_cell_pick <- function(df, periode) {
  if (is.null(df) || !nrow(df) || !("periode" %in% names(df))) return(NULL)
  hit <- df[as.character(df$periode) == periode, , drop = FALSE]
  if (!nrow(hit)) return(NULL)
  hit[1, , drop = FALSE]
}

fpa_cell_at <- function(row, id) {
  if (is.null(row) || !(id %in% names(row))) return(NA_real_)
  x <- suppressWarnings(as.numeric(row[[id]][[1]]))
  if (!is.finite(x)) NA_real_ else x
}

fpa_cell_prev <- function(df, periode) {
  if (is.null(df) || !nrow(df)) return("")
  pers <- as.character(df$periode)
  i <- match(periode, pers)
  if (is.na(i) || i <= 1L) return("")
  pers[[i - 1L]]
}

fpa_cell_base <- function(config, role, paths) {
  p <- tryCatch(fpa_resolve_path(config, role, paths), error = function(e) NA_character_)
  if (length(p) != 1L || is.na(p) || !nzchar(p)) return("")
  basename(p)
}

fpa_cell_days <- function(x) {
  x <- suppressWarnings(as.numeric(x[[1]]))
  if (!is.finite(x)) return("\u2013")
  if (abs(x - round(x)) < 1e-6) as.character(as.integer(round(x))) else fpa_cell_factor(x)
}

fpa_cell_line <- function(id, label, amount, role = "term", factors = list(), equation = "", note = "") {
  list(
    id = id, label = label, amount = amount, role = role,
    factors = factors, equation = equation, note = note
  )
}

fpa_cell_fac <- function(op, label, value) {
  list(op = op, label = label, value = value)
}

fpa_cell_debt_note <- function(debt, cal_debt, dec_file, debt_on) {
  if (!nzchar(dec_file %||% "")) return("")
  if (cal_debt >= 0.5 && abs(abs(debt) - cal_debt) < 0.5) {
    return(fpa_txt("cash_read_debt_on", dec_file))
  }
  if (!isTRUE(debt_on) && cal_debt >= 0.5 && abs(debt) < 0.5) {
    return(fpa_txt("cash_read_debt_off", dec_file, fpa_cell_amt(cal_debt)))
  }
  if (abs(debt) < 0.5 && cal_debt < 0.5) return(fpa_txt("cash_read_debt_none"))
  ""
}

#' Depreciation added this month by capex dated on a forecast month.
fpa_cell_calendar_da <- function(config, paths, periode) {
  dec <- tryCatch(fpa_read_decisions(config, paths), error = function(e) NULL)
  if (is.null(dec) || !isTRUE(dec$ok) || is.null(dec$data) || !nrow(dec$data)) return(0)
  keep <- vapply(dec$data$periode, function(p) isTRUE(fpa_cell_is_forecast(paths, p)), logical(1))
  open <- dec$data[keep, , drop = FALSE]
  if (!nrow(open)) return(0)
  extra <- fpa_da_from_capex(open, periode)
  x <- suppressWarnings(as.numeric(extra[[as.character(periode)]]))
  if (length(x) != 1L || !is.finite(x)) 0 else x
}

#' Calendar rows for the cell note. Comma and semicolon files both apply.
fpa_cell_calendar <- function(config, paths) {
  empty <- list(ok = FALSE, file = "", data = NULL, applied = FALSE)
  dec <- tryCatch(fpa_read_decisions(config, paths), error = function(e) NULL)
  if (!is.null(dec) && isTRUE(dec$ok) && !is.null(dec$data)) {
    file <- if (!is.na(dec$path)) basename(as.character(dec$path)) else ""
    return(list(ok = TRUE, file = file, data = dec$data, applied = TRUE))
  }
  path <- tryCatch(fpa_resolve_path(config, "decisions", paths), error = function(e) NA_character_)
  if (length(path) != 1L || is.na(path) || !nzchar(path) || !file.exists(path)) return(empty)
  file <- basename(path)
  df <- tryCatch(
    utils::read.csv(path, sep = ";", stringsAsFactors = FALSE, check.names = FALSE, fileEncoding = "UTF-8"),
    error = function(e) NULL
  )
  if (is.null(df) || !nrow(df)) return(list(ok = FALSE, file = file, data = NULL, applied = FALSE))
  names(df) <- tolower(names(df))
  if (!all(c("date", "libelle", "montant", "type") %in% names(df))) {
    return(list(ok = FALSE, file = file, data = NULL, applied = FALSE))
  }
  df$montant <- abs(suppressWarnings(as.numeric(gsub(",", ".", as.character(df$montant), fixed = TRUE))))
  df$type <- tolower(as.character(df$type))
  df$periode <- format(as.Date(df$date), "%Y-%m")
  list(ok = TRUE, file = file, data = df, applied = FALSE)
}

fpa_cell_calendar_month <- function(cal, periode) {
  out <- list(cap = 0, debt = 0, rows = list(), file = "")
  if (is.null(cal)) return(out)
  out$file <- as.character(cal$file %||% "")
  if (!isTRUE(cal$ok) || is.null(cal$data) || !nrow(cal$data)) return(out)
  sub <- cal$data[as.character(cal$data$periode) == periode, , drop = FALSE]
  if (!nrow(sub)) return(out)
  for (i in seq_len(nrow(sub))) {
    kind <- as.character(sub$type[[i]])
    amt <- suppressWarnings(as.numeric(sub$montant[[i]]))
    if (!is.finite(amt)) next
    if (identical(kind, "capex")) {
      out$cap <- out$cap + amt
      out$rows[[length(out$rows) + 1L]] <- fpa_cell_fac(
        if (length(out$rows)) "+" else "",
        as.character(sub$libelle[[i]]),
        fpa_cell_amt(amt)
      )
    } else if (kind %in% c("dette_tirage", "dette_remboursement")) {
      out$debt <- out$debt + amt
    }
  }
  out
}

#' Indirect cash bridge for one forecast month. Refuses a month that does not tie.
fpa_cash_cell_read <- function(config, periode, paths = fpa_paths(config), line = "cash_close") {
  no <- list(ok = FALSE)
  periode <- as.character(periode %||% "")
  if (!fpa_cell_is_forecast(paths, periode)) return(no)
  cash <- fpa_cell_csv(paths, "cash-bridge-monthly.csv")
  pnl <- fpa_cell_csv(paths, "pnl-monthly.csv")
  bs <- fpa_cell_csv(paths, "bs-monthly.csv")
  row <- fpa_cell_pick(cash, periode)
  prow <- fpa_cell_pick(pnl, periode)
  if (is.null(row) || is.null(prow)) return(no)
  ni <- fpa_cell_at(row, "NI")
  da <- fpa_cell_at(row, "D_A")
  wc <- fpa_cell_at(row, "wc_impact")
  capex <- fpa_cell_at(row, "capex")
  debt <- fpa_cell_at(row, "debt_net")
  delta <- fpa_cell_at(row, "delta_cash")
  open <- fpa_cell_at(row, "cash_open")
  close <- fpa_cell_at(row, "cash_close")
  cfo <- fpa_cell_at(row, "CFO")
  cfi <- fpa_cell_at(row, "CFI")
  cff <- fpa_cell_at(row, "CFF")
  rel <- fpa_cell_at(row, "wc_opening_release")
  if (!is.finite(rel)) rel <- 0
  if (!fpa_cell_tie(delta, ni + da + wc + rel - capex + debt)) return(no)
  if (!fpa_cell_tie(close, open + delta)) return(no)
  if (!fpa_cell_tie(cfo, ni + da + wc + rel)) return(no)
  if (!fpa_cell_tie(cfi, -capex)) return(no)
  if (!fpa_cell_tie(cff, debt)) return(no)
  if (!fpa_cell_tie(ni, fpa_cell_at(prow, "NI"))) return(no)
  if (!fpa_cell_tie(da, fpa_cell_at(prow, "D_A"))) return(no)

  prev_p <- fpa_cell_prev(cash, periode)
  prev_bs <- if (nzchar(prev_p)) fpa_cell_pick(bs, prev_p) else NULL
  bs_row <- fpa_cell_pick(bs, periode)
  wc_factors <- list()
  wc_note <- fpa_txt("cash_read_wc_plain")
  if (!is.null(prev_bs) && !is.null(bs_row)) {
    d_ar <- fpa_cell_at(bs_row, "AR") - fpa_cell_at(prev_bs, "AR")
    d_inv <- fpa_cell_at(bs_row, "INV") - fpa_cell_at(prev_bs, "INV")
    d_ap <- fpa_cell_at(bs_row, "AP") - fpa_cell_at(prev_bs, "AP")
    d_ocl <- fpa_cell_at(bs_row, "OTHER_CL") - fpa_cell_at(prev_bs, "OTHER_CL")
    if (fpa_cell_tie(wc, -d_ar - d_inv + d_ap + d_ocl)) {
      wc_factors <- list(
        fpa_cell_fac("\u2212", "Clients", fpa_cell_amt(d_ar)),
        fpa_cell_fac("\u2212", "Stocks", fpa_cell_amt(d_inv)),
        fpa_cell_fac("+", "Fournisseurs", fpa_cell_amt(d_ap)),
        fpa_cell_fac("+", "Autres dettes", fpa_cell_amt(d_ocl))
      )
    }
  }
  assum_file <- fpa_cell_base(config, "assumptions", paths)
  assum <- tryCatch(fpa_read_assumptions(config, paths), error = function(e) NULL)
  if (!is.null(assum) && isTRUE(assum$ok) && !is.null(bs_row) && nzchar(assum_file)) {
    dso <- tryCatch(fpa_assumption_num(assum$wide, periode, "dso", default = NA_real_), error = function(e) NA_real_)
    dpo <- tryCatch(fpa_assumption_num(assum$wide, periode, "dpo", default = NA_real_), error = function(e) NA_real_)
    dio <- tryCatch(fpa_assumption_num(assum$wide, periode, "dio", default = NA_real_), error = function(e) NA_real_)
    rev <- fpa_cell_at(prow, "REV")
    cogs <- fpa_cell_at(prow, "COGS")
    opex <- fpa_cell_at(prow, "OPEX_OTHER")
    pay <- fpa_cell_at(prow, "PAYROLL")
    md <- fpa_wc_month_days(periode)
    days_ok <- is.finite(md) && md >= 1 &&
      fpa_cell_tie(fpa_cell_at(bs_row, "AR"), rev * dso / md) &&
      fpa_cell_tie(fpa_cell_at(bs_row, "INV"), cogs * dio / md) &&
      fpa_cell_tie(fpa_cell_at(bs_row, "AP"), (cogs + opex) * dpo / md) &&
      fpa_cell_tie(fpa_cell_at(bs_row, "OTHER_CL"), pay * 15 / md)
    if (isTRUE(days_ok)) {
      wc_note <- fpa_txt("cash_read_wc", fpa_cell_days(dso), md, fpa_cell_days(dio), md, fpa_cell_days(dpo), md, md, assum_file)
    }
  }

  month_cal <- fpa_cell_calendar_month(fpa_cell_calendar(config, paths), periode)
  dec_file <- month_cal$file
  cal_cap <- month_cal$cap
  cal_debt <- month_cal$debt
  cap_rows <- month_cal$rows
  capex_gap <- nzchar(dec_file) && !fpa_cell_tie(cal_cap, capex, tol = 0.5)
  capex_factors <- list()
  if (capex_gap) {
    capex_note <- fpa_txt("cash_read_capex_gap", dec_file, fpa_cell_amt(cal_cap), fpa_cell_amt(capex))
  } else if (abs(capex) < 0.5 && abs(cal_cap) < 0.5) {
    capex_note <- fpa_txt("cash_read_capex_none")
  } else if (length(cap_rows) && nzchar(dec_file)) {
    capex_factors <- cap_rows
    capex_note <- fpa_txt("cash_read_capex_lines", dec_file)
  } else {
    capex_note <- ""
  }
  debt_on <- isTRUE(fpa_mechanism_on(config, "debt_schedule"))
  debt_note <- fpa_cell_debt_note(debt, cal_debt, dec_file, debt_on)
  debt_skipped <- !debt_on && cal_debt >= 0.5 && abs(debt) < 0.5

  rows <- list(
    fpa_cell_line("cash_open", "Ouverture", open, note = if (nzchar(prev_p)) fpa_txt("cash_read_open", prev_p) else ""),
    fpa_cell_line("NI", "Résultat net", ni, note = fpa_txt("cash_read_ni")),
    fpa_cell_line("D_A", "Dotations", da, note = fpa_txt("cash_read_da")),
    fpa_cell_line("wc_impact", "BFR", wc, factors = wc_factors, note = wc_note),
    fpa_cell_line("wc_opening_release", fpa_txt("loc_wc_open_release"), rel, note = if (abs(rel) >= 0.5) fpa_txt("wc_open_step") else ""),
    fpa_cell_line("capex", "Investissement", capex, factors = capex_factors, note = capex_note),
    fpa_cell_line("debt_net", "Dette nette", debt, note = debt_note),
    fpa_cell_line(
      "delta_cash", "Variation", delta, role = "sum",
      factors = list(
        fpa_cell_fac("", "Résultat net", fpa_cell_amt(ni)),
        fpa_cell_fac("+", "Dotations", fpa_cell_amt(da)),
        fpa_cell_fac("+", "BFR", fpa_cell_amt(wc)),
        fpa_cell_fac("+", fpa_txt("loc_wc_open_release"), fpa_cell_amt(rel)),
        fpa_cell_fac("\u2212", "Investissement", fpa_cell_amt(capex)),
        fpa_cell_fac("+", "Dette nette", fpa_cell_amt(debt))
      )
    ),
    fpa_cell_line(
      "cash_close", fpa_txt("loc_closing"), close, role = "oi",
      factors = list(
        fpa_cell_fac("", "Ouverture", fpa_cell_amt(open)),
        fpa_cell_fac("+", "Variation", fpa_cell_amt(delta))
      ),
      equation = fpa_txt("cash_read_bridge")
    ),
    fpa_cell_line("flows", fpa_txt("cash_read_flows"), NA_real_, role = "head"),
    fpa_cell_line(
      "CFO", "Flux exploitation", cfo,
      factors = list(
        fpa_cell_fac("", "Résultat net", fpa_cell_amt(ni)),
        fpa_cell_fac("+", "Dotations", fpa_cell_amt(da)),
        fpa_cell_fac("+", "BFR", fpa_cell_amt(wc))
      )
    ),
    fpa_cell_line("CFI", "Flux investissement", cfi, equation = fpa_txt("cash_read_cfi")),
    fpa_cell_line("CFF", "Flux financement", cff, equation = fpa_txt("cash_read_cff"))
  )
  ids <- vapply(rows, function(r) r$id, character(1))
  focus <- as.character(line %||% "")
  if (!focus %in% ids || identical(focus, "flows")) focus <- "cash_close"
  hit <- rows[[match(focus, ids)]]
  files <- unique(c(assum_file, dec_file))
  files <- files[nzchar(files)]
  list(
    ok = TRUE, kind = "cash", periode = periode, focus = focus,
    focus_label = hit$label, screen = fpa_cell_screen(hit$amount),
    precise = fpa_cell_amt(hit$amount, digits = 2),
    files = files, rows = rows,
    capex_gap = capex_gap, debt_skipped = debt_skipped,
    close = close, delta = delta, wc = wc
  )
}

fpa_cell_bs_name <- function(id) {
  switch(as.character(id),
    CASH = "Trésorerie", AR = "Clients", INV = "Stocks", PPE = "Immobilisations",
    OTHER_CA = "Autres actifs", AP = "Fournisseurs", ST_DEBT = "Dette court terme",
    LT_DEBT = "Dette long terme", OTHER_CL = "Autres dettes", EQUITY = "Capitaux propres",
    as.character(id)
  )
}

#' Balance sheet for one forecast month. Assets must equal liabilities and equity.
fpa_bs_cell_read <- function(config, periode, paths = fpa_paths(config), line = "assets") {
  no <- list(ok = FALSE)
  periode <- as.character(periode %||% "")
  if (!fpa_cell_is_forecast(paths, periode)) return(no)
  bs <- fpa_cell_csv(paths, "bs-monthly.csv")
  pnl <- fpa_cell_csv(paths, "pnl-monthly.csv")
  cash <- fpa_cell_csv(paths, "cash-bridge-monthly.csv")
  row <- fpa_cell_pick(bs, periode)
  prow <- fpa_cell_pick(pnl, periode)
  crow <- fpa_cell_pick(cash, periode)
  if (is.null(row) || is.null(prow) || is.null(crow)) return(no)
  at <- function(id) fpa_cell_at(row, id)
  assets <- at("CASH") + at("AR") + at("INV") + at("PPE") + at("OTHER_CA")
  claims <- at("AP") + at("ST_DEBT") + at("LT_DEBT") + at("OTHER_CL") + at("EQUITY")
  if (!is.finite(assets) || abs(assets - claims) > 1) return(no)
  if (!fpa_cell_tie(at("CASH"), fpa_cell_at(crow, "cash_close"))) return(no)

  prev_p <- fpa_cell_prev(bs, periode)
  prev <- if (nzchar(prev_p)) fpa_cell_pick(bs, prev_p) else NULL
  capex <- fpa_cell_at(crow, "capex")
  da <- fpa_cell_at(crow, "D_A")
  ni <- fpa_cell_at(crow, "NI")
  if (!is.null(prev)) {
    if (!fpa_cell_tie(at("PPE"), fpa_cell_at(prev, "PPE") + capex - da)) return(no)
    if (!fpa_cell_tie(at("EQUITY"), fpa_cell_at(prev, "EQUITY") + ni)) return(no)
  }

  assum_file <- fpa_cell_base(config, "assumptions", paths)
  open_file <- fpa_cell_base(config, "opening_bs", paths)
  assum <- tryCatch(fpa_read_assumptions(config, paths), error = function(e) NULL)
  day <- function(id) {
    if (is.null(assum) || !isTRUE(assum$ok)) return(NA_real_)
    tryCatch(fpa_assumption_num(assum$wide, periode, id, default = NA_real_), error = function(e) NA_real_)
  }
  dso <- day("dso")
  dpo <- day("dpo")
  dio <- day("dio")
  rev <- fpa_cell_at(prow, "REV")
  cogs <- fpa_cell_at(prow, "COGS")
  opex <- fpa_cell_at(prow, "OPEX_OTHER")
  pay <- fpa_cell_at(prow, "PAYROLL")
  md <- fpa_wc_month_days(periode)
  ar_ok <- is.finite(md) && fpa_cell_tie(at("AR"), rev * dso / md)
  inv_ok <- is.finite(md) && fpa_cell_tie(at("INV"), cogs * dio / md)
  ap_ok <- is.finite(md) && fpa_cell_tie(at("AP"), (cogs + opex) * dpo / md)
  ocl_ok <- is.finite(md) && fpa_cell_tie(at("OTHER_CL"), pay * 15 / md)

  month_cal <- fpa_cell_calendar_month(fpa_cell_calendar(config, paths), periode)
  dec_file <- month_cal$file
  cal_cap <- month_cal$cap
  cal_debt <- month_cal$debt
  capex_gap <- nzchar(dec_file) && !fpa_cell_tie(cal_cap, capex, tol = 0.5)
  debt_on <- isTRUE(fpa_mechanism_on(config, "debt_schedule"))
  debt_note <- fpa_cell_debt_note(fpa_cell_at(crow, "debt_net"), cal_debt, dec_file, debt_on)

  shown <- function(id) is.finite(at(id)) && abs(at(id)) >= 0.5
  fac_sum <- function(ids) {
    out <- list()
    for (id in ids) {
      if (!shown(id) && !identical(id, "CASH")) next
      out[[length(out) + 1L]] <- fpa_cell_fac(
        if (length(out)) "+" else "",
        fpa_cell_bs_name(id),
        fpa_cell_amt(at(id))
      )
    }
    out
  }
  ppe_note <- if (nzchar(prev_p)) fpa_txt("bs_read_ppe", prev_p) else ""
  if (capex_gap) {
    extra <- fpa_txt("cash_read_capex_gap", dec_file, fpa_cell_amt(cal_cap), fpa_cell_amt(capex))
    ppe_note <- paste(ppe_note, extra)
  }
  ppe_factors <- if (!is.null(prev)) {
    list(
      fpa_cell_fac("", prev_p, fpa_cell_amt(fpa_cell_at(prev, "PPE"))),
      fpa_cell_fac("+", "Investissement", fpa_cell_amt(capex)),
      fpa_cell_fac("\u2212", "Dotations", fpa_cell_amt(da))
    )
  } else {
    list()
  }
  eq_factors <- if (!is.null(prev)) {
    list(
      fpa_cell_fac("", prev_p, fpa_cell_amt(fpa_cell_at(prev, "EQUITY"))),
      fpa_cell_fac("+", "Résultat net", fpa_cell_amt(ni))
    )
  } else {
    list()
  }
  hold_note <- function(base) {
    note <- if (nzchar(open_file)) fpa_txt(base, open_file) else ""
    if (nzchar(debt_note)) note <- paste(note, debt_note)
    trimws(note)
  }
  rows <- list(
    fpa_cell_line("h_assets", fpa_txt("bs_read_assets"), NA_real_, role = "head"),
    fpa_cell_line("CASH", fpa_cell_bs_name("CASH"), at("CASH"), note = fpa_txt("bs_read_cash")),
    if (ar_ok && nzchar(assum_file)) {
      fpa_cell_line(
        "AR", fpa_cell_bs_name("AR"), at("AR"),
        factors = list(
          fpa_cell_fac("", "Chiffre d'affaires", fpa_cell_amt(rev)),
          fpa_cell_fac("\u00d7", fpa_txt("loc_delay_word"), paste0(fpa_cell_days(dso), " j")),
          fpa_cell_fac("/", fpa_txt("loc_year_word"), "365")
        ),
        note = fpa_txt("bs_read_ar", assum_file)
      )
    } else if (shown("AR")) fpa_cell_line("AR", fpa_cell_bs_name("AR"), at("AR")),
    if (inv_ok && nzchar(assum_file)) {
      fpa_cell_line(
        "INV", fpa_cell_bs_name("INV"), at("INV"),
        factors = list(
          fpa_cell_fac("", "Achats", fpa_cell_amt(cogs)),
          fpa_cell_fac("\u00d7", fpa_txt("loc_delay_word"), paste0(fpa_cell_days(dio), " j")),
          fpa_cell_fac("/", fpa_txt("loc_year_word"), "365")
        ),
        note = fpa_txt("bs_read_inv", assum_file)
      )
    } else if (shown("INV")) fpa_cell_line("INV", fpa_cell_bs_name("INV"), at("INV")),
    if (!is.null(prev) || shown("PPE")) {
      fpa_cell_line("PPE", fpa_cell_bs_name("PPE"), at("PPE"), factors = ppe_factors, note = ppe_note)
    },
    if (shown("OTHER_CA")) {
      fpa_cell_line("OTHER_CA", fpa_cell_bs_name("OTHER_CA"), at("OTHER_CA"),
        note = if (nzchar(open_file)) fpa_txt("bs_read_oca", open_file) else "")
    },
    fpa_cell_line("assets", fpa_txt("bs_read_tot_assets"), assets, role = "sum", factors = fac_sum(c("CASH", "AR", "INV", "PPE", "OTHER_CA"))),
    fpa_cell_line("h_claims", fpa_txt("bs_read_claims"), NA_real_, role = "head"),
    if (ap_ok && nzchar(assum_file)) {
      fpa_cell_line(
        "AP", fpa_cell_bs_name("AP"), at("AP"),
        factors = list(
          fpa_cell_fac("", "Achats et autres charges", fpa_cell_amt(cogs + opex)),
          fpa_cell_fac("\u00d7", fpa_txt("loc_delay_word"), paste0(fpa_cell_days(dpo), " j")),
          fpa_cell_fac("/", fpa_txt("loc_year_word"), "365")
        ),
        note = fpa_txt("bs_read_ap", assum_file)
      )
    } else if (shown("AP")) fpa_cell_line("AP", fpa_cell_bs_name("AP"), at("AP")),
    if (shown("ST_DEBT")) {
      fpa_cell_line("ST_DEBT", fpa_cell_bs_name("ST_DEBT"), at("ST_DEBT"), note = hold_note("bs_read_hold"))
    },
    if (shown("LT_DEBT") || nzchar(debt_note)) {
      fpa_cell_line("LT_DEBT", fpa_cell_bs_name("LT_DEBT"), at("LT_DEBT"), note = hold_note("bs_read_hold"))
    },
    if (ocl_ok) {
      fpa_cell_line(
        "OTHER_CL", fpa_cell_bs_name("OTHER_CL"), at("OTHER_CL"),
        factors = list(
          fpa_cell_fac("", "Masse salariale", fpa_cell_amt(pay)),
          fpa_cell_fac("\u00d7", fpa_txt("loc_delay_word"), "15 j"),
          fpa_cell_fac("/", fpa_txt("loc_year_word"), "365")
        ),
        note = fpa_txt("bs_read_ocl")
      )
    } else if (shown("OTHER_CL")) fpa_cell_line("OTHER_CL", fpa_cell_bs_name("OTHER_CL"), at("OTHER_CL")),
    if (!is.null(prev) || shown("EQUITY")) {
      fpa_cell_line(
        "EQUITY", fpa_cell_bs_name("EQUITY"), at("EQUITY"),
        factors = eq_factors,
        note = if (nzchar(prev_p)) fpa_txt("bs_read_eq", prev_p) else ""
      )
    },
    fpa_cell_line(
      "claims", fpa_txt("bs_read_tot_claims"), claims, role = "sum",
      factors = fac_sum(c("AP", "ST_DEBT", "LT_DEBT", "OTHER_CL", "EQUITY")),
      note = fpa_txt("bs_read_identity", fpa_cell_amt(assets), fpa_cell_amt(claims), fpa_cell_amt(assets - claims))
    )
  )
  rows <- rows[!vapply(rows, is.null, logical(1))]
  ids <- vapply(rows, function(r) r$id, character(1))
  focus <- as.character(line %||% "")
  if (!focus %in% ids || grepl("^h_", focus)) focus <- "assets"
  hit <- rows[[match(focus, ids)]]
  files <- unique(c(assum_file, dec_file, open_file))
  files <- files[nzchar(files)]
  list(
    ok = TRUE, kind = "bs", periode = periode, focus = focus,
    focus_label = hit$label, screen = fpa_cell_screen(hit$amount),
    precise = fpa_cell_amt(hit$amount, digits = 2),
    files = files, rows = rows,
    assets = assets, claims = claims, gap = assets - claims,
    capex_gap = capex_gap
  )
}

