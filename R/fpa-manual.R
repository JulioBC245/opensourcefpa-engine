# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-manual.R — Dated menu entries that move cash / P&L / BFR (saisies-manuelles.csv)
# LAST CHANGE: 2026-10-13
# Convention path next to the config (not a config key). JSON keeps comments only.

FPA_MANUAL_NATURES <- c("decalage", "nouveau")
FPA_MANUAL_SENS <- c("in", "out")
FPA_MANUAL_STATUTS <- c("actif", "retire")
FPA_MANUAL_LIGNES <- c(
  "OTHER_OP_INC", "OPEX_OTHER", "REV", "COGS", "PAYROLL", "FIN", "EXCEP"
)

#' Path of the dated ledger. Lives under input/ next to fpa-config.json.
fpa_manual_path <- function(config) {
  root <- dirname(fpa_local_overlay_path(config))
  file.path(root, "input", "saisies-manuelles.csv")
}

#' On/off for the whole ledger. Missing file means the lines are in the calculation.
#' Not a config key: site sync rewrites fpa-config.json.
fpa_manual_apply_path <- function(config) {
  file.path(dirname(fpa_manual_path(config)), "saisies-appliquees.txt")
}

#' 2% of the cash the year generates, rounded to the nearest 100, never under 500.
fpa_manual_floor <- function(cash) {
  floor_eur <- 500L
  if (is.null(cash) || !nrow(cash) || !all(c("cash_open", "cash_close") %in% names(cash))) return(floor_eur)
  open <- suppressWarnings(as.numeric(cash$cash_open[[1]]))
  close <- suppressWarnings(as.numeric(cash$cash_close[[nrow(cash)]]))
  net <- if (is.finite(open) && is.finite(close)) abs(close - open) else NA_real_
  if (!is.finite(net) || net <= 0) return(floor_eur)
  stepped <- round(0.02 * net / 100) * 100
  as.integer(max(floor_eur, stepped))
}

fpa_manual_applied <- function(config) {
  path <- fpa_manual_apply_path(config)
  if (!file.exists(path)) return(TRUE)
  raw <- readLines(path, n = 1L, warn = FALSE, encoding = "UTF-8")
  if (!length(raw)) return(TRUE)
  !identical(tolower(trimws(raw[[1]])), "off")
}

fpa_manual_set_applied <- function(config, on) {
  path <- fpa_manual_apply_path(config)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  tmp <- paste0(path, ".tmp")
  writeLines(if (isTRUE(on)) "on" else "off", tmp, useBytes = TRUE)
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
  invisible(isTRUE(on))
}

fpa_manual_id <- function() {
  paste0("m", format(Sys.time(), "%Y%m%d%H%M%S"), sprintf("%03d", sample.int(999L, 1L)))
}

fpa_manual_empty <- function() {
  data.frame(
    id = character(),
    saisi_le = character(),
    nature = character(),
    sens = character(),
    montant = numeric(),
    date_effet = as.Date(character()),
    date_prevue = as.Date(character()),
    ligne = character(),
    quantite = numeric(),
    unite = character(),
    libelle = character(),
    statut = character(),
    journal_id = character(),
    stringsAsFactors = FALSE
  )
}

fpa_manual_parse_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  raw <- as.character(x %||% "")
  if (!length(raw) || !nzchar(raw[[1]]) || identical(raw[[1]], "NA")) return(as.Date(NA))
  suppressWarnings(as.Date(raw[[1]]))
}

fpa_manual_ym <- function(d) {
  d <- fpa_manual_parse_date(d)
  if (!is.finite(d)) return("")
  format(d, "%Y-%m")
}

#' Default P&L line for a new cash item.
fpa_manual_default_ligne <- function(sens) {
  if (identical(as.character(sens), "in")) "OTHER_OP_INC" else "OPEX_OTHER"
}

fpa_manual_frame <- function(df) {
  if (is.null(df) || !ncol(df)) return(NULL)
  names(df) <- tolower(trimws(names(df)))
  need <- c("id", "nature", "sens", "montant", "date_effet", "statut")
  if (!all(need %in% names(df))) return(NULL)
  empty <- fpa_manual_empty()
  for (nm in names(empty)) {
    if (!(nm %in% names(df))) df[[nm]] <- if (nm %in% c("montant", "quantite")) NA_real_ else ""
  }
  df
}

fpa_manual_normalize_row <- function(row) {
  if (is.null(row)) return(NULL)
  id <- as.character(row$id %||% "")
  if (!nzchar(id)) id <- fpa_manual_id()
  nature <- tolower(trimws(as.character(row$nature %||% "")))
  sens <- tolower(trimws(as.character(row$sens %||% "")))
  montant <- abs(suppressWarnings(as.numeric(gsub(",", ".", as.character(row$montant %||% ""), fixed = TRUE))))
  date_effet <- fpa_manual_parse_date(row$date_effet)
  date_prevue <- fpa_manual_parse_date(row$date_prevue)
  ligne <- toupper(trimws(as.character(row$ligne %||% "")))
  if (!nzchar(ligne)) ligne <- fpa_manual_default_ligne(sens)
  statut <- tolower(trimws(as.character(row$statut %||% "actif")))
  if (!statut %in% FPA_MANUAL_STATUTS) statut <- "actif"
  quantite <- suppressWarnings(as.numeric(gsub(",", ".", as.character(row$quantite %||% ""), fixed = TRUE)))
  unite <- as.character(row$unite %||% "")
  libelle <- trimws(as.character(row$libelle %||% ""))
  journal_id <- as.character(row$journal_id %||% "")
  saisi_le <- as.character(row$saisi_le %||% format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  list(
    id = id,
    saisi_le = saisi_le,
    nature = nature,
    sens = sens,
    montant = montant,
    date_effet = date_effet,
    date_prevue = date_prevue,
    ligne = ligne,
    quantite = if (is.finite(quantite)) quantite else NA_real_,
    unite = unite,
    libelle = libelle,
    statut = statut,
    journal_id = journal_id
  )
}

fpa_manual_as_df <- function(rows) {
  empty <- fpa_manual_empty()
  if (!length(rows)) return(empty)
  parts <- lapply(rows, function(r) {
    n <- fpa_manual_normalize_row(r)
    if (is.null(n)) return(NULL)
    data.frame(
      id = n$id,
      saisi_le = n$saisi_le,
      nature = n$nature,
      sens = n$sens,
      montant = n$montant,
      date_effet = n$date_effet,
      date_prevue = n$date_prevue,
      ligne = n$ligne,
      quantite = n$quantite,
      unite = n$unite,
      libelle = n$libelle,
      statut = n$statut,
      journal_id = n$journal_id,
      stringsAsFactors = FALSE
    )
  })
  parts <- Filter(Negate(is.null), parts)
  if (!length(parts)) return(empty)
  do.call(rbind, parts)
}

#' Read the ledger. Missing file is an empty ok ledger (engine stays identical).
fpa_manual_read <- function(config) {
  path <- fpa_manual_path(config)
  if (!file.exists(path)) {
    return(list(ok = TRUE, path = path, data = fpa_manual_empty(), error = NULL))
  }
  raw <- tryCatch(
    utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE,
      fileEncoding = "UTF-8", na.strings = c("", "NA")
    ),
    error = function(e) NULL
  )
  if (is.null(raw)) {
    raw <- tryCatch(
      utils::read.csv(
        path, sep = ";", stringsAsFactors = FALSE, check.names = FALSE,
        fileEncoding = "UTF-8", na.strings = c("", "NA")
      ),
      error = function(e) NULL
    )
  }
  df <- fpa_manual_frame(raw)
  if (is.null(df)) {
    return(list(
      ok = FALSE, path = path, data = fpa_manual_empty(), error = "schema",
      message = "saisies-manuelles.csv: colonnes id, nature, sens, montant, date_effet, statut."
    ))
  }
  rows <- lapply(seq_len(nrow(df)), function(i) as.list(df[i, , drop = FALSE]))
  list(ok = TRUE, path = path, data = fpa_manual_as_df(rows), error = NULL)
}

#' Atomic write. Comma separator, UTF-8.
fpa_manual_write <- function(config, df) {
  path <- fpa_manual_path(config)
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
  frame <- if (is.data.frame(df)) df else fpa_manual_as_df(df)
  if (!nrow(frame)) frame <- fpa_manual_empty()
  # Dates as ISO strings for a stable CSV
  out <- frame
  out$date_effet <- ifelse(is.na(out$date_effet), "", as.character(out$date_effet))
  out$date_prevue <- ifelse(is.na(out$date_prevue), "", as.character(out$date_prevue))
  out$quantite <- ifelse(is.na(out$quantite), "", as.character(out$quantite))
  tmp <- paste0(path, ".tmp")
  utils::write.csv(out, tmp, row.names = FALSE, fileEncoding = "UTF-8", na = "")
  if (file.exists(path)) file.remove(path)
  file.rename(tmp, path)
  invisible(fpa_manual_read(config))
}

#' Active rows only (statut = actif).
fpa_manual_active <- function(df) {
  if (is.null(df) || !nrow(df)) return(fpa_manual_empty())
  df[as.character(df$statut) == "actif", , drop = FALSE]
}

#' One row per entry: used, or excluded with a reason.
#' Closed-month rule: nouveau / either date of a decalage in a closed month → excluded.
fpa_manual_ledger <- function(df, periods, actual_periods = character()) {
  empty <- data.frame(
    id = character(), libelle = character(), nature = character(), sens = character(),
    periode = character(), state = character(), reason = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(df) || !nrow(df)) return(empty)
  periods <- as.character(periods %||% character(0))
  closed <- as.character(actual_periods %||% character(0))
  rows <- vector("list", nrow(df))
  for (i in seq_len(nrow(df))) {
    id <- as.character(df$id[[i]] %||% "")
    lib <- as.character(df$libelle[[i]] %||% "")
    nature <- as.character(df$nature[[i]] %||% "")
    sens <- as.character(df$sens[[i]] %||% "")
    statut <- as.character(df$statut[[i]] %||% "")
    amt <- suppressWarnings(as.numeric(df$montant[[i]]))
    d_eff <- fpa_manual_parse_date(df$date_effet[[i]])
    d_prev <- fpa_manual_parse_date(df$date_prevue[[i]])
    per_eff <- fpa_manual_ym(d_eff)
    per_prev <- fpa_manual_ym(d_prev)
    ligne <- as.character(df$ligne[[i]] %||% "")
    reason <- if (identical(statut, "retire")) {
      "retire"
    } else if (!nature %in% FPA_MANUAL_NATURES) {
      "nature inconnue"
    } else if (!sens %in% FPA_MANUAL_SENS) {
      "sens inconnu"
    } else if (!is.finite(amt) || amt <= 0) {
      "montant illisible"
    } else if (!is.finite(d_eff)) {
      "date_effet illisible"
    } else if (identical(nature, "decalage") && !is.finite(d_prev)) {
      "date_prevue manquante"
    } else if (identical(nature, "decalage") && identical(per_eff, per_prev)) {
      fpa_txt("loc_same_month_no_effect")
    } else if (identical(nature, "nouveau") && nzchar(ligne) && !ligne %in% FPA_MANUAL_LIGNES) {
      "ligne inconnue"
    } else if (length(periods) && !per_eff %in% periods &&
               !(identical(nature, "decalage") && per_prev %in% periods)) {
      "hors horizon"
    } else if (length(closed) && identical(nature, "nouveau") && per_eff %in% closed) {
      paste0("mois clos (", per_eff, fpa_txt("loc_first_open_required"))
    } else if (length(closed) && identical(nature, "decalage") &&
               (per_eff %in% closed || per_prev %in% closed)) {
      paste0(
        "mois clos (",
        paste(unique(c(per_prev, per_eff)[c(per_prev, per_eff) %in% closed]), collapse = ", "),
        fpa_txt("loc_first_open_required")
      )
    } else {
      ""
    }
    rows[[i]] <- data.frame(
      id = id, libelle = lib, nature = nature, sens = sens,
      periode = per_eff,
      state = if (nzchar(reason)) "excluded" else "used",
      reason = reason, stringsAsFactors = FALSE
    )
  }
  do.call(rbind, rows)
}

#' Rows the engine applies (used state).
fpa_manual_used <- function(df, periods, actual_periods = character()) {
  led <- fpa_manual_ledger(df, periods, actual_periods)
  if (!nrow(led) || is.null(df) || !nrow(df)) return(fpa_manual_empty())
  keep <- led$id[led$state == "used"]
  df[as.character(df$id) %in% keep, , drop = FALSE]
}

#' Apply nouveau entries onto the P&L after WC is built.
#' Marginal tax only: keeps the month's existing TAX and adds rate × change in
#' taxable base, so a prior forecast tax figure is not wiped.
fpa_manual_apply_nouveau <- function(pnl, used, assum_wide = NULL, config = NULL) {
  if (is.null(pnl) || !nrow(pnl) || is.null(used) || !nrow(used)) return(pnl)
  nov <- used[as.character(used$nature) == "nouveau", , drop = FALSE]
  if (!nrow(nov)) return(pnl)
  touched <- character()
  tax_on <- !is.null(config) && fpa_mechanism_on(config, "effective_tax_rate")
  income_lines <- c("OTHER_OP_INC", "REV")
  for (i in seq_len(nrow(nov))) {
    per <- fpa_manual_ym(nov$date_effet[[i]])
    ix <- match(per, as.character(pnl$periode))
    if (is.na(ix)) next
    ligne <- as.character(nov$ligne[[i]])
    if (!ligne %in% names(pnl) || !ligne %in% FPA_MANUAL_LIGNES) next
    amt <- abs(as.numeric(nov$montant[[i]]))
    if (!is.finite(amt) || amt <= 0) next
    # Income lines: add on "in", subtract on "out". Expense lines: opposite.
    if (ligne %in% income_lines) {
      delta <- if (identical(as.character(nov$sens[[i]]), "in")) amt else -amt
    } else {
      delta <- if (identical(as.character(nov$sens[[i]]), "out")) amt else -amt
    }
    oi_before <- pnl$OI[[ix]] %||% 0
    fin <- pnl$FIN[[ix]] %||% 0
    excep <- pnl$EXCEP[[ix]] %||% 0
    if (!is.finite(oi_before)) oi_before <- 0
    if (!is.finite(fin)) fin <- 0
    if (!is.finite(excep)) excep <- 0
    base_before <- max(oi_before - fin - excep, 0)
    pnl[[ligne]][[ix]] <- (pnl[[ligne]][[ix]] %||% 0) + delta
    # Local re-derive for this row so OI reflects the line move before tax.
    pnl$GP[[ix]] <- pnl$REV[[ix]] + pnl$OTHER_OP_INC[[ix]] - pnl$COGS[[ix]]
    pnl$EBITDA[[ix]] <- pnl$GP[[ix]] - pnl$PAYROLL[[ix]] - pnl$OPEX_OTHER[[ix]]
    pnl$OI[[ix]] <- pnl$EBITDA[[ix]] - pnl$D_A[[ix]]
    oi_after <- pnl$OI[[ix]] %||% 0
    if (!is.finite(oi_after)) oi_after <- 0
    base_after <- max(oi_after - fin - excep, 0)
    if (tax_on) {
      rate <- tryCatch(
        fpa_assumption_num(assum_wide, per, "taux.impot", default = 0.25),
        error = function(e) 0.25
      )
      if (!is.finite(rate)) rate <- 0.25
      tax_delta <- rate * (base_after - base_before)
      pnl$TAX[[ix]] <- (pnl$TAX[[ix]] %||% 0) + tax_delta
    }
    pnl$NI[[ix]] <- pnl$OI[[ix]] - pnl$FIN[[ix]] - pnl$EXCEP[[ix]] - pnl$TAX[[ix]]
    touched <- c(touched, paste(per, ligne, sep = "|"))
  }
  attr(pnl, "manual_touched") <- unique(touched)
  pnl
}

#' Timing shifts: hold AR (in) or AP (out) between date_prevue and date_effet.
#' Cash moves via the existing wc_impact path. NI is unchanged.
fpa_manual_apply_decalage <- function(wc, used, periods) {
  if (is.null(wc) || is.null(used) || !nrow(used) || !length(periods)) return(wc)
  dec <- used[as.character(used$nature) == "decalage", , drop = FALSE]
  if (!nrow(dec)) return(wc)
  periods <- as.character(periods)
  for (i in seq_len(nrow(dec))) {
    amt <- abs(as.numeric(dec$montant[[i]]))
    if (!is.finite(amt) || amt <= 0) next
    per_eff <- fpa_manual_ym(dec$date_effet[[i]])
    per_prev <- fpa_manual_ym(dec$date_prevue[[i]])
    if (!nzchar(per_eff) || !nzchar(per_prev) || identical(per_eff, per_prev)) next
    i_eff <- match(per_eff, periods)
    i_prev <- match(per_prev, periods)
    if (is.na(i_eff) || is.na(i_prev)) next
    # Hold the receivable/payable from the earlier month through the month before the later one.
    lo <- min(i_prev, i_eff)
    hi <- max(i_prev, i_eff)
    hold <- periods[lo:(hi - 1L)]
    # Early payment (effet before prevue): AR/AP lower during the hold (cash already moved).
    # Late payment (effet after prevue): AR/AP higher during the hold (cash not yet moved).
    late <- i_eff > i_prev
    sign <- if (late) 1 else -1
    if (identical(as.character(dec$sens[[i]]), "in")) {
      for (p in hold) {
        wc$AR[[p]] <- (wc$AR[[p]] %||% 0) + sign * amt
      }
    } else {
      for (p in hold) {
        wc$AP[[p]] <- (wc$AP[[p]] %||% 0) + sign * amt
      }
    }
  }
  wc
}

#' Periods/lines touched by used nouveau entries (for evidence labels).
fpa_manual_touched_keys <- function(used) {
  if (is.null(used) || !nrow(used)) return(character(0))
  nov <- used[as.character(used$nature) == "nouveau", , drop = FALSE]
  if (!nrow(nov)) return(character(0))
  paste(fpa_manual_ym(nov$date_effet), as.character(nov$ligne), sep = "|")
}

#' Append or update one entry. Retire by id with statut = retire.
fpa_manual_upsert <- function(config, entry) {
  cur <- fpa_manual_read(config)
  df <- cur$data
  n <- fpa_manual_normalize_row(entry)
  if (is.null(n)) return(cur)
  if (!nrow(df)) {
    return(fpa_manual_write(config, fpa_manual_as_df(list(n))))
  }
  hit <- which(as.character(df$id) == n$id)
  row <- fpa_manual_as_df(list(n))
  if (length(hit)) {
    df[hit[[1]], ] <- row[1, ]
  } else {
    df <- rbind(df, row)
  }
  fpa_manual_write(config, df)
}

fpa_manual_retire <- function(config, id) {
  cur <- fpa_manual_read(config)
  df <- cur$data
  if (!nrow(df)) return(cur)
  hit <- which(as.character(df$id) == as.character(id))
  if (!length(hit)) return(cur)
  df$statut[hit] <- "retire"
  fpa_manual_write(config, df)
}

#' Migrate legacy cash-outlook.json items into the ledger once.
#' Returns the number of rows added. Leaves as_of / opening on the JSON file.
fpa_manual_migrate_outlook <- function(config) {
  out <- fpa_outlook_read(config)
  items <- out$items %||% list()
  if (!length(items)) return(0L)
  cur <- fpa_manual_read(config)
  existing_libs <- if (nrow(cur$data)) {
    paste(cur$data$libelle, as.character(cur$data$date_effet), cur$data$montant)
  } else {
    character(0)
  }
  added <- 0L
  for (it in items) {
    row <- fpa_outlook_normalize_item(it)
    if (is.null(row)) next
    key <- paste(row$label, row$date, row$amount)
    if (key %in% existing_libs) next
    entry <- list(
      id = fpa_manual_id(),
      saisi_le = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      nature = "nouveau",
      sens = row$kind,
      montant = row$amount,
      date_effet = row$date,
      date_prevue = "",
      ligne = fpa_manual_default_ligne(row$kind),
      quantite = NA_real_,
      unite = "",
      libelle = row$label,
      statut = "actif",
      journal_id = ""
    )
    cur <- fpa_manual_upsert(config, entry)
    added <- added + 1L
  }
  if (added > 0L) {
    # Clear items from outlook JSON so one movement is not stored twice.
    fpa_outlook_write(config, list(
      as_of = out$as_of,
      opening = out$opening,
      items = list()
    ))
  }
  added
}

#' Ledger rows that fall inside the four-week outlook window, as outlook items.
fpa_manual_outlook_items <- function(config, as_of, n = 4L) {
  as_of <- fpa_outlook_date(as_of)
  weeks <- fpa_outlook_weeks(as_of, n)
  if (!length(weeks) || !is.finite(as_of)) return(list())
  end <- weeks[[length(weeks)]]$end
  cur <- fpa_manual_read(config)
  df <- fpa_manual_active(cur$data)
  if (!nrow(df)) return(list())
  items <- list()
  for (i in seq_len(nrow(df))) {
    d <- fpa_manual_parse_date(df$date_effet[[i]])
    if (!is.finite(d) || d < as_of || d > end) next
    items[[length(items) + 1L]] <- list(
      id = as.character(df$id[[i]]),
      kind = as.character(df$sens[[i]]),
      label = as.character(df$libelle[[i]]),
      amount = as.numeric(df$montant[[i]]),
      date = as.character(d),
      uncertain = FALSE,
      nature = as.character(df$nature[[i]]),
      ligne = as.character(df$ligne[[i]])
    )
  }
  items
}

#' Before/after acknowledgement from two engine runs.
fpa_manual_ack <- function(before, after, ye_month = NULL) {
  cash_b <- before$cash
  cash_a <- after$cash
  pnl_b <- before$pnl
  pnl_a <- after$pnl
  pick_close <- function(cash, ym) {
    if (is.null(cash) || !nrow(cash)) return(NA_real_)
    if (nzchar(ym %||% "")) {
      i <- match(ym, as.character(cash$periode))
      if (!is.na(i)) return(suppressWarnings(as.numeric(cash$cash_close[[i]])))
    }
    suppressWarnings(as.numeric(cash$cash_close[[nrow(cash)]]))
  }
  ym <- as.character(ye_month %||% "")
  if (!nzchar(ym) && !is.null(cash_a) && nrow(cash_a)) ym <- as.character(cash_a$periode[[nrow(cash_a)]])
  close_b <- pick_close(cash_b, ym)
  close_a <- pick_close(cash_a, ym)
  ni_b <- if (is.null(pnl_b) || !nrow(pnl_b)) NA_real_ else sum(suppressWarnings(as.numeric(pnl_b$NI)), na.rm = TRUE)
  ni_a <- if (is.null(pnl_a) || !nrow(pnl_a)) NA_real_ else sum(suppressWarnings(as.numeric(pnl_a$NI)), na.rm = TRUE)
  list(
    ye_month = ym,
    cash_before = close_b,
    cash_after = close_a,
    cash_delta = if (is.finite(close_b) && is.finite(close_a)) close_a - close_b else NA_real_,
    ni_before = ni_b,
    ni_after = ni_a,
    ni_delta = if (is.finite(ni_b) && is.finite(ni_a)) ni_a - ni_b else NA_real_,
    ni_unchanged = is.finite(ni_b) && is.finite(ni_a) && abs(ni_a - ni_b) < 0.5
  )
}
