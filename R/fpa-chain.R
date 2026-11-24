# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-chain.R — What the config connects, and what the last run used.
# LAST CHANGE: 2026-10-12 — budget year retarget (exercice N, N-1 moves with N)
# A link exists only when a mechanism is on and its columns or keys are present,
# or when a statement file is mapped straight onto an output. A custom file
# with no such link stays visible and unconnected.

fpa_chain_catalog <- function() {
  list(
    list(id = "capacity_x_util_x_rate", label = fpa_txt("loc_mech_capacity"),
         role = "ops_other", needs = c("capacite", "taux_utilisation", "tarif"),
         keys = character(), line_id = "REV", line = "Chiffre d'affaires", out = "pnl"),
    list(id = "qty_x_price", label = fpa_txt("match_mech_qty"),
         role = "ops_other", needs = c("quantite", "prix"),
         keys = character(), line_id = "REV", line = "Chiffre d'affaires", out = "pnl"),
    list(id = "customers_x_spend", label = fpa_txt("match_mech_customers"),
         role = "ops_other", needs = c("nb_clients", "depense_moyenne"),
         keys = character(), line_id = "REV", line = "Chiffre d'affaires", out = "pnl"),
    list(id = "revenue_growth_pct", label = fpa_txt("match_mech_growth"),
         role = "assumptions", needs = character(), keys = "croissance.ca",
         line_id = "REV", line = "Chiffre d'affaires", out = "pnl"),
    list(id = "headcount_x_cost", label = fpa_txt("loc_mech_staff"),
         role = "ops_other", needs = c("effectif", "cout_moyen"),
         keys = character(), line_id = "PAYROLL", line = "Masse salariale", out = "pnl"),
    list(id = "variable_cost_pct_or_unit", label = fpa_txt("match_lever_cout_variable_pct"),
         role = "assumptions", needs = character(), keys = "cout.variable",
         line_id = "COGS", line = "Achats", out = "pnl"),
    list(id = "fixed_monthly_expense", label = fpa_txt("match_lever_charges_fixes"),
         role = "assumptions", needs = character(), keys = "charge.fixe",
         line_id = "OPEX_OTHER", line = "Autres charges", out = "pnl"),
    list(id = "effective_tax_rate", label = fpa_txt("match_mech_tax"),
         role = "assumptions", needs = character(), keys = "taux.impot",
         line_id = "TAX", line = fpa_txt("loc_tax"), out = "pnl"),
    list(id = "dso_dpo_dio", label = fpa_txt("loc_mech_days"),
         role = "assumptions", needs = character(), keys = c("dso", "dpo", "dio"),
         line_id = "CASH", line = "Trésorerie", out = "cash"),
    list(id = "capex_schedule", label = fpa_txt("loc_mech_capex_cal"),
         role = "decisions", needs = character(), keys = character(),
         line_id = "D_A", line = fpa_txt("loc_mech_da_cash"), out = "cash"),
    list(id = "debt_schedule", label = fpa_txt("loc_mech_debt_cal"),
         role = "decisions", needs = character(), keys = character(),
         line_id = "DEBT", line = "Trésorerie", out = "cash")
  )
}

fpa_chain_frame <- function(source) {
  role <- as.character(source$file_role %||% "")
  frame <- as.character(source$time_frame %||% "")
  if (!nzchar(frame)) {
    frame <- if (role %in% c("pnl_actuals", "opening_bs")) "history" else if (identical(role, "budget")) "plan" else "rolling"
  }
  frame
}

fpa_chain_frame_label <- function(frame) {
  switch(as.character(frame),
    history = "Historique",
    rolling = "Suivi",
    plan = "Plan",
    ""
  )
}

#' Every file the calculation reads: role paths and extra files.
fpa_config_source_paths <- function(config) {
  if (is.null(config)) return(character(0))
  out <- character(0)
  for (nm in names(config$paths %||% list())) {
    if (identical(nm, "working_root_hint")) next
    fp <- as.character(config$paths[[nm]] %||% "")
    if (length(fp) == 1L && nzchar(fp)) out <- c(out, fp)
  }
  for (ex in config$extra_files %||% list()) {
    fp <- as.character(ex$path %||% "")
    if (length(fp) == 1L && nzchar(fp)) out <- c(out, fp)
  }
  unique(out)
}

fpa_chain_source_path <- function(config, source) {
  sid <- as.character(source$source_id %||% "")
  for (ex in config$extra_files %||% list()) {
    if (!identical(as.character(ex$source_id %||% ""), sid)) next
    p <- as.character(ex$path %||% "")
    if (nzchar(p)) return(p)
  }
  role <- as.character(source$file_role %||% "")
  if (exists("fpa_resolve_path", mode = "function") && nzchar(role)) {
    hit <- tryCatch(fpa_resolve_path(config, role), error = function(e) NA_character_)
    if (length(hit) == 1L && !is.na(hit) && nzchar(hit)) return(as.character(hit))
  }
  as.character((config$paths %||% list())[[role]] %||% "")
}

fpa_chain_roles <- function(source) {
  vals <- unlist(source$column_map %||% list(), use.names = FALSE)
  tolower(as.character(vals))
}

fpa_chain_keys <- function(config) {
  rows <- fpa_enabled_assumptions(config)
  if (!length(rows)) return(character())
  vapply(rows, function(k) as.character(k$id), character(1))
}

fpa_chain_has_keys <- function(keys, wanted) {
  if (!length(wanted)) return(TRUE)
  have <- tolower(keys)
  all(vapply(wanted, function(w) any(startsWith(have, tolower(w))), logical(1)))
}

fpa_chain_used_lines <- function(evidence) {
  if (is.null(evidence) || !nrow(evidence)) return(character())
  if (!all(c("line_id", "evidence_label") %in% names(evidence))) return(character())
  ok <- as.character(evidence$evidence_label) %in% c("mechanical_rule", "management_assumption")
  unique(as.character(evidence$line_id)[ok])
}

fpa_chain_has_import <- function(evidence) {
  if (is.null(evidence) || !nrow(evidence) || !"evidence_label" %in% names(evidence)) return(FALSE)
  any(as.character(evidence$evidence_label) == "imported_actual")
}

#' Nodes and edges for the configuration map.
#' evidence is pnl-monthly-long, or NULL when the last run was not read.
fpa_chain_build <- function(config, evidence = NULL) {
  empty_nodes <- data.frame(
    id = character(), col = character(), title = character(), sub = character(),
    frame = character(), status = character(), detail = character(), path = character(),
    stringsAsFactors = FALSE
  )
  empty_edges <- data.frame(from = character(), to = character(), kind = character(), stringsAsFactors = FALSE)
  if (is.null(config)) {
    return(list(nodes = empty_nodes, edges = empty_edges, gaps = "Pas de configuration.", read = FALSE))
  }
  nodes <- list()
  edges <- list()
  gaps <- character()
  add_node <- function(id, col, title, sub, frame, status, detail, path = "") {
    nodes[[length(nodes) + 1L]] <<- data.frame(
      id = id, col = col, title = title, sub = sub, frame = frame,
      status = status, detail = detail, path = as.character(path %||% ""),
      stringsAsFactors = FALSE
    )
  }
  add_edge <- function(from, to, kind) {
    edges[[length(edges) + 1L]] <<- data.frame(from = from, to = to, kind = kind, stringsAsFactors = FALSE)
  }

  read <- !is.null(evidence)
  used <- fpa_chain_used_lines(evidence)
  imported <- fpa_chain_has_import(evidence)
  keys <- tryCatch(fpa_chain_keys(config), error = function(e) character())
  on <- function(id) isTRUE(tryCatch(fpa_mechanism_on(config, id), error = function(e) FALSE))

  sources <- config$sources %||% list()
  for (s in sources) {
    sid <- paste0("src:", as.character(s$source_id %||% s$source_name %||% length(nodes)))
    path <- fpa_chain_source_path(config, s)
    present <- nzchar(path) && file.exists(path)
    frame <- fpa_chain_frame(s)
    name <- as.character(s$source_name %||% s$source_id %||% "Fichier")
    file <- if (nzchar(path)) basename(path) else "chemin vide"
    declared <- as.character(unlist(s$headers_declared %||% list()))
    mapped <- names(s$column_map %||% list())
    bare <- setdiff(declared, mapped)
    sub <- paste(fpa_chain_frame_label(frame), file, sep = " · ")
    where <- if (!nzchar(path)) {
      fpa_txt("loc_no_path")
    } else if (present) {
      fpa_txt("loc_file_on_disk")
    } else {
      paste0(fpa_txt("loc_file_missing_at"), path, ". ")
    }
    detail <- paste0(
      name, ". ", file, ". ", where,
      if (length(bare)) paste0(fpa_todo_column_sentence(bare), " ") else ""
    )
    add_node(sid, "source", name, sub, frame, if (present) "présent" else "absent", detail, path)
    if (!present) gaps <- c(gaps, paste0(name, " : ", trimws(where)))
    if (length(bare)) gaps <- c(gaps, paste0(name, " : ", paste(bare, collapse = ", "), fpa_txt("loc_without_role_sp")))
  }

  out_meta <- list(
    pnl = list(title = "Compte de résultat", sub = fpa_txt("loc_actual_then_fc")),
    bs = list(title = "Bilan", sub = fpa_txt("loc_opening_from_bs")),
    cash = list(title = "Trésorerie", sub = fpa_txt("loc_days_capex_debt")),
    var = list(title = fpa_txt("loc_gap_to_budget"), sub = fpa_txt("loc_voted_vs_actual"))
  )
  for (oid in names(out_meta)) {
    out_status <- if (!read) {
      "non relu"
    } else if (identical(oid, "pnl") && (imported || length(used))) {
      "écrit"
    } else {
      "non vérifié"
    }
    add_node(
      paste0("out:", oid), "output", out_meta[[oid]]$title, out_meta[[oid]]$sub,
      "", out_status,
      paste0(out_meta[[oid]]$title, ". ", out_meta[[oid]]$sub, ". ",
             if (identical(out_status, "non vérifié")) fpa_txt("loc_output_not_reread") else "")
    )
  }

  # Straight reads. Annual history is not turned into a monthly forecast.
  direct <- list(
    list(role = "pnl_actuals", rule = "read:annual", label = fpa_txt("loc_read_closed_years"),
         out = "pnl", text = fpa_txt("loc_annual_as_is")),
    list(role = "pnl_monthly", rule = "read:month", label = fpa_txt("loc_months_already_closed"),
         out = "pnl", text = fpa_txt("loc_closed_feed_actual")),
    list(role = "opening_bs", rule = "read:bs", label = fpa_txt("loc_opening_bs"),
         out = "bs", text = fpa_txt("loc_opening_fixes")),
    list(role = "budget", rule = "read:budget", label = fpa_txt("scnp_col_budget"),
         out = "var", text = fpa_txt("loc_budget_frozen"))
  )
  rule_line <- list()
  for (d in direct) {
    hits <- Filter(function(s) identical(as.character(s$file_role %||% ""), d$role), sources)
    if (!length(hits)) next
    rid <- paste0("rule:", d$rule)
    status <- if (!read) "non relu" else if (d$out == "pnl" && imported) "utilisé" else if (d$out != "pnl" && read) "déclaré" else "déclaré"
    add_node(rid, "rule", d$label, d$text, fpa_chain_frame(hits[[1]]), status, d$text)
    rule_line[[rid]] <- if (d$out == "pnl") "IMPORT" else d$out
    for (s in hits) {
      add_edge(paste0("src:", as.character(s$source_id)), rid, "direct")
    }
    add_edge(rid, paste0("out:", d$out), "direct")
  }

  enabled_rules <- list()
  for (m in fpa_chain_catalog()) {
    if (!on(m$id)) next
    role_sources <- Filter(function(s) identical(as.character(s$file_role %||% ""), m$role), sources)
    matched <- Filter(function(s) {
      roles <- fpa_chain_roles(s)
      cols_ok <- !length(m$needs) || all(m$needs %in% roles)
      keys_ok <- fpa_chain_has_keys(keys, m$keys)
      cols_ok && keys_ok
    }, role_sources)
    rid <- paste0("rule:", m$id)
    if (length(matched)) {
      status <- "relié"
      detail <- paste0(m$label, " alimente ", m$line, ".")
    } else if (!length(role_sources)) {
      status <- "incomplet"
      detail <- paste0(m$label, fpa_txt("loc_rule_on_no_file"), m$role, fpa_txt("loc_is_not_described"))
      gaps <- c(gaps, detail)
    } else {
      status <- "incomplet"
      missing <- character()
      if (length(m$needs)) missing <- c(missing, paste(m$needs, collapse = ", "))
      if (length(m$keys) && !fpa_chain_has_keys(keys, m$keys)) missing <- c(missing, paste(m$keys, collapse = ", "))
      detail <- paste0(m$label, fpa_txt("loc_rule_on_missing"), paste(missing, collapse = " ; "), ".")
      gaps <- c(gaps, detail)
    }
    add_node(rid, "rule", m$label, detail, "rolling", status, detail)
    enabled_rules[[rid]] <- m$line_id
    for (s in matched) add_edge(paste0("src:", as.character(s$source_id)), rid, "mechanism")
    add_edge(rid, paste0("out:", m$out), "mechanism")
    if (identical(m$id, "capex_schedule")) add_edge(rid, "out:pnl", "mechanism")
    if (identical(m$id, "dso_dpo_dio")) {
      for (s in sources) {
        if (identical(as.character(s$file_role %||% ""), "opening_bs")) {
          add_edge(paste0("src:", as.character(s$source_id)), rid, "mechanism")
        }
      }
    }
  }

  # A calculated line marks a rule as used only when that rule is the only one writing it.
  if (read && length(used)) {
    by_line <- split(names(enabled_rules), unlist(enabled_rules))
    for (rid in names(enabled_rules)) {
      line_id <- enabled_rules[[rid]]
      writers <- by_line[[line_id]]
      if (!length(writers) || !(line_id %in% used)) next
      hit <- which(vapply(nodes, function(n) identical(n$id, rid), logical(1)))
      if (!length(hit)) next
      if (length(writers) == 1L) {
        nodes[[hit]]$status <- "utilisé"
        nodes[[hit]]$detail <- paste0(nodes[[hit]]$detail, fpa_txt("loc_calc_wrote_line"))
      } else {
        nodes[[hit]]$status <- "partagé"
        nodes[[hit]]$detail <- paste0(nodes[[hit]]$detail, fpa_txt("loc_calc_wrote_shared"))
      }
    }
  }

  linked_src <- unique(unlist(lapply(edges, function(e) e$from)))
  for (i in seq_along(nodes)) {
    if (!identical(nodes[[i]]$col, "source")) next
    if (nodes[[i]]$id %in% linked_src) next
    if (identical(as.character(nodes[[i]]$status), "absent")) next
    nodes[[i]]$status <- "sans suite"
    file <- basename(as.character(nodes[[i]]$path %||% ""))
    if (!nzchar(file) || identical(file, ".")) file <- as.character(nodes[[i]]$title)
    nodes[[i]]$detail <- fpa_todo_unused_sentence(file)
    gaps <- c(gaps, paste0(nodes[[i]]$title, fpa_txt("loc_no_rule_downstream")))
  }

  node_df <- if (length(nodes)) do.call(rbind, nodes) else empty_nodes
  edge_df <- if (length(edges)) do.call(rbind, edges) else empty_edges
  if (nrow(edge_df)) edge_df <- unique(edge_df)
  list(nodes = node_df, edges = edge_df, gaps = unique(gaps), read = read)
}

fpa_chain_status_bits <- function(rows) {
  if (is.null(rows) || !nrow(rows)) return("0")
  tab <- table(as.character(rows$status))
  tab <- sort(tab, decreasing = TRUE)
  paste(paste(as.integer(tab), names(tab)), collapse = " · ")
}

#' Sources whose path is empty or does not point at a file.
#' A file on disk with no downstream rule is not in this list.
fpa_chain_absent_sources <- function(config) {
  empty <- data.frame(id = character(), title = character(), path = character(), stringsAsFactors = FALSE)
  if (is.null(config)) return(empty)
  rows <- list()
  for (s in config$sources %||% list()) {
    path <- tryCatch(fpa_chain_source_path(config, s), error = function(e) "")
    path <- as.character(path %||% "")
    if (length(path) != 1L) path <- ""
    if (nzchar(path) && file.exists(path)) next
    sid <- paste0("src:", as.character(s$source_id %||% s$source_name %||% "fichier"))
    title <- as.character(s$source_name %||% s$source_id %||% "Fichier")
    if (length(title) != 1L || !nzchar(title)) title <- "Fichier"
    rows[[length(rows) + 1L]] <- data.frame(
      id = sid, title = title, path = path, stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

#' Month number from a wide header such as "Septembre (€)" or "Sep".
fpa_chain_month_col_index <- function(name) {
  raw <- tolower(enc2utf8(as.character(name %||% "")))
  raw <- gsub("\\(.*\\)", "", raw, perl = TRUE)
  raw <- iconv(raw, from = "UTF-8", to = "ASCII//TRANSLIT")
  if (is.na(raw)) raw <- ""
  raw <- gsub("[^a-z]", "", tolower(raw))
  keys <- c(
    janvier = 1L, january = 1L, jan = 1L,
    fevrier = 2L, february = 2L, feb = 2L,
    mars = 3L, march = 3L, mar = 3L,
    avril = 4L, april = 4L, apr = 4L,
    mai = 5L, may = 5L,
    juin = 6L, june = 6L, jun = 6L,
    juillet = 7L, july = 7L, jul = 7L,
    aout = 8L, august = 8L, aug = 8L,
    septembre = 9L, september = 9L, sep = 9L, sept = 9L,
    octobre = 10L, october = 10L, oct = 10L,
    novembre = 11L, november = 11L, nov = 11L,
    decembre = 12L, december = 12L, dec = 12L
  )
  if (!nzchar(raw) || !raw %in% names(keys)) return(NA_integer_)
  as.integer(keys[[raw]])
}

#' Months that carry a non-zero amount in a wide monthly workbook.
#' Year comes from the declared periods when the headers have no year.
fpa_chain_wide_month_dates <- function(path, years = integer()) {
  if (!nzchar(path) || !file.exists(path)) return(as.Date(character()))
  years <- unique(as.integer(years))
  years <- years[is.finite(years)]
  if (!length(years)) {
    m <- regmatches(path, regexpr("20[0-9]{2}", path))
    if (length(m) && nzchar(m)) years <- as.integer(m)
  }
  if (!length(years)) return(as.Date(character()))
  ext <- tolower(tools::file_ext(path))
  df <- NULL
  if (ext %in% c("csv", "txt")) {
    df <- fpa_csv_read_sniff(path, nrows = 5000L)
  } else if (ext %in% c("xlsx", "xlsm", "xls") && requireNamespace("readxl", quietly = TRUE)) {
    df <- tryCatch(
      as.data.frame(readxl::read_excel(path, n_max = 5000), stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(df) || !ncol(df)) return(as.Date(character()))
  out <- as.Date(character())
  for (nm in names(df)) {
    mo <- fpa_chain_month_col_index(nm)
    if (is.na(mo)) next
    vals <- suppressWarnings(as.numeric(df[[nm]]))
    if (!any(is.finite(vals) & abs(vals) > 1e-9)) next
    for (y in years) out <- c(out, as.Date(sprintf("%04d-%02d-01", y, mo)))
  }
  unique(out)
}

fpa_chain_file_dates <- function(path) {
  if (!nzchar(path) || !file.exists(path)) return(as.Date(character()))
  ext <- tolower(tools::file_ext(path))
  df <- NULL
  if (ext %in% c("csv", "txt")) {
    df <- fpa_csv_read_sniff(path, nrows = 5000L)
  } else if (ext %in% c("xlsx", "xlsm", "xls") && requireNamespace("readxl", quietly = TRUE)) {
    df <- tryCatch(
      as.data.frame(readxl::read_excel(path, n_max = 5000), stringsAsFactors = FALSE),
      error = function(e) NULL
    )
  }
  if (is.null(df) || !ncol(df)) return(as.Date(character()))
  years <- grep("^[0-9]{4}$", names(df), value = TRUE)
  year_dates <- as.Date(character())
  if (length(years)) year_dates <- as.Date(paste0(years, "-12-01"))
  i <- which(tolower(names(df)) %in% c("periode", "period", "date", "mois"))[1]
  if (is.na(i)) return(year_dates)
  raw <- as.character(df[[i]])
  raw <- raw[nzchar(raw)]
  ym <- unique(grep("^[0-9]{4}-[0-9]{2}$", raw, value = TRUE))
  col_dates <- if (length(ym)) {
    # A YYYY-MM row covers the whole month. The 1st alone stops the bar on
    # the first day, short of a plan that closes on the 31st.
    starts <- as.Date(paste0(ym, "-01"))
    ends <- as.Date(vapply(ym, function(token) {
      bits <- strsplit(token, "-", fixed = TRUE)[[1]]
      as.character(fpa_chain_month_end(as.integer(bits[[1]]), as.integer(bits[[2]])))
    }, character(1)))
    c(starts, ends)
  } else {
    parsed <- suppressWarnings(as.Date(raw))
    parsed[is.finite(as.numeric(parsed))]
  }
  c(year_dates, col_dates)
}

#' A year column covers the whole exercise, not only its closing day.
fpa_chain_period_bounds <- function(p) {
  pe <- as.Date(as.character(p$period_end %||% NA_character_))
  if (length(pe) != 1L || is.na(pe)) return(as.Date(character()))
  grain <- as.character(p$grain %||% "")
  if (identical(grain, "year") || identical(grain, "month")) {
    first <- as.Date(paste0(format(pe, "%Y-%m"), "-01"))
    if (identical(grain, "year")) {
      first <- seq(first, by = "-11 months", length.out = 2L)[[2]]
    }
    return(c(first, pe))
  }
  pe
}

#' Opening balance is one instant: the first day of the monthly book.
fpa_chain_snapshot_date <- function(config) {
  sources <- config$sources %||% list()
  monthly <- as.Date(character())
  for (s in sources) {
    if (!identical(as.character(s$file_role %||% ""), "pnl_monthly")) next
    bounds <- as.Date(unlist(lapply(s$periods %||% list(), fpa_chain_period_bounds)))
    bounds <- bounds[is.finite(as.numeric(bounds))]
    monthly <- c(monthly, bounds)
  }
  if (length(monthly)) return(min(monthly))
  annual <- as.Date(character())
  for (s in sources) {
    if (!identical(as.character(s$file_role %||% ""), "pnl_actuals")) next
    bounds <- as.Date(unlist(lapply(s$periods %||% list(), fpa_chain_period_bounds)))
    bounds <- bounds[is.finite(as.numeric(bounds))]
    annual <- c(annual, bounds)
  }
  if (length(annual)) return(max(annual) + 1)
  as.Date(NA)
}

#' How far each source reaches, against a cut date.
#' A rolling file is late when its last month is more than one month behind today.
fpa_chain_coverage <- function(config, today = Sys.Date()) {
  empty <- data.frame(
    id = character(), title = character(), frame = character(),
    start = as.Date(character()), end = as.Date(character()),
    months_back = integer(), state = character(),
    left_pct = numeric(), width_pct = numeric(), ahead_pct = numeric(), today_pct = numeric(),
    stringsAsFactors = FALSE
  )
  sources <- config$sources %||% list()
  if (!length(sources)) return(empty)
  today <- as.Date(today)
  rows <- lapply(sources, function(s) {
    declared <- as.Date(unlist(lapply(s$periods %||% list(), fpa_chain_period_bounds)))
    path <- fpa_chain_source_path(config, s)
    observed <- fpa_chain_file_dates(path)
    years_hint <- unique(as.integer(format(declared[is.finite(as.numeric(declared))], "%Y")))
    wide <- fpa_chain_wide_month_dates(path, years_hint)
    frame <- fpa_chain_frame(s)
    # Rolling monthly books often declare the whole year while future months are
    # still blank zeros. The bar must stop on the last month that carries a figure.
    if (identical(frame, "rolling") && length(wide)) {
      dates <- wide
      if (length(declared)) dates <- c(min(declared), wide)
    } else {
      dates <- c(declared, observed, wide)
    }
    dates <- dates[is.finite(as.numeric(dates))]
    if (!length(dates) && identical(as.character(s$file_role %||% ""), "opening_bs")) {
      snap <- fpa_chain_snapshot_date(config)
      if (length(snap) == 1L && !is.na(snap)) dates <- snap
    }
    title <- as.character(s$source_name %||% s$source_id %||% "Fichier")
    if (!length(dates)) {
      return(data.frame(
        id = paste0("src:", as.character(s$source_id)), title = title, frame = frame,
        start = as.Date(NA), end = as.Date(NA), months_back = NA_integer_, state = "non daté",
        left_pct = NA_real_, width_pct = NA_real_, ahead_pct = NA_real_, today_pct = NA_real_,
        stringsAsFactors = FALSE
      ))
    }
    start <- min(dates)
    end <- max(dates)
        if (identical(frame, "rolling") && length(wide)) {
          end <- max(wide)
          end <- fpa_chain_month_end(as.integer(format(end, "%Y")), as.integer(format(end, "%m")))
          # Actuals do not claim days after today, even inside the last filled month.
          if (end > today) end <- today
        }
    end_m <- as.integer(format(end, "%Y")) * 12L + as.integer(format(end, "%m"))
    now_m <- as.integer(format(today, "%Y")) * 12L + as.integer(format(today, "%m"))
    # Rates and parameters hold until the next change. A January row is in force,
    # not a monthly feed that stopped in January.
    state <- if (identical(as.character(s$file_role %||% ""), "opening_bs") && start == end) {
      "instantané"
    } else if (identical(as.character(s$file_role %||% ""), "assumptions")) {
      "en vigueur"
    } else if (identical(frame, "rolling")) {
      if (end_m >= now_m - 1L) "à jour" else "en retard"
    } else if (identical(frame, "history")) {
      if (as.integer(format(end, "%Y")) >= as.integer(format(today, "%Y")) - 1L) "à jour" else "en retard"
    } else if (as.integer(format(end, "%Y")) >= as.integer(format(today, "%Y"))) {
      "à jour"
    } else {
      "en retard"
    }
    data.frame(
      id = paste0("src:", as.character(s$source_id)), title = title, frame = frame,
      start = start, end = end,
      months_back = as.integer(round(as.numeric(today - start) / 30.44)),
      state = state,
      left_pct = NA_real_, width_pct = NA_real_, ahead_pct = NA_real_, today_pct = NA_real_,
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, rows)
  dated <- is.finite(as.numeric(out$start))
  out$mark <- "bar"
  ok <- is.finite(as.numeric(out$start)) & is.finite(as.numeric(out$end))
  same_month <- ok & format(out$start, "%Y-%m") == format(out$end, "%Y-%m")
  out$mark[same_month | (ok & out$state == "instantané")] <- "dot"
  out$mark[!ok] <- "none"
  if (!any(dated)) return(out)
  lo <- min(c(out$start[dated], today))
  hi <- max(c(out$end[dated], today))
  span <- as.numeric(hi - lo)
  if (!is.finite(span) || span < 1) span <- 1
  out$today_pct <- as.numeric(today - lo) / span * 100
  out$left_pct[dated] <- as.numeric(out$start[dated] - lo) / span * 100
  reach <- pmin(out$end, today)
  # A voted plan is one figure for the whole span. Do not fade it after today.
  plan <- dated & !is.na(out$frame) & out$frame == "plan"
  reach[plan] <- out$end[plan]
  out$width_pct[dated] <- pmax(0, as.numeric(reach[dated] - out$start[dated]) / span * 100)
  out$ahead_pct[dated] <- pmax(0, as.numeric(out$end[dated] - today) / span * 100)
  out$ahead_pct[plan] <- 0
  out
}

fpa_chain_window <- function(cov, today = Sys.Date()) {
  if (is.null(cov) || !nrow(cov) || !("start" %in% names(cov))) return(NULL)
  dated <- is.finite(as.numeric(cov$start))
  if (!any(dated)) return(NULL)
  today <- as.Date(today)
  lo <- min(c(cov$start[dated], today), na.rm = TRUE)
  hi <- max(c(cov$end[dated], today), na.rm = TRUE)
  span <- as.numeric(hi - lo)
  if (!is.finite(span) || span < 1) span <- 1
  list(lo = lo, hi = hi, span = span, today = today)
}

fpa_chain_month_fr <- function(month) {
  names <- c(
    fpa_txt("month_1"), fpa_txt("month_2"), fpa_txt("month_3"), fpa_txt("month_4"), fpa_txt("month_5"), fpa_txt("month_6"),
    fpa_txt("month_7"), fpa_txt("month_8"), fpa_txt("month_9"), fpa_txt("month_10"), fpa_txt("month_11"), fpa_txt("month_12")
  )
  month <- as.integer(month)
  if (is.na(month) || month < 1L || month > 12L) return("")
  names[[month]]
}

fpa_chain_month_end <- function(year, month) {
  start <- as.Date(sprintf("%04d-%02d-01", as.integer(year), as.integer(month)))
  seq(start, by = "month", length.out = 2L)[[2]] - 1
}

#' Date window for one fiscal year on a chart.
#' `fy_months` is the landing exercise. `which` is "fy" or "prior".
fpa_fy_window <- function(fy_months, which = "fy") {
  months <- sort(unique(as.character(fy_months)))
  months <- months[grepl("^[0-9]{4}-[0-9]{2}$", months)]
  if (identical(as.character(which[[1]]), "prior") && length(months)) {
    months <- sprintf("%04d-%s", as.integer(substr(months, 1, 4)) - 1L, substr(months, 6, 7))
  }
  if (!length(months)) {
    return(list(start = as.Date(NA), end = as.Date(NA), months = character()))
  }
  start <- as.Date(paste0(months[[1]], "-01")) - 12
  last <- as.Date(paste0(months[[length(months)]], "-01"))
  end <- seq(last, by = "month", length.out = 2L)[[2]] + 2
  list(start = start, end = end, months = months)
}

#' First day of each fiscal year that falls strictly inside `periods` (YYYY-MM).
#' The opening year is not marked: the axis already starts there.
fpa_fy_mark_dates <- function(periods, fy_end = 12L) {
  empty <- data.frame(x = as.Date(character()), label = character(), stringsAsFactors = FALSE)
  periods <- as.character(periods)
  periods <- periods[grepl("^[0-9]{4}-[0-9]{2}$", periods)]
  if (length(periods) < 2L) return(empty)
  fy_end <- suppressWarnings(as.integer(fy_end[[1]]))
  if (length(fy_end) != 1L || is.na(fy_end) || fy_end < 1L || fy_end > 12L) fy_end <- 12L
  ord <- sort(unique(periods))
  lo <- as.Date(paste0(ord[[1]], "-01"))
  hi <- as.Date(paste0(ord[[length(ord)]], "-01"))
  y0 <- as.integer(substr(ord[[1]], 1, 4)) - 1L
  y1 <- as.integer(substr(ord[[length(ord)]], 1, 4)) + 1L
  rows <- list()
  for (y in y0:y1) {
    start <- fpa_chain_fy_bounds(y, fy_end)$start
    if (start <= lo || start > hi) next
    lab <- if (fy_end == 12L) {
      as.character(y)
    } else {
      sprintf("%02d-%02d", as.integer(format(start, "%Y")) %% 100L, y %% 100L)
    }
    rows[[length(rows) + 1L]] <- data.frame(x = start, label = lab, stringsAsFactors = FALSE)
  }
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out[!duplicated(out$x), , drop = FALSE]
}

#' Fiscal year that closes in `end_year`, month `fy_end` (1–12).
fpa_chain_fy_bounds <- function(end_year, fy_end) {
  fy_end <- as.integer(fy_end)
  end_year <- as.integer(end_year)
  if (fy_end == 12L) {
    start <- as.Date(sprintf("%04d-01-01", end_year))
  } else {
    start <- as.Date(sprintf("%04d-%02d-01", end_year - 1L, fy_end + 1L))
  }
  list(start = start, end = fpa_chain_month_end(end_year, fy_end))
}

#' One row of calendar years and one row of fiscal years, for the file span.
#' Every year that touches the window is included.
fpa_chain_rulers <- function(cov, fy_end = 12L, today = Sys.Date()) {
  empty <- data.frame(
    kind = character(), label = character(), full = character(),
    left_pct = numeric(), width_pct = numeric(),
    stringsAsFactors = FALSE
  )
  win <- fpa_chain_window(cov, today)
  if (is.null(win)) return(empty)
  fy_end <- as.integer(fy_end %||% 12L)
  if (is.na(fy_end) || fy_end < 1L || fy_end > 12L) fy_end <- 12L
  clip <- function(a, b) {
    a <- max(as.Date(a), win$lo)
    b <- min(as.Date(b), win$hi)
    if (b < a) return(NULL)
    left <- as.numeric(a - win$lo) / win$span * 100
    width <- as.numeric(b - a) / win$span * 100
    if (width > 1.2) width <- width - 0.35
    list(left = left, width = max(width, 0.6))
  }
  rows <- list()
  y0 <- as.integer(format(win$lo, "%Y"))
  y1 <- as.integer(format(win$hi, "%Y"))
  for (y in y0:y1) {
    box <- clip(sprintf("%d-01-01", y), sprintf("%d-12-31", y))
    if (is.null(box)) next
    rows[[length(rows) + 1L]] <- data.frame(
      kind = "civil", label = as.character(y), full = as.character(y),
      left_pct = box$left, width_pct = box$width, stringsAsFactors = FALSE
    )
  }
  for (y in (y0 - 1L):(y1 + 1L)) {
    bounds <- fpa_chain_fy_bounds(y, fy_end)
    if (bounds$end < win$lo || bounds$start > win$hi) next
    box <- clip(bounds$start, bounds$end)
    if (is.null(box)) next
    full <- if (fy_end == 12L) as.character(y) else sprintf("%d-%d", as.integer(format(bounds$start, "%Y")), y)
    short <- if (fy_end == 12L) as.character(y) else sprintf("%02d-%02d", as.integer(format(bounds$start, "%Y")) %% 100L, y %% 100L)
    rows[[length(rows) + 1L]] <- data.frame(
      kind = "fiscal", label = short, full = full,
      left_pct = box$left, width_pct = box$width, stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(empty)
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

fpa_chain_touch <- function(chain, id) {
  if (!nzchar(id) || is.null(chain$edges) || !nrow(chain$edges)) return(id)
  e <- chain$edges
  near <- unique(c(id, e$to[e$from == id], e$from[e$to == id]))
  more <- unique(c(e$to[e$from %in% near], e$from[e$to %in% near]))
  unique(c(near, more))
}

#' Closing year of the fiscal year that contains `today`. December close in October is that calendar year.
fpa_budget_open_year <- function(fy_end = 12L, today = Sys.Date()) {
  fy_end <- as.integer(fy_end %||% 12L)
  if (is.na(fy_end) || fy_end < 1L || fy_end > 12L) fy_end <- 12L
  today <- as.Date(today)
  y <- as.integer(format(today, "%Y"))
  m <- as.integer(format(today, "%m"))
  if (m <= fy_end) y else y + 1L
}

fpa_budget_fy_end <- function(config) {
  n <- suppressWarnings(as.integer((config$company %||% list())$fiscal_year_end_month %||% 12L))
  if (length(n) != 1L || is.na(n) || n < 1L || n > 12L) return(12L)
  n
}

#' Open year first, then the year before, the next year, and two years back. The saved year stays in the list.
fpa_budget_year_choices <- function(open_year, selected = NA_integer_) {
  open_year <- as.integer(open_year)
  raw <- c(open_year, open_year - 1L, open_year + 1L, open_year - 2L)
  selected <- suppressWarnings(as.integer(selected))
  if (length(selected) == 1L && !is.na(selected)) raw <- c(raw, selected)
  raw <- as.integer(raw)
  raw <- raw[!is.na(raw) & raw >= 2000L & raw <= 2100L]
  unique(raw)
}

fpa_budget_fold <- function(s) {
  s <- tolower(enc2utf8(as.character(s %||% "")))
  s <- chartr(
    "\u00e0\u00e1\u00e2\u00e4\u00e9\u00e8\u00ea\u00eb\u00ef\u00ee\u00f4\u00f6\u00f9\u00fb\u00fc\u00e7",
    "aaaaeeeeiioouuuc",
    s
  )
  trimws(gsub("[^a-z0-9+-]+", " ", s))
}

#' Enough of the website period parser to retarget N, N-1, and calendar years. Months stay months.
fpa_budget_parse_label <- function(label) {
  empty <- list(kind = "unknown", offset = NA_integer_, year = NA_integer_, month = NA_integer_)
  raw <- trimws(as.character(label %||% ""))
  if (!nzchar(raw)) return(empty)
  folded <- fpa_budget_fold(raw)
  compact <- gsub(" ", "", folded, fixed = TRUE)
  if (!nzchar(folded)) return(empty)
  rel <- function(offset) {
    list(kind = "relative", offset = as.integer(offset), year = NA_integer_, month = NA_integer_)
  }
  if (grepl("^n\\+[0-9]+$", compact)) return(rel(sub("^n\\+", "", compact)))
  if (grepl("^n-[0-9]+$", compact)) return(rel(-as.integer(sub("^n-", "", compact))))
  if (compact %in% c("n", "montantn", "budgetn")) return(rel(0L))
  if (grepl("n-[0-9]+$", compact) && grepl("montant|amount|budget|exercice", compact)) {
    return(rel(-as.integer(sub("^.*n-", "", compact))))
  }
  padded <- paste0(" ", folded, " ")
  if (grepl("montant|amount|budget|exercice", folded) &&
      grepl(" n ", padded, fixed = TRUE) &&
      !grepl("20[0-9]{2}", compact)) {
    return(rel(0L))
  }
  iso <- regexec("(20[0-9]{2})[-/. ](0?[1-9]|1[0-2])", folded)
  hit <- regmatches(folded, iso)[[1]]
  if (length(hit) >= 3L) {
    return(list(
      kind = "month", offset = NA_integer_,
      year = as.integer(hit[[2]]), month = as.integer(hit[[3]])
    ))
  }
  if (grepl("^20[0-9]{2}$", compact)) {
    return(list(kind = "calendar", offset = NA_integer_, year = as.integer(compact), month = NA_integer_))
  }
  months <- c(
    janvier = 1L, janv = 1L, jan = 1L, fevrier = 2L, fevr = 2L, fev = 2L,
    mars = 3L, mar = 3L, avril = 4L, avr = 4L, mai = 5L, juin = 6L, jun = 6L,
    juillet = 7L, juil = 7L, jul = 7L, aout = 8L, aug = 8L,
    septembre = 9L, sept = 9L, sep = 9L, octobre = 10L, oct = 10L,
    novembre = 11L, nov = 11L, decembre = 12L, dec = 12L
  )
  tokens <- strsplit(folded, "[ +]+", perl = TRUE)[[1]]
  tokens <- tokens[nzchar(tokens)]
  for (token in tokens) {
    if (!token %in% names(months)) next
    y <- NA_integer_
    for (other in tokens) if (grepl("^20[0-9]{2}$", other)) y <- as.integer(other)
    return(list(kind = "month", offset = NA_integer_, year = y, month = unname(months[[token]])))
  }
  empty
}

fpa_budget_period_cols <- function(column_map, column_period) {
  cols <- list()
  column_map <- column_map %||% list()
  column_period <- column_period %||% list()
  for (header in names(column_map)) {
    if (!identical(as.character(column_map[[header]] %||% ""), "periode_montant")) next
    lab <- column_period[[header]] %||% header
    lab <- trimws(as.character(if (length(lab)) lab[[1]] else header))
    cols[[length(cols) + 1L]] <- list(header = header, label = lab)
  }
  cols
}

fpa_budget_chr_map <- function(x) {
  if (is.null(x) || !length(x)) return(stats::setNames(character(), character()))
  vals <- vapply(x, function(v) as.character(v %||% "")[[1]], "")
  names(vals) <- names(x)
  vals[order(names(vals))]
}

#' Point N, or the latest calendar column, at `year`. N-1 moves with it. Month columns stay.
fpa_budget_retarget <- function(column_map, column_period, year) {
  year <- as.integer(year)
  cols <- fpa_budget_period_cols(column_map, column_period)
  out <- column_period %||% list()
  rel <- list()
  for (col in cols) {
    parsed <- fpa_budget_parse_label(col$label)
    if (identical(parsed$kind, "relative") && !is.na(parsed$offset)) {
      rel[[length(rel) + 1L]] <- list(header = col$header, year = year + parsed$offset)
    }
  }
  if (length(rel)) {
    for (row in rel) out[[row$header]] <- as.character(row$year)
    return(out)
  }
  years <- integer()
  for (col in cols) {
    parsed <- fpa_budget_parse_label(col$label)
    if (identical(parsed$kind, "calendar") && !is.na(parsed$year)) years <- c(years, parsed$year)
  }
  if (!length(years)) return(out)
  delta <- year - max(years)
  if (delta == 0L) return(out)
  for (col in cols) {
    parsed <- fpa_budget_parse_label(col$label)
    if (identical(parsed$kind, "calendar") && !is.na(parsed$year)) {
      out[[col$header]] <- as.character(parsed$year + delta)
    }
  }
  out
}

fpa_budget_declared_year <- function(source) {
  en <- suppressWarnings(as.integer(source$exercice_n %||% NA_integer_))
  if (length(en) == 1L && !is.na(en) && en >= 2000L && en <= 2100L) return(en)
  years <- integer()
  for (col in fpa_budget_period_cols(source$column_map, source$column_period)) {
    parsed <- fpa_budget_parse_label(col$label)
    if (identical(parsed$kind, "calendar") && !is.na(parsed$year)) years <- c(years, parsed$year)
  }
  if (length(years)) return(max(years))
  NA_integer_
}

fpa_budget_period_years <- function(source) {
  years <- integer()
  for (p in source$periods %||% list()) {
    y <- suppressWarnings(as.integer(p$year %||% NA_integer_))
    if (length(y) == 1L && !is.na(y)) years <- c(years, y)
  }
  if (!length(years)) {
    y <- fpa_budget_declared_year(source)
    if (!is.na(y)) years <- y
  }
  unique(years)
}

#' Rewrite one budget source. Other sources are copied through. An empty map is not written.
fpa_budget_patch_sources <- function(sources, year, fy_end, source_id = "", today = Sys.Date()) {
  year <- suppressWarnings(as.integer(year))
  sources <- sources %||% list()
  if (is.na(year) || year < 2000L || year > 2100L) return(list(ok = FALSE, reason = "year"))
  idx <- 0L
  if (length(sources)) {
    for (i in seq_along(sources)) {
      src <- sources[[i]]
      if (!identical(as.character(src$file_role %||% ""), "budget")) next
      if (nzchar(source_id) && !identical(as.character(src$source_id %||% ""), source_id)) next
      idx <- i
      break
    }
  }
  if (!idx) return(list(ok = FALSE, reason = "missing"))
  src <- sources[[idx]]
  cm <- src$column_map %||% list()
  cp <- src$column_period %||% list()
  if (!length(cm) && !length(cp)) return(list(ok = FALSE, reason = "empty"))
  declared <- fpa_budget_declared_year(src)
  next_cp <- fpa_budget_retarget(cm, cp, year)
  periods <- fpa_budget_compile_periods(
    cm, next_cp, year, fy_end,
    clock_year = as.integer(format(as.Date(today), "%Y")),
    prefer_month = identical(as.character(src$grain %||% ""), "month")
  )
  unchanged <- !is.na(declared) && declared == year && identical(fpa_budget_chr_map(next_cp), fpa_budget_chr_map(cp))
  src$exercice_n <- year
  src$column_period <- next_cp
  src$periods <- periods
  sources[[idx]] <- src
  list(
    ok = TRUE, sources = sources,
    source_id = as.character(src$source_id %||% ""),
    unchanged = unchanged
  )
}

fpa_budget_compile_periods <- function(column_map, column_period, exercice_n, fy_end,
                                       clock_year = NA_integer_, prefer_month = FALSE) {
  fy_end <- as.integer(fy_end %||% 12L)
  if (is.na(fy_end) || fy_end < 1L || fy_end > 12L) fy_end <- 12L
  clock_year <- as.integer(clock_year)
  if (is.na(clock_year)) clock_year <- as.integer(format(Sys.Date(), "%Y"))
  cols <- fpa_budget_period_cols(column_map, column_period)
  label_years <- integer()
  for (col in cols) {
    parsed <- fpa_budget_parse_label(col$label)
    if (!is.na(parsed$year)) label_years <- c(label_years, parsed$year)
  }
  en <- suppressWarnings(as.integer(exercice_n %||% NA_integer_))
  n_year <- if (length(label_years)) {
    max(label_years)
  } else if (length(en) == 1L && !is.na(en) && en >= 2000L && en <= 2100L) {
    en
  } else {
    NA_integer_
  }
  cal_years <- integer()
  for (col in cols) {
    parsed <- fpa_budget_parse_label(col$label)
    if (identical(parsed$kind, "calendar") && !is.na(parsed$year)) cal_years <- c(cal_years, parsed$year)
  }
  max_cal <- if (length(cal_years)) max(cal_years) else n_year
  cursor <- if (length(label_years)) label_years[[1]] else clock_year
  prev_month <- NA_integer_
  month_year <- list()
  for (col in cols) {
    parsed <- fpa_budget_parse_label(col$label)
    if (!identical(parsed$kind, "month") || is.na(parsed$month)) next
    y <- parsed$year
    if (is.na(y)) {
      if (!is.na(prev_month) && parsed$month < prev_month) cursor <- cursor + 1L
      y <- cursor
    } else {
      cursor <- y
    }
    prev_month <- parsed$month
    month_year[[col$header]] <- as.integer(y)
  }
  one <- function(header, label, kind, offset, year, month, grain) {
    end <- if (is.na(year) || is.na(month)) NULL else as.character(fpa_chain_month_end(year, month))
    list(
      header = header, user_label = label, kind = kind,
      offset = as.integer(offset),
      year = if (is.na(year)) NULL else as.integer(year),
      month = if (is.na(month)) NULL else as.integer(month),
      period_end = end, grain = grain
    )
  }
  compiled <- lapply(cols, function(col) {
    parsed <- fpa_budget_parse_label(col$label)
    if (identical(parsed$kind, "month") && !is.na(parsed$month)) {
      y <- month_year[[col$header]] %||% parsed$year %||% clock_year
      return(one(col$header, col$label, "month", 0L, as.integer(y), parsed$month, "month"))
    }
    if (identical(parsed$kind, "calendar") && !is.na(parsed$year)) {
      offset <- if (!is.na(max_cal)) parsed$year - max_cal else 0L
      return(one(col$header, col$label, "calendar", offset, parsed$year, fy_end, "year"))
    }
    if (identical(parsed$kind, "relative") && !is.na(parsed$offset) && !is.na(n_year)) {
      y <- n_year + parsed$offset
      return(one(col$header, col$label, "relative", parsed$offset, y, fy_end, "year"))
    }
    month <- if (isTRUE(prefer_month) && !is.na(parsed$month)) parsed$month else fy_end
    grain <- if (isTRUE(prefer_month)) "month" else "year"
    one(
      col$header, col$label, parsed$kind,
      if (is.na(parsed$offset)) 0L else parsed$offset,
      n_year, month, grain
    )
  })
  if (!length(compiled)) return(list())
  if (any(vapply(compiled, function(row) identical(row$grain, "month"), logical(1)))) {
    ord <- order(vapply(compiled, function(row) as.character(row$period_end %||% ""), ""))
  } else {
    ord <- order(vapply(compiled, function(row) as.integer(row$year %||% 0L), 0L), decreasing = TRUE)
  }
  compiled[ord]
}

#' The menu is shown when the budget periods do not include the open exercise.
fpa_budget_needs_year <- function(source, fy_end = 12L, today = Sys.Date()) {
  if (is.null(source) || !identical(as.character(source$file_role %||% ""), "budget")) return(NULL)
  open <- fpa_budget_open_year(fy_end, today)
  if (open %in% fpa_budget_period_years(source)) return(NULL)
  declared <- fpa_budget_declared_year(source)
  if (is.na(declared)) declared <- open
  list(
    open = open,
    declared = as.integer(declared),
    choices = fpa_budget_year_choices(open, declared)
  )
}

fpa_budget_save_config <- function(config_path, cfg) {
  if (!nzchar(config_path %||% "") || !file.exists(config_path)) return(FALSE)
  cfg$`.config_path` <- NULL
  cfg$`.engine_version` <- NULL
  hist <- file.path(dirname(config_path), "work", "config-history")
  dir.create(hist, recursive = TRUE, showWarnings = FALSE)
  bak <- file.path(hist, paste0("fpa-config-", format(Sys.time(), "%Y%m%d-%H%M%S"), ".json"))
  file.copy(config_path, bak, overwrite = FALSE)
  tmp <- tempfile(tmpdir = dirname(config_path), fileext = ".json")
  wrote <- tryCatch({
    jsonlite::write_json(cfg, tmp, auto_unbox = TRUE, pretty = TRUE, null = "null")
    TRUE
  }, error = function(e) FALSE)
  if (!isTRUE(wrote) || !file.exists(tmp) || !is.finite(file.info(tmp)$size) || file.info(tmp)$size < 20) {
    unlink(tmp)
    return(FALSE)
  }
  removed <- file.remove(config_path)
  if (!isTRUE(removed)) {
    unlink(tmp)
    return(FALSE)
  }
  ok <- file.rename(tmp, config_path)
  if (!isTRUE(ok)) {
    file.copy(bak, config_path, overwrite = TRUE)
    unlink(tmp)
    return(FALSE)
  }
  TRUE
}

#' Rewrite the budget source in the local file. Does not call the website and does not edit the spreadsheet.
fpa_budget_write_year <- function(config_path, year, source_id = "", today = Sys.Date()) {
  cfg <- fpa_load_config(config_path)
  patched <- fpa_budget_patch_sources(cfg$sources, year, fpa_budget_fy_end(cfg), source_id, today)
  if (!isTRUE(patched$ok) || isTRUE(patched$unchanged)) return(patched)
  cfg$sources <- patched$sources
  if (!fpa_budget_save_config(config_path, cfg)) return(list(ok = FALSE, reason = "write"))
  patched
}

#' After a 200 from the budget-year post, store the returned revision so the next pull does not restore the old year.
fpa_budget_stamp_revision <- function(config_path, revision) {
  revision <- suppressWarnings(as.integer(revision))
  if (length(revision) != 1L || is.na(revision)) return(FALSE)
  cfg <- fpa_load_config(config_path)
  if (is.null(cfg$project) || !is.list(cfg$project)) return(FALSE)
  cfg$project$sync_revision <- revision
  fpa_budget_save_config(config_path, cfg)
}
