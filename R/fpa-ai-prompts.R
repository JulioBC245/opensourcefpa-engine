# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Analyst context and prompt catalog.
# Euro amounts are stored as {{e:number}} and rendered in one place.

fpa_ai_eur_token <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) return("n/a")
  sprintf("{{e:%.4f}}", x)
}

fpa_ai_currency_mark <- function(code) {
  raw <- trimws(as.character(code %||% ""))
  if (!nzchar(raw)) raw <- "EUR"
  if (grepl("[€$£¥]", raw)) return(raw)
  switch(toupper(raw),
    EUR = "€", EURO = "€",
    USD = "$",
    GBP = "£",
    JPY = "¥", CNY = "¥",
    toupper(raw)
  )
}

fpa_ai_fmt_eur <- function(x, mark = "€") {
  x <- suppressWarnings(as.numeric(x))
  if (!is.finite(x)) return("n/a")
  neg <- x < 0
  txt <- format(round(abs(x)), big.mark = " ", scientific = FALSE, trim = TRUE)
  if (neg) txt <- paste0("-", txt)
  mark <- as.character(mark %||% "")
  if (!nzchar(mark)) txt else paste(txt, mark)
}

#' User statement labels: functional id -> the names from the income statement or the balance sheet.
#' Several names on one id are the lines the engine added together.
fpa_ai_labels_build <- function(pnl_map = NULL, bs_map = NULL) {
  acc <- list()
  subs <- character()
  add <- function(map) {
    if (is.null(map) || !length(map)) return()
    ids <- as.character(unname(map))
    nms <- names(map)
    if (is.null(nms)) return()
    for (i in seq_along(ids)) {
      id <- ids[[i]]
      lab <- trimws(as.character(nms[[i]] %||% ""))
      if (!nzchar(id) || !nzchar(lab)) next
      if (id %in% c("SECTION", "IGNORE")) next
      if (identical(id, "SUBTOTAL")) {
        subs <<- c(subs, lab)
        next
      }
      acc[[id]] <<- unique(c(acc[[id]], lab))
    }
  }
  add(pnl_map)
  add(bs_map)
  attr(acc, "subtotals") <- unique(subs)
  acc
}

fpa_ai_line_name <- function(id, labels = NULL) {
  id <- as.character(id %||% "")
  if (!nzchar(id)) return("")
  if (is.null(labels)) labels <- list()
  hit <- labels[[id]]
  if (length(hit)) {
    labs <- hit[nzchar(as.character(hit))]
    if (length(labs)) {
      name <- paste(labs, collapse = " + ")
      if (length(labs) > 1L) name <- paste0(name, " (lignes regroupées)")
      return(name)
    }
  }
  subs <- attr(labels, "subtotals")
  if (length(subs) && exists("fpa_fold_label", mode = "function")) {
    folded <- vapply(as.character(subs), fpa_fold_label, character(1))
    pat <- switch(id,
      OI = "resultat d exploitation",
      NI = "resultat net",
      NULL
    )
    if (!is.null(pat)) {
      i <- which(grepl(pat, folded, fixed = TRUE))
      if (length(i)) return(as.character(subs[[i[[1]]]]))
    }
  }
  computed <- c(
    OI = "Résultat d'exploitation",
    NI = "Résultat net",
    GP = "Marge brute",
    EBITDA = "EBITDA"
  )
  if (id %in% names(computed)) return(unname(computed[[id]]))
  if (exists("fpa_line_fr", mode = "function")) {
    lab <- tryCatch(fpa_line_fr(id), error = function(e) id)
    if (nzchar(lab) && !identical(lab, id)) return(lab)
  }
  id
}

fpa_ai_fmt_pct <- function(x, base) {
  x <- suppressWarnings(as.numeric(x))
  base <- suppressWarnings(as.numeric(base))
  if (!is.finite(x) || !is.finite(base) || abs(base) < 1) return("n/a")
  sprintf("%.1f%%", 100 * x / base)
}

fpa_ai_render_text <- function(text, mode = "sans", base = NA_real_, mark = "€") {
  text <- as.character(text %||% "")
  mode <- fpa_ai_mode_norm(mode)
  hit <- gregexpr("\\{\\{e:(-?[0-9.]+)\\}\\}", text, perl = TRUE)
  if (hit[[1]][[1]] >= 0) {
    m <- regmatches(text, hit)[[1]]
    nums <- as.numeric(sub("^\\{\\{e:", "", sub("\\}\\}$", "", m)))
    repl <- if (identical(mode, "avec")) {
      vapply(nums, function(n) fpa_ai_fmt_eur(n, mark), "")
    } else {
      vapply(nums, function(n) fpa_ai_fmt_pct(n, base), "")
    }
    for (i in seq_along(m)) text <- sub(m[[i]], repl[[i]], text, fixed = TRUE)
  }
  hit2 <- gregexpr("\\{\\{t:(-?[0-9.]+)\\}\\}", text, perl = TRUE)
  if (hit2[[1]][[1]] < 0) return(text)
  m2 <- regmatches(text, hit2)[[1]]
  nums2 <- as.numeric(sub("^\\{\\{t:", "", sub("\\}\\}$", "", m2)))
  repl2 <- if (identical(mode, "avec")) {
    sprintf("%.4f", nums2)
  } else {
    vapply(nums2, function(n) {
      if (!is.finite(base) || abs(base) < 1) "na" else sprintf("%.4f", 100 * n / base)
    }, "")
  }
  for (i in seq_along(m2)) text <- sub(m2[[i]], repl2[[i]], text, fixed = TRUE)
  text
}

fpa_ai_line <- function(...) paste(..., collapse = "")

fpa_ai_pnl_block <- function(pnl, labels = NULL) {
  if (is.null(pnl) || !nrow(pnl)) return("No income statement.")
  need <- c("periode", "REV", "OI", "NI")
  if (!all(need %in% names(pnl))) return("Income statement columns are missing.")
  pnl <- pnl[order(pnl$periode), , drop = FALSE]
  rows <- utils::tail(pnl, 6)
  rev_l <- fpa_ai_line_name("REV", labels)
  oi_l <- fpa_ai_line_name("OI", labels)
  ni_l <- fpa_ai_line_name("NI", labels)
  bits <- vapply(seq_len(nrow(rows)), function(i) {
    r <- rows[i, ]
    sprintf(
      "%s : %s %s, %s %s, %s %s",
      r$periode, rev_l, fpa_ai_eur_token(r$REV), oi_l, fpa_ai_eur_token(r$OI), ni_l, fpa_ai_eur_token(r$NI)
    )
  }, "")
  paste(bits, collapse = "\n")
}

fpa_ai_cash_block <- function(cash, labels = NULL) {
  if (is.null(cash) || !nrow(cash)) return("No cash bridge.")
  need <- c("periode", "cash_open", "cash_close", "NI", "D_A", "wc_impact", "capex", "debt_net")
  if (!all(need %in% names(cash))) return("Cash columns are missing.")
  cash <- cash[order(cash$periode), , drop = FALSE]
  r <- cash[nrow(cash), ]
  open <- as.numeric(r$cash_open)
  close <- as.numeric(r$cash_close)
  steps <- sum(as.numeric(r$NI), as.numeric(r$D_A), as.numeric(r$wc_impact), as.numeric(r$capex), as.numeric(r$debt_net), na.rm = TRUE)
  extra <- if ("wc_opening_release" %in% names(r)) as.numeric(r$wc_opening_release) else 0
  if (!is.finite(extra)) extra <- 0
  steps <- steps + extra
  paste(
    sprintf(
      "%s : solde d'ouverture %s, %s %s, %s %s, besoin en fonds de roulement %s, investissement %s, dette %s, solde de clôture %s.",
      r$periode,
      fpa_ai_eur_token(open),
      fpa_ai_line_name("NI", labels), fpa_ai_eur_token(r$NI),
      fpa_ai_line_name("D_A", labels), fpa_ai_eur_token(r$D_A),
      fpa_ai_eur_token(r$wc_impact), fpa_ai_eur_token(r$capex), fpa_ai_eur_token(r$debt_net),
      fpa_ai_eur_token(close)
    ),
    sprintf(
      "tie open=%s close=%s steps=%s",
      sprintf("{{t:%.4f}}", open), sprintf("{{t:%.4f}}", close), sprintf("{{t:%.4f}}", steps)
    ),
    sep = "\n"
  )
}

fpa_ai_variance_block <- function(variance, labels = NULL) {
  if (is.null(variance) || !nrow(variance)) return("No variance.")
  if (!all(c("periode", "line_id", "variance") %in% names(variance))) return("Variance columns are missing.")
  last <- max(as.character(variance$periode))
  sub <- variance[as.character(variance$periode) == last, , drop = FALSE]
  sub$abs <- abs(suppressWarnings(as.numeric(sub$variance)))
  sub <- sub[order(-sub$abs), , drop = FALSE]
  sub <- utils::head(sub, 5)
  bits <- vapply(seq_len(nrow(sub)), function(i) {
    r <- sub[i, ]
    price <- if ("price" %in% names(r)) fpa_ai_eur_token(r$price) else "n/a"
    volume <- if ("volume" %in% names(r)) fpa_ai_eur_token(r$volume) else "n/a"
    sprintf(
      "%s : écart %s (prix %s, volume %s)",
      fpa_ai_line_name(r$line_id, labels), fpa_ai_eur_token(r$variance), price, volume
    )
  }, "")
  paste(c(paste("Dernier mois", last), bits), collapse = "\n")
}

fpa_ai_voted_revenue <- function(pnl, months = NULL) {
  if (is.null(pnl) || !nrow(pnl) || !"REV" %in% names(pnl)) return(NA_real_)
  if (length(months)) pnl <- pnl[as.character(pnl$periode) %in% as.character(months), , drop = FALSE]
  if (!nrow(pnl)) return(NA_real_)
  sum(suppressWarnings(as.numeric(pnl$REV)), na.rm = TRUE)
}

fpa_ai_missing <- function() "Not included in this request."

#' Build named text blocks. The company name is never included.
fpa_ai_context <- function(books, company = "", closed = "", floor = NA_real_, labels = NULL, currency = "EUR", months = NULL, cutoff = "") {
  pnl <- books$pnl
  if (length(months) && !is.null(pnl) && nrow(pnl) && "periode" %in% names(pnl)) {
    pnl <- pnl[as.character(pnl$periode) %in% as.character(months), , drop = FALSE]
  }
  base <- fpa_ai_voted_revenue(pnl)
  mark <- fpa_ai_currency_mark(currency)
  blocks <- list(
    scope = paste(
      if (nzchar(closed)) paste("Mois clos jusqu'à", closed) else "Mois clos non indiqué.",
      if (length(months)) paste("Exercice", substr(months[[1]], 1L, 4L), "·", length(months), "mois.") else "",
      sprintf("Base %s de l'exercice %s.", fpa_ai_line_name("REV", labels), fpa_ai_eur_token(base))
    ),
    headline = fpa_ai_pnl_block({
      row <- pnl
      if (!is.null(row) && nrow(row) && nzchar(cutoff) && cutoff %in% as.character(row$periode)) {
        row <- row[as.character(row$periode) == cutoff, , drop = FALSE]
      } else {
        row <- utils::tail(row, 1)
      }
      row
    }, labels),
    pnl_months = fpa_ai_pnl_block(pnl, labels),
    variance_top = fpa_ai_variance_block(books$variance, labels),
    landing = if (is.null(books$landing)) fpa_ai_missing() else as.character(books$landing),
    cash = fpa_ai_cash_block({
      cash <- books$cash
      if (length(months) && !is.null(cash) && nrow(cash) && "periode" %in% names(cash)) {
        cash <- cash[as.character(cash$periode) %in% as.character(months), , drop = FALSE]
        if (nzchar(cutoff) && cutoff %in% as.character(cash$periode)) {
          cash <- cash[as.character(cash$periode) == cutoff, , drop = FALSE]
        }
      }
      cash
    }, labels),
    bs = if (is.null(books$bs_note)) fpa_ai_missing() else as.character(books$bs_note),
    health = if (is.null(books$health_note)) fpa_ai_missing() else as.character(books$health_note),
    anomalies = if (is.null(books$anomalies_note)) fpa_ai_missing() else as.character(books$anomalies_note),
    close = if (is.null(books$close_note)) fpa_ai_missing() else as.character(books$close_note),
    drivers = if (is.null(books$drivers_note)) fpa_ai_missing() else as.character(books$drivers_note),
    scenarios = if (is.null(books$scenarios_note)) fpa_ai_missing() else as.character(books$scenarios_note),
    sensitivity = if (is.null(books$sensitivity_note)) fpa_ai_missing() else as.character(books$sensitivity_note),
    journal = if (is.null(books$journal_note)) fpa_ai_missing() else as.character(books$journal_note)
  )
  if (!is.null(books$bs) && nrow(books$bs) && all(c("periode", "CASH", "EQUITY") %in% names(books$bs))) {
    bs <- books$bs[order(books$bs$periode), , drop = FALSE]
    r <- bs[nrow(bs), ]
    blocks$bs <- sprintf(
      "%s : %s %s, %s %s.",
      r$periode,
      fpa_ai_line_name("CASH", labels), fpa_ai_eur_token(r$CASH),
      fpa_ai_line_name("EQUITY", labels), fpa_ai_eur_token(r$EQUITY)
    )
  }
  if (is.finite(floor)) {
    blocks$cash <- paste(blocks$cash, sprintf("Plancher %s.", fpa_ai_eur_token(floor)))
  }
  attr(blocks, "base") <- base
  attr(blocks, "mark") <- mark
  blocks
}

fpa_ai_render_blocks <- function(blocks, mode = "sans", company = "") {
  base <- attr(blocks, "base")
  mark <- attr(blocks, "mark") %||% "€"
  mode <- fpa_ai_mode_norm(mode)
  out <- lapply(blocks, function(text) {
    text <- fpa_ai_render_text(text, mode, base, mark)
    if (identical(mode, "sans")) {
      text <- gsub("\\{\\{c:[^}]*\\}\\}", "the company", text, perl = TRUE)
    } else {
      text <- gsub("\\{\\{c:([^}]*)\\}\\}", "\\1", text, perl = TRUE)
    }
    text
  })
  attr(out, "base") <- base
  attr(out, "mode") <- mode
  out
}

fpa_ai_slice_text <- function(blocks, keys = NULL, cap = 12000L) {
  if (!length(keys)) keys <- names(blocks)
  keys <- intersect(as.character(keys), names(blocks))
  parts <- vapply(keys, function(k) paste0("[", k, "]\n", blocks[[k]]), "")
  text <- paste(parts, collapse = "\n\n")
  if (nchar(text) <= cap) return(text)
  paste0(substr(text, 1L, cap), "\n[cut] The context was cut at ", cap, " characters.")
}

fpa_ai_system_prompt <- function(mode = "sans", lang = "fr", mark = "€", reconciled = FALSE) {
  mode <- fpa_ai_mode_norm(mode)
  lang <- if (identical(lang, "en")) "en" else "fr"
  base <- paste(
    if (isTRUE(reconciled)) {
      "You are a senior FP&A analyst. The balance-sheet check for this pack is consistent."
    } else {
      "You are a senior FP&A analyst reading a calculated view. A missing block was not included in this request. Do not treat that as a fact about the company."
    },
    "Use only the figures in the context. If a figure is missing, say so.",
    "The phrase \"Not included in this request.\" means the application did not send that block.",
    "Answer in four short parts: observed fact with its period and source, explained contribution, hypothesis to validate (label it as a hypothesis), suggested next action.",
    "Prefer a range to a false point. Stay under 400 words.",
    "Name a line only with the label written in the context. Do not use an internal code and do not substitute another accounting term.",
    if (identical(lang, "fr")) "Write in French." else "Write in English."
  )
  if (identical(mode, "sans")) {
    base <- paste(base, "Figures are percentages of annual revenue. Do not estimate money amounts and do not write a currency mark.")
  } else {
    base <- paste(base, sprintf(
      "Every money figure ends with %s, with a space before the mark, as in 1 977 %s. Never write a money figure without that mark.",
      mark, mark
    ))
  }
  base
}

fpa_ai_item <- function(cluster, id, tab, fr, en, recipe_fr, recipe_en, slice) {
  list(
    cluster = cluster, id = id, tab = tab,
    label = c(fr = fr, en = en),
    recipe = c(fr = recipe_fr, en = recipe_en),
    slice = slice
  )
}

fpa_ai_catalog <- function() {
  list(
    fpa_ai_item("direction", "dir_three", "Synthèse",
      "Note en trois points", "Three-point note",
      "Écris une note de trois points sur le mois clos.",
      "Write a three-point note on the closed month.",
      c("scope", "headline", "pnl_months")),
    fpa_ai_item("direction", "dir_changed", "Synthèse",
      "Ce qui a changé depuis la version enregistrée", "What changed since the saved version",
      "Dis ce qui a changé depuis la version enregistrée.",
      "Say what changed since the saved version.",
      c("scope", "headline", "close")),
    fpa_ai_item("direction", "dir_one", "Synthèse",
      "Le chiffre à surveiller", "The one number to watch",
      "Choisis le seul chiffre que la direction doit surveiller, et dis pourquoi.",
      "Pick the one number management should watch, and say why.",
      c("scope", "headline", "cash")),
    fpa_ai_item("direction", "dir_board", "Synthèse",
      "Questions du conseil", "Questions the board will ask",
      "Liste les questions que le conseil posera sur ces chiffres.",
      "List the questions the board will ask about these figures.",
      c("scope", "headline", "variance_top", "cash")),
    fpa_ai_item("variance", "var_why", "Écarts",
      "Pourquoi l'écart au budget", "Why we are off budget",
      "Explique l'écart au budget.",
      "Explain the gap to budget.",
      c("scope", "variance_top", "pnl_months")),
    fpa_ai_item("variance", "var_split", "Écarts",
      "Prix, volume ou calendrier", "Price, volume, or timing",
      "Sépare l'écart entre prix, volume et calendrier.",
      "Split the gap into price, volume, and timing.",
      c("variance_top")),
    fpa_ai_item("variance", "var_three", "Écarts",
      "Les trois écarts qui comptent", "The three gaps that matter",
      "Garde les trois écarts qui comptent et laisse le reste.",
      "Keep the three gaps that matter and leave the rest.",
      c("variance_top", "headline")),
    fpa_ai_item("variance", "var_line", "Écarts",
      "Commentaire de gestion, ligne par ligne", "Line-by-line management comment",
      "Rédige un commentaire de gestion court pour chaque ligne en écart.",
      "Write a short management comment for each line that is off.",
      c("variance_top")),
    fpa_ai_item("landing", "land_year", "Scénarios",
      "Où l'année atterrit", "Where the year lands",
      "Dis où l'année atterrit par rapport au budget voté.",
      "Say where the year lands against the voted budget.",
      c("scope", "landing", "pnl_months")),
    fpa_ai_item("landing", "land_band", "Scénarios",
      "Prudent et ambitieux", "Cautious and ambitious",
      "Dis ce qui sépare le chemin prudent du chemin ambitieux.",
      "Say what separates the cautious path from the ambitious path.",
      c("scenarios", "landing")),
    fpa_ai_item("landing", "land_hold", "Scénarios",
      "Tenir le budget", "What it takes to hold the budget",
      "Dis ce qu'il faudrait pour tenir le budget voté.",
      "Say what it would take to hold the voted budget.",
      c("landing", "variance_top", "drivers")),
    fpa_ai_item("landing", "land_risk", "Scénarios",
      "Risques et occasions", "Risks and opportunities",
      "Liste les risques et les occasions encore ouverts sur l'année.",
      "List the risks and opportunities still open for the year.",
      c("scenarios", "anomalies", "journal")),
    fpa_ai_item("cash", "cash_end", "Trésorerie",
      "Trésorerie de fin d'année et point bas", "Year-end cash and the low point",
      "Dis où sera la trésorerie en fin d'année et si le point bas passe sous le plancher.",
      "Say where cash will stand at year-end and whether the low point breaks the floor.",
      c("cash", "scope")),
    fpa_ai_item("cash", "cash_uses", "Trésorerie",
      "Ce qui emploie la trésorerie", "What uses the cash",
      "Explique le pont de trésorerie : ce qui entre et ce qui sort.",
      "Explain the cash bridge: what comes in and what goes out.",
      c("cash")),
    fpa_ai_item("cash", "cash_wc", "Trésorerie",
      "Clients, stocks, fournisseurs", "Customers, stock, suppliers",
      "Dis comment le besoin en fonds de roulement bouge.",
      "Say how working capital is moving.",
      c("cash", "bs")),
    fpa_ai_item("cash", "cash_profit", "Trésorerie",
      "Résultat et trésorerie", "Profit and cash",
      "Explique l'écart entre le résultat et la trésorerie.",
      "Explain the gap between profit and cash.",
      c("headline", "cash")),
    fpa_ai_item("close", "close_ready", "Clôture",
      "La clôture est-elle prête", "Is the close ready",
      "Dis si la clôture du mois est prête, et ce qui manque.",
      "Say whether the month close is ready, and what is missing.",
      c("close", "anomalies")),
    fpa_ai_item("close", "close_anom", "Anomalies",
      "Anomalies à traiter d'abord", "Anomalies to clear first",
      "Classe les anomalies à traiter avant de figer le mois.",
      "Rank the anomalies to clear before the month is locked.",
      c("anomalies")),
    fpa_ai_item("close", "close_drift", "Clôture",
      "Dérive depuis la version enregistrée", "Drift since the saved version",
      "Décris la dérive depuis la version enregistrée.",
      "Describe the drift since the saved version.",
      c("close", "headline")),
    fpa_ai_item("close", "close_checks", "Bilan",
      "Contrôles de cohérence", "Consistency checks",
      "Vérifie l'identité du bilan et le rattachement du pont de trésorerie.",
      "Check the balance-sheet identity and that the cash bridge ties.",
      c("bs", "cash")),
    fpa_ai_item("drivers", "drv_trust", "Drivers",
      "Quels drivers méritent confiance", "Which drivers deserve trust",
      "Dis quels drivers méritent confiance et lesquels non.",
      "Say which drivers deserve trust and which do not.",
      c("drivers")),
    fpa_ai_item("drivers", "drv_lag", "Drivers",
      "Décalage et horizon", "Lag and horizon",
      "Dis ce que le décalage et l'horizon changent pour la prévision.",
      "Say what the lag and the horizon change for the forecast.",
      c("drivers")),
    fpa_ai_item("drivers", "drv_gap", "Drivers",
      "Ce que le modèle ne couvre pas", "What the model does not cover",
      "Dis ce que le modèle ne couvre pas.",
      "Say what the model does not cover.",
      c("drivers", "anomalies")),
    fpa_ai_item("health", "health_check", "Santé",
      "État de santé", "Health check",
      "Fais un état de santé à partir des ratios.",
      "Give a health check from the ratios.",
      c("health", "headline")),
    fpa_ai_item("health", "health_liq", "Santé",
      "Liquidité et marge de sécurité", "Liquidity and safety margin",
      "Juge la liquidité et la marge de sécurité.",
      "Judge liquidity and the safety margin.",
      c("health", "cash")),
    fpa_ai_item("health", "health_bank", "Santé",
      "Lecture d'un banquier", "How a banker would read it",
      "Lis ces ratios comme un banquier.",
      "Read these ratios as a banker would.",
      c("health", "bs")),
    fpa_ai_item("decisions", "dec_levers", "Impact",
      "Effet des leviers essayés", "Effect of the levers tried",
      "Décris l'effet des leviers déjà essayés.",
      "Describe the effect of the levers already tried.",
      c("sensitivity", "scenarios")),
    fpa_ai_item("decisions", "dec_sens", "Impact",
      "Les trois sensibilités les plus fortes", "The three strongest sensitivities",
      "Garde les trois sensibilités les plus fortes.",
      "Keep the three strongest sensitivities.",
      c("sensitivity")),
    fpa_ai_item("decisions", "dec_journal", "Journal",
      "Suite des décisions du journal", "Follow-up of journal decisions",
      "Dis quelles décisions du journal sont à revoir avec ces chiffres.",
      "Say which journal decisions should be reviewed against these figures.",
      c("journal", "headline"))
  )
}

fpa_ai_catalog_item <- function(id) {
  cat <- fpa_ai_catalog()
  hit <- Filter(function(x) identical(x$id, id), cat)
  if (!length(hit)) NULL else hit[[1]]
}

fpa_ai_pick <- function(item, field, lang = "fr") {
  lang <- if (identical(lang, "en")) "en" else "fr"
  val <- item[[field]]
  if (is.null(val)) return("")
  out <- unname(val[[lang]])
  if (!length(out) || !nzchar(out[[1]])) out <- unname(val[["fr"]])
  if (!length(out)) "" else out[[1]]
}

fpa_ai_read_csv <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path)) return(NULL)
  tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"), error = function(e) NULL)
}

fpa_ai_books_from_home <- function(home) {
  work <- file.path(home, "work")
  list(
    pnl = fpa_ai_read_csv(file.path(work, "pnl-monthly.csv")),
    cash = fpa_ai_read_csv(file.path(work, "cash-bridge-monthly.csv")),
    variance = fpa_ai_read_csv(file.path(work, "variance-monthly.csv")),
    bs = fpa_ai_read_csv(file.path(work, "bs-monthly.csv"))
  )
}

fpa_ai_books_enrich <- function(books, labels = NULL) {
  if (is.null(books$bs) || !nrow(books$bs) || is.null(books$pnl) || !nrow(books$pnl)) return(books)
  per <- max(as.character(books$bs$periode))
  if (exists("fpa_health_month", mode = "function")) {
    h <- tryCatch(fpa_health_month(books$bs, books$pnl, per), error = function(e) NULL)
    if (!is.null(h) && nrow(h) && all(c("id", "value") %in% names(h))) {
      books$health_note <- paste(vapply(seq_len(nrow(h)), function(i) {
        v <- suppressWarnings(as.numeric(h$value[[i]]))
        sprintf("%s %s", h$id[[i]], if (is.finite(v)) sprintf("%.2f", v) else "n/a")
      }, ""), collapse = "\n")
    }
  }
  if (exists("fpa_anomaly_scan", mode = "function")) {
    a <- tryCatch(fpa_anomaly_scan(books$bs, books$pnl, books$cash), error = function(e) NULL)
    if (!is.null(a) && nrow(a)) {
      books$anomalies_note <- paste(vapply(seq_len(min(8L, nrow(a))), function(i) {
        sprintf(
          "%s %s %s écart %s",
          a$periode[[i]], a$family[[i]], fpa_ai_line_name(a$line_id[[i]], labels), fpa_ai_eur_token(a$gap[[i]])
        )
      }, ""), collapse = "\n")
    }
  }
  books
}

fpa_ai_closed_through <- function(home) {
  st <- fpa_ai_read_csv(file.path(home, "work", "period-status.csv"))
  if (is.null(st) || !nrow(st) || !all(c("periode", "status") %in% names(st))) return("")
  hit <- st[fpa_period_status_norm(st$status) == "actual", , drop = FALSE]
  if (!nrow(hit)) return("")
  max(as.character(hit$periode))
}

fpa_ai_user_message <- function(recipe, blocks, slice = NULL) {
  paste(
    "Recipe:", recipe, "",
    "Context:", fpa_ai_slice_text(blocks, slice),
    sep = "\n"
  )
}
