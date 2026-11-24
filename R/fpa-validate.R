# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-validate.R — Plain-French findings; no R traceback to the user surface
# LAST CHANGE: 2026-09-18 — L1/L2

#' Create an empty findings data.frame.
fpa_findings_empty <- function() {
  data.frame(
    code = character(),
    severity = character(),  # error | warning | info
    message = character(),
    stringsAsFactors = FALSE
  )
}

fpa_finding <- function(code, severity, message) {
  data.frame(code = code, severity = severity, message = message, stringsAsFactors = FALSE)
}

#' Validate read results + opening BS balance + month integrity.
fpa_validate <- function(config, reads, opening_bs_vec = NULL) {
  findings <- fpa_findings_empty()

  # Missing files
  for (role in names(reads)) {
    r <- reads[[role]]
    if (!isTRUE(r$ok)) {
      label <- switch(role,
        pnl_actuals = "compte de resultat historique",
        pnl_monthly = "compte de resultat mensuel",
        opening_bs = "bilan d'ouverture",
        budget = "budget",
        role
      )
      if (identical(r$error, "missing")) {
        findings <- rbind(findings, fpa_finding(
          "missing_file", "error",
          paste0(
            fpa_txt("loc_file_of_the"), label, " est introuvable: ",
            r$path %||% "(chemin vide)",
            fpa_txt("loc_check_config_path")
          )
        ))
      } else {
        findings <- rbind(findings, fpa_finding(
          "read_fail", "error",
          paste0(
            fpa_txt("loc_cannot_read_file"), label, " (",
            basename(r$path %||% ""), "). ",
            r$message %||% "Format non reconnu."
          )
        ))
      }
    }
  }

  # P&L month integrity
  if (!is.null(reads$pnl_actuals) && isTRUE(reads$pnl_actuals$ok) &&
      !is.null(reads$pnl_actuals$mapped) && nrow(reads$pnl_actuals$mapped)) {
    m <- reads$pnl_actuals$mapped
    if (identical(reads$pnl_actuals$grain, "month")) {
      m$periode_key <- format(as.Date(m$period_end), "%Y-%m")
      keys <- sort(unique(m$periode_key))
      key_dup <- paste(m$functional_label, m$periode_key, sep = "|")
      if (any(duplicated(key_dup))) {
        findings <- rbind(findings, fpa_finding(
          "duplicate_month", "error",
          fpa_txt("loc_duplicate_months")
        ))
      }
      if (length(keys) >= 2) {
        start <- as.Date(paste0(keys[[1]], "-01"))
        end <- as.Date(paste0(keys[[length(keys)]], "-01"))
        want <- format(seq(start, end, by = "month"), "%Y-%m")
        missing <- setdiff(want, keys)
        if (length(missing)) {
          findings <- rbind(findings, fpa_finding(
            "month_gap", "error",
            paste0(
              fpa_txt("loc_months_missing_pnl"),
              paste(missing, collapse = ", "), "."
            )
          ))
        }
      }
    } else if (identical(reads$pnl_actuals$grain, "year")) {
      has_monthly <- !is.null(reads$pnl_monthly) && isTRUE(reads$pnl_monthly$ok) &&
        !is.null(reads$pnl_monthly$mapped) && nrow(reads$pnl_monthly$mapped) > 0
      if (!has_monthly) {
        findings <- rbind(findings, fpa_finding(
          "annual_only", "info",
          paste0(
            fpa_txt("loc_pnl_is_annual"),
            "pas un suivi du rolling. Le moteur ne divise pas l'annee par douze. ",
            fpa_txt("loc_bring_a_living_file")
          )
        ))
      }
    }

    # Sign-flipped cost heuristic: COGS/PAYROLL negative while REV positive in same period
    for (pe in unique(m$period_end)) {
      slice <- m[m$period_end == pe, , drop = FALSE]
      rev <- sum(slice$montant[slice$functional_label == "REV"], na.rm = TRUE)
      cogs <- sum(slice$montant[slice$functional_label == "COGS"], na.rm = TRUE)
      if (rev > 0 && cogs < 0) {
        findings <- rbind(findings, fpa_finding(
          "sign_flip", "warning",
          paste0(
            fpa_txt("loc_cogs_negative"), pe,
            " alors que le CA est positif. Verifiez le signe (couts en valeur absolue)."
          )
        ))
      }
    }
  }

  # Budget unmatched lines
  if (!is.null(reads$budget) && isTRUE(reads$budget$ok) &&
      !is.null(reads$budget$long) && !is.null(reads$pnl_actuals$long)) {
    bmap <- fpa_line_map(config, "budget")
    for (lab in unique(reads$budget$long$libelle)) {
      if (!nzchar(as.character(lab))) next
      fl <- fpa_lookup_line(lab, bmap)
      if (is.na(fl) || !nzchar(fl)) {
        findings <- rbind(findings, fpa_finding(
          "unmatched_budget", "warning",
          paste0(
            fpa_txt("loc_budget_line_unmatched"), lab, "\"."
          )
        ))
      }
    }

    # Coverage: an actual line the budget does not carry is compared to zero,
    # never dropped. Report it with its amount, which only exists here.
    if (!is.null(reads$pnl_actuals$mapped) && !is.null(reads$budget$mapped)) {
      derived <- c("D_A", "FIN", "TAX")
      actual_ids <- unique(reads$pnl_actuals$mapped$functional_label)
      budget_ids <- unique(reads$budget$mapped$functional_label)
      gaps <- setdiff(setdiff(actual_ids, budget_ids), derived)
      for (id in gaps) {
        amt <- sum(
          reads$pnl_actuals$mapped$montant[
            reads$pnl_actuals$mapped$functional_label == id
          ],
          na.rm = TRUE
        )
        findings <- rbind(findings, fpa_finding(
          "budget_gap", "info",
          paste0(
            fpa_txt("loc_actual_without_budget"), id, ", ",
            format(round(amt), big.mark = " ", scientific = FALSE),
            fpa_txt("loc_budget_read_zero")
          )
        ))
      }
    }
  }

  # Opening BS balance
  if (!is.null(opening_bs_vec) && length(opening_bs_vec)) {
    bal <- fpa_bs_balance_check(opening_bs_vec)
    if (!bal$ok) {
      findings <- rbind(findings, fpa_finding(
        "bs_unbalanced", "error",
        bal$message
      ))
    }
  } else if (!is.null(reads$opening_bs) && isTRUE(reads$opening_bs$ok) &&
             !is.null(reads$opening_bs$mapped) && nrow(reads$opening_bs$mapped)) {
    vec <- fpa_mapped_to_bs_vec(reads$opening_bs$mapped)
    bal <- fpa_bs_balance_check(vec)
    if (!bal$ok) {
      findings <- rbind(findings, fpa_finding(
        "bs_unbalanced", "error",
        bal$message
      ))
    }
  }

  # Two clocks: a closed-year annual P&L is not a rolling feed.
  # history_only extras are correlation/driver only: they do not count.
  has_rolling <- FALSE
  for (s in config$sources %||% list()) {
    if (isTRUE(fpa_source_is_rolling(s))) {
      has_rolling <- TRUE
      break
    }
  }
  if (!isTRUE(has_rolling)) {
    findings <- rbind(findings, fpa_finding(
      "no_rolling_feed", "warning",
      paste0(
        fpa_txt("loc_no_rolling"),
        fpa_txt("loc_annual_does_not_move"),
        "Un driver en historique seulement non plus. ",
        fpa_txt("loc_bring_one_living")
      )
    ))
  }

  pair_notes <- fpa_driver_pair_findings(config)
  for (fn in pair_notes) {
    findings <- rbind(findings, fn)
  }

  findings
}

#' Convert mapped BS rows (possibly one period) to a named numeric vector.
fpa_mapped_to_bs_vec <- function(mapped) {
  if (!nrow(mapped)) return(numeric(0))
  # Prefer opening / first period_end
  pe <- unique(mapped$period_end)
  pe0 <- pe[[1]]
  slice <- mapped[mapped$period_end == pe0, , drop = FALSE]
  setNames(slice$montant, slice$functional_label)
}

#' Check A = L + E. Amounts are absolute: assets vs liabilities+equity.
fpa_bs_balance_check <- function(vec, tol = 1) {
  assets <- 0
  liab <- 0
  eq <- 0
  for (nm in names(vec)) {
    v <- as.numeric(vec[[nm]])
    if (is.na(v)) next
    if (fpa_bs_is_asset(nm)) assets <- assets + v
    else if (fpa_bs_is_liability(nm)) liab <- liab + v
    else if (fpa_bs_is_equity(nm)) eq <- eq + v
  }
  gap <- assets - (liab + eq)
  out <- list(
    ok = abs(gap) <= tol,
    gap = gap,
    assets = assets,
    liabilities = liab,
    equity = eq,
    message = "Bilan d'ouverture equilibre."
  )
  if (isTRUE(out$ok)) return(out)
  if (gap > 0) {
    out$message <- paste0(
      "Le bilan d'ouverture ne balance pas: l'actif depasse le passif de ",
      format(round(gap, 2), big.mark = " "),
      " EUR. Cote court: passif / capitaux propres."
    )
  } else {
    out$message <- paste0(
      "Le bilan d'ouverture ne balance pas: le passif depasse l'actif de ",
      format(round(abs(gap), 2), big.mark = " "),
      " EUR. Cote court: actif."
    )
  }
  out
}

#' TRUE if findings contain any error severity.
fpa_has_errors <- function(findings) {
  any(findings$severity == "error")
}

#' Format findings for console (French).
fpa_format_findings <- function(findings) {
  if (!nrow(findings)) return("Aucune anomalie.")
  txt <- paste(
    sprintf("[%s] %s", toupper(findings$severity), findings$message),
    collapse = "\n"
  )
  Encoding(txt) <- "UTF-8"
  # R 4.2+ is UTF-8 internally; PowerShell/cmd often is not. RStudio can print accents.
  if (identical(.Platform$OS.type, "windows") &&
      !identical(.Platform$GUI, "RStudio")) {
    ascii <- iconv(txt, from = "UTF-8", to = "ASCII//TRANSLIT")
    if (!is.na(ascii) && nzchar(ascii)) return(ascii)
  }
  txt
}
