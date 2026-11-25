# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-lines.R — Engine line catalogue (kept in step with the opensourcefpa.com configurator)
# LAST CHANGE: 2026-09-21 — PCG suffix lookup (Impots vs Impots, taxes…)

FPA_LINE_CATALOG <- data.frame(
  id = c(
    "REV", "OTHER_OP_INC", "COGS", "PAYROLL", "OPEX_OTHER", "D_A", "FIN", "EXCEP", "TAX",
    "CASH", "AR", "INV", "AP", "PPE", "ST_DEBT", "LT_DEBT", "OTHER_CA", "OTHER_CL", "EQUITY",
    "SECTION", "SUBTOTAL", "IGNORE"
  ),
  fr = c(
    "Chiffre d'affaires / ventes",
    "Autres produits d'exploitation (hors CA)",
    "Achats / cout des ventes",
    "Masse salariale",
    "Autres charges d'exploitation",
    "Dotations aux amortissements",
    "Charges / produits financiers",
    "Produits / charges exceptionnels",
    "Impot sur les societes",
    "Tresorerie",
    "Creances clients",
    "Stocks",
    "Dettes fournisseurs",
    "Immobilisations nettes",
    "Dettes financieres court terme",
    "Dettes financieres long terme",
    "Autres actifs courants",
    "Autres passifs courants",
    "Capitaux propres",
    "En-tete de rubrique (plan)",
    "Sous-total / total (controle, avec niveau)",
    "Ne pas importer (colonne / hors perimetre)"
  ),
  family = c(
    rep("pnl", 9),
    rep("bs", 10),
    "check", "check", "skip"
  ),
  stringsAsFactors = FALSE
)

#' Binding role for a functional line id.
fpa_binding_role <- function(id) {
  if (identical(id, "SUBTOTAL")) return("subtotal")
  if (identical(id, "SECTION")) return("section")
  if (!nzchar(id %||% "") || identical(id, "IGNORE")) return("skip")
  "input"
}

#' P&L line ids that hold amounts (importable).
fpa_pnl_input_ids <- function() {
  FPA_LINE_CATALOG$id[FPA_LINE_CATALOG$family == "pnl"]
}

#' BS line ids that hold amounts.
fpa_bs_input_ids <- function() {
  FPA_LINE_CATALOG$id[FPA_LINE_CATALOG$family == "bs"]
}

#' Sign convention for P&L: costs are stored as positive amounts in import;
#' contribution to NI is negative for expense lines.
fpa_pnl_ni_sign <- function(id) {
  if (id %in% c("REV", "OTHER_OP_INC")) return(1)
  if (id %in% c("COGS", "PAYROLL", "OPEX_OTHER", "D_A", "FIN", "EXCEP", "TAX")) return(-1)
  0
}

#' Asset (+) / liability+equity (-) for A = L + E check using signed balances.
#' Assets positive; liabilities and equity positive on the credit side.
fpa_bs_is_asset <- function(id) {
  id %in% c("CASH", "AR", "INV", "PPE", "OTHER_CA")
}

fpa_bs_is_liability <- function(id) {
  id %in% c("AP", "ST_DEBT", "LT_DEBT", "OTHER_CL")
}

fpa_bs_is_equity <- function(id) {
  identical(id, "EQUITY")
}

#' French label for a line id.
fpa_line_fr <- function(id) {
  i <- match(id, FPA_LINE_CATALOG$id)
  if (is.na(i)) return(id)
  FPA_LINE_CATALOG$fr[[i]]
}

#' Fold a row label or header for matching (accents, case, punctuation).
fpa_fold_label <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- tolower(x)
  x <- iconv(x, from = "UTF-8", to = "ASCII//TRANSLIT")
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\s+", " ", x)
  trimws(ifelse(is.na(x), "", x))
}

#' Resolve a file row label against a named line_map (user_label -> functional).
#' Exact first, then folded equality, then longest folded prefix so a plan key
#' "Impots" maps the PCG caption "Impots, taxes et versements assimiles".
fpa_lookup_line <- function(lab, line_map) {
  if (is.null(line_map) || !length(line_map)) return(NA_character_)
  lab <- trimws(as.character(lab %||% ""))
  if (!nzchar(lab)) return(NA_character_)
  nms <- names(line_map)
  if (is.null(nms) || !length(nms)) return(NA_character_)
  if (lab %in% nms) {
    v <- unname(as.character(line_map[[lab]]))[[1]]
    return(if (nzchar(v)) v else NA_character_)
  }
  fold_lab <- fpa_fold_label(lab)
  if (!nzchar(fold_lab)) return(NA_character_)
  fold_keys <- vapply(nms, fpa_fold_label, character(1), USE.NAMES = FALSE)
  eq <- which(fold_keys == fold_lab)
  if (length(eq)) {
    v <- unname(as.character(line_map[[nms[[eq[[1]]]]]]))[[1]]
    return(if (nzchar(v)) v else NA_character_)
  }
  hit_i <- integer(0)
  hit_n <- integer(0)
  for (i in seq_along(nms)) {
    k <- fold_keys[[i]]
    if (nchar(k) < 4L) next
    if (startsWith(fold_lab, paste0(k, " "))) {
      hit_i <- c(hit_i, i)
      hit_n <- c(hit_n, nchar(k))
    }
  }
  if (!length(hit_i)) return(NA_character_)
  i <- hit_i[[which.max(hit_n)]]
  v <- unname(as.character(line_map[[nms[[i]]]]))[[1]]
  if (!nzchar(v)) NA_character_ else v
}

fpa_lookup_lines <- function(labs, line_map) {
  vapply(
    as.character(labs),
    function(lab) fpa_lookup_line(lab, line_map),
    character(1),
    USE.NAMES = FALSE
  )
}
