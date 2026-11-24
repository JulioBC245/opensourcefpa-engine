# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-labels.R — Evidence labels on every produced cell
# LAST CHANGE: 2026-09-18 — L1/L2

FPA_LABEL_IMPORTED <- "imported_actual"
FPA_LABEL_MECHANICAL <- "mechanical_rule"
FPA_LABEL_ASSUMPTION <- "management_assumption"
FPA_LABEL_MANUAL <- "saisie_manuelle"

#' Attach a single evidence label column to a data.frame.
fpa_with_label <- function(df, label) {
  df$evidence_label <- label
  df
}

#' Build a long labelled statement table from a wide month x line matrix.
#' @param mat named list or data.frame with periode + line columns
#' @param labels named character: line_id -> evidence label
fpa_label_statement <- function(mat, labels) {
  if (is.null(mat) || !nrow(mat)) {
    return(data.frame(
      periode = character(), line_id = character(), montant = numeric(),
      evidence_label = character(), stringsAsFactors = FALSE
    ))
  }
  line_cols <- setdiff(names(mat), c("periode", "period_end"))
  rows <- list()
  for (i in seq_len(nrow(mat))) {
    pe <- mat$periode[[i]]
    for (lc in line_cols) {
      lab <- if (is.function(labels)) {
        labels(lc, pe)
      } else {
        labels[[lc]] %||% FPA_LABEL_MECHANICAL
      }
      rows[[length(rows) + 1]] <- data.frame(
        periode = pe,
        line_id = lc,
        montant = as.numeric(mat[[lc]][[i]]),
        evidence_label = lab %||% FPA_LABEL_MECHANICAL,
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
