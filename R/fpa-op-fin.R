# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Op/Fin (ROIC) line nature from fpa-config.json.
# Optional. Absent or empty map => no Op/Fin UI. Engine never invents tags.

#' Normalise a nature token to op | fin | other, or NA if unusable.
fpa_nature_norm <- function(x) {
  v <- tolower(trimws(as.character(x %||% "")))
  if (!nzchar(v)) return(NA_character_)
  if (v %in% c("op", "operating", "exploitation", "ops")) return("op")
  if (v %in% c("fin", "financing", "finance", "financement")) return("fin")
  if (v %in% c("other", "autre", "others", "n/a", "na", "-")) return("other")
  NA_character_
}

#' Named character vector line_id -> op|fin|other for one statement.
#' Returns character(0) when the block is missing or has no usable tags.
fpa_line_nature <- function(config, statement = c("pnl", "bs", "cash")) {
  statement <- match.arg(statement)
  raw <- config$line_nature %||% NULL
  if (is.null(raw) || !length(raw)) return(character(0))
  block <- raw[[statement]] %||% NULL
  if (is.null(block) || !length(block)) return(character(0))
  nms <- names(block)
  if (is.null(nms) || !length(nms)) return(character(0))
  out <- character(0)
  for (i in seq_along(nms)) {
    id <- trimws(as.character(nms[[i]]))
    cat <- fpa_nature_norm(block[[i]])
    if (!nzchar(id) || is.na(cat)) next
    out[[id]] <- cat
  }
  out
}

#' TRUE when at least one usable Op/Fin tag exists for the statement.
fpa_line_nature_has <- function(config, statement = c("pnl", "bs", "cash")) {
  length(fpa_line_nature(config, statement)) > 0L
}

#' Look up nature for one line id (NA if unknown).
fpa_line_nature_of <- function(nature_map, id) {
  if (is.null(nature_map) || !length(nature_map)) return(NA_character_)
  id <- as.character(id %||% "")
  if (!nzchar(id) || !(id %in% names(nature_map))) return(NA_character_)
  as.character(nature_map[[id]])
}

#' Keep line under scope "all" or "op_fin" (op and fin only).
fpa_nature_keep <- function(id, nature_map, scope = "all") {
  scope <- as.character(scope %||% "all")
  if (!identical(scope, "op_fin")) return(TRUE)
  cat <- fpa_line_nature_of(nature_map, id)
  !is.na(cat) && cat %in% c("op", "fin")
}
