# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-sensitivity.R — One assumption at a time, plus the interaction residual.
# LAST CHANGE: 2026-09-21
# The residual is the gap between moving every assumption together and the sum
# of the one-at-a-time spans. It is reported on its own. It is not allocated.

fpa_assumption_family <- function(id) {
  id <- as.character(id)
  if (grepl("dso|dpo|dio|impot|dette|taux|cash", id)) return("finance")
  if (grepl("charge|cout|cogs|opex|payroll", id)) return("costs")
  if (grepl("capacite|effectif|volume|nuitee|utilisation", id)) return("operations")
  "commercial"
}

fpa_apply_assumption_override <- function(assum, override) {
  if (is.null(override) || !length(override)) return(assum)
  if (is.null(assum$wide) || !nrow(assum$wide)) return(assum)
  for (id in names(override)) {
    if (!(id %in% names(assum$wide))) assum$wide[[id]] <- NA_real_
    assum$wide[[id]] <- as.numeric(override[[id]])
  }
  assum
}

fpa_apply_line_scale <- function(pnl, line_scale, forecast_periods) {
  if (is.null(line_scale) || !length(line_scale) || !length(forecast_periods)) return(pnl)
  idx <- pnl$periode %in% forecast_periods
  if (!any(idx)) return(pnl)
  for (nm in names(line_scale)) {
    if (!(nm %in% names(pnl))) next
    pnl[[nm]][idx] <- pnl[[nm]][idx] * as.numeric(line_scale[[nm]])
  }
  fpa_pnl_derive(pnl)
}

#' Low-to-high span of operating income for each enabled assumption.
#' low and high default to 0.9 and 1.1 times the latest value when the caller
#' does not pass a grid.
fpa_sensitivity_run <- function(config_path, write = FALSE, factors = c(0.9, 1.1)) {
  config <- fpa_load_config(config_path)
  ids <- fpa_enabled_assumption_ids(config)
  base <- fpa_run(config_path, write = FALSE, stop_on_error = TRUE)
  empty <- data.frame(
    assumption_id = character(), family = character(),
    low = numeric(), high = numeric(),
    oi_low = numeric(), oi_high = numeric(), oi_span = numeric(),
    stringsAsFactors = FALSE
  )
  if (!length(ids) || is.null(base$assumptions$wide) || !nrow(base$assumptions$wide)) {
    return(list(rows = empty, interaction_residual = NA_real_, base_oi = sum(base$pnl$OI, na.rm = TRUE)))
  }
  wide <- base$assumptions$wide
  rows <- list()
  spans <- numeric(0)
  for (id in ids) {
    if (!(id %in% names(wide))) next
    latest <- wide[[id]][nrow(wide)]
    if (!is.finite(latest)) next
    low_v <- as.numeric(latest) * factors[[1]]
    high_v <- as.numeric(latest) * factors[[2]]
    low <- fpa_run(config_path, write = FALSE, stop_on_error = TRUE,
                   assumption_override = setNames(list(low_v), id))
    high <- fpa_run(config_path, write = FALSE, stop_on_error = TRUE,
                    assumption_override = setNames(list(high_v), id))
    oi_low <- sum(low$pnl$OI, na.rm = TRUE)
    oi_high <- sum(high$pnl$OI, na.rm = TRUE)
    span <- oi_high - oi_low
    spans[[id]] <- span
    rows[[length(rows) + 1L]] <- data.frame(
      assumption_id = id,
      family = fpa_assumption_family(id),
      low = low_v, high = high_v,
      oi_low = oi_low, oi_high = oi_high, oi_span = span,
      stringsAsFactors = FALSE
    )
  }
  out <- if (length(rows)) do.call(rbind, rows) else empty
  together <- stats::setNames(lapply(ids, function(id) {
    if (!(id %in% names(wide))) return(NULL)
    latest <- wide[[id]][nrow(wide)]
    if (!is.finite(latest)) return(NULL)
    as.numeric(latest) * factors[[2]]
  }), ids)
  together <- together[!vapply(together, is.null, logical(1))]
  residual <- NA_real_
  if (length(together)) {
    all_high <- fpa_run(config_path, write = FALSE, stop_on_error = TRUE,
                        assumption_override = together)
    moved <- sum(all_high$pnl$OI, na.rm = TRUE) - sum(base$pnl$OI, na.rm = TRUE)
    residual <- moved - sum(spans)
  }
  if (isTRUE(write) && nrow(out)) {
    paths <- fpa_paths(config)
    utils::write.csv(out, file.path(paths$work, "sensitivity.csv"), row.names = FALSE)
  }
  list(rows = out, interaction_residual = residual, base_oi = sum(base$pnl$OI, na.rm = TRUE))
}
