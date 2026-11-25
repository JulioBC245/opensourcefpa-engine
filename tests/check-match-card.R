# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# The match card reads mechanisms and scenario_ranges from the configuration.
root <- "."
source(file.path(root, "tests", "helpers.R"), encoding = "UTF-8")
source(file.path(root, "R/fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R/fpa-match.R"), encoding = "UTF-8")
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny missing")

hotel <- list(
  mechanisms = list(
    capacity_x_util_x_rate = TRUE,
    headcount_x_cost = TRUE,
    qty_x_price = FALSE,
    customers_x_spend = FALSE,
    effective_tax_rate = TRUE,
    fixed_monthly_expense = TRUE,
    dso_dpo_dio = TRUE,
    variable_cost_pct_or_unit = TRUE,
    revenue_growth_pct = FALSE,
    capex_schedule = TRUE,
    debt_schedule = TRUE
  ),
  scenario_ranges = list(inputs = list(
    list(id = "taux_utilisation", label = "Taux d'occupation / d'utilisation", mechanism = "capacity_x_util_x_rate", low_pct = 10, high_pct = 10),
    list(id = "tarif", label = "Tarif moyen", mechanism = "capacity_x_util_x_rate", low_pct = 10, high_pct = 10),
    list(id = "cout.variable.pct", label = "Coût variable", mechanism = "variable_cost_pct_or_unit", low_pct = 10, high_pct = 10),
    list(id = "cout_moyen", label = "Coût moyen chargé", mechanism = "headcount_x_cost", low_pct = 10, high_pct = 10),
    list(id = "charges.fixes", label = "Charges fixes", mechanism = "fixed_monthly_expense", low_pct = 10, high_pct = 10)
  ))
)

html <- paste(as.character(fpa_shiny_match_card(hotel)), collapse = "\n")
ok <- c(
  on_occ = grepl("Taux d'occupation / d'utilisation", html, fixed = TRUE),
  on_rate = grepl("Tarif moyen", html, fixed = TRUE),
  on_var = grepl("Coût variable", html, fixed = TRUE),
  on_cost = grepl("Coût moyen chargé", html, fixed = TRUE),
  on_fix = grepl("Charges fixes", html, fixed = TRUE),
  width = grepl("Largeur enregistrée", html, fixed = TRUE),
  held_head = grepl("Effectif", html, fixed = TRUE),
  held_tax = grepl("Taux d'impôt", html, fixed = TRUE),
  held_days = grepl("Délai clients", html, fixed = TRUE),
  off_qty = grepl("Quantité × prix", html, fixed = TRUE),
  off_word = grepl("n'est pas cochée", html, fixed = TRUE),
  no_volume_row = !grepl("Volume vendu", html, fixed = TRUE)
)
cat(sprintf("hotel %s/%s\n", sum(ok), length(ok)))
if (any(!ok)) {
  cat(paste(names(ok)[!ok], collapse = ", "), "\n")
  stop("hotel match card failed")
}

live_path <- fpa_optional_data_config()
if (!is.null(live_path) && requireNamespace("jsonlite", quietly = TRUE)) {
  live <- jsonlite::fromJSON(live_path, simplifyVector = FALSE)
  live_html <- paste(as.character(fpa_shiny_match_card(live)), collapse = "\n")
  inputs <- live$scenario_ranges$inputs
  n <- 0L
  hit <- 0L
  if (!is.null(inputs)) {
    for (x in inputs) {
      n <- n + 1L
      lab <- as.character(x$label %||% "")
      if (nzchar(lab) && grepl(lab, live_html, fixed = TRUE)) hit <- hit + 1L
    }
  }
  cat(sprintf("live labels %s/%s\n", hit, n))
  if (n > 0L && hit != n) stop("live config labels missing from the card")
} else {
  fpa_notify_skip_optional_data("live match card")
}
cat("match card ok\n")
