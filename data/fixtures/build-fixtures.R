# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Generate fictional 24-month fixture company (Exemple SAS). Run once from fpa-local root.
# LAST CHANGE: 2026-09-18

root <- normalizePath(".", winslash = "/")
if (!dir.exists(file.path(root, "R"))) stop("Run from fpa-local root")

indir <- file.path(root, "data", "fixtures", "input")
cfgdir <- file.path(root, "data", "fixtures", "config")
dir.create(indir, recursive = TRUE, showWarnings = FALSE)
dir.create(cfgdir, recursive = TRUE, showWarnings = FALSE)

months <- format(seq(as.Date("2024-01-01"), as.Date("2025-12-01"), by = "month"), "%Y-%m")

# --- Opening BS (balanced) ---
bs <- data.frame(
  libelle = c(
    "Tresorerie", "Clients", "Stocks", "Immobilisations nettes", "Autres actifs courants",
    "Fournisseurs", "Dettes CT", "Dettes LT", "Autres passifs courants", "Capitaux propres"
  ),
  montant = c(50000, 30000, 10000, 200000, 5000, 20000, 10000, 80000, 5000, 180000),
  stringsAsFactors = FALSE
)
write.csv(bs, file.path(indir, "bs-opening.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# --- Monthly P&L ---
rows <- list()
for (i in seq_along(months)) {
  p <- months[[i]]
  rev <- 100000 + (i - 1) * 500
  cogs <- round(rev * 0.35, 2)
  lines <- list(
    c("Chiffre d'affaires", rev),
    c("Achats", cogs),
    c("Salaires", 25000),
    c("Charges externes", 8000),
    c("Amortissements", 2000),
    c("Charges financieres", 500),
    c("Impot sur les societes", round(max(rev - cogs - 25000 - 8000 - 2000 - 500, 0) * 0.25, 2))
  )
  for (L in lines) {
    rows[[length(rows) + 1]] <- data.frame(
      libelle = L[[1]], periode = p, montant = as.numeric(L[[2]]),
      stringsAsFactors = FALSE
    )
  }
}
pnl <- do.call(rbind, rows)
write.csv(pnl, file.path(indir, "pnl.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Budget = same structure, +3% on REV
budget <- pnl
budget$montant[budget$libelle == "Chiffre d'affaires"] <-
  round(budget$montant[budget$libelle == "Chiffre d'affaires"] * 1.03, 2)
write.csv(budget, file.path(indir, "budget.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# --- Parameters ---
pars <- list()
for (p in months) {
  pars[[length(pars) + 1]] <- data.frame(
    periode = p,
    parametre = c("taux.impot", "dso", "dpo", "dio", "charge.fixe.loyer"),
    valeur = c(0.25, 45, 40, 30, 4500),
    stringsAsFactors = FALSE
  )
}
parametres <- do.call(rbind, pars)
write.csv(parametres, file.path(indir, "parametres.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# --- Decisions (empty base; gate tests add their own) ---
dec <- data.frame(
  date = as.Date(character()),
  libelle = character(),
  montant = numeric(),
  type = character(),
  life_months = numeric(),
  interest_rate = numeric(),
  break_cost = numeric(),
  stringsAsFactors = FALSE
)
write.csv(dec, file.path(indir, "investissements-emprunts.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# --- Volumes (hotel-style, optional) ---
vol_rows <- list()
for (p in months) {
  vol_rows[[length(vol_rows) + 1]] <- data.frame(
    periode = p,
    item = "chambre_standard",
    capacite = 40,
    taux_utilisation = 0.65,
    tarif = 95,
    effectif = 12,
    cout_moyen = 3100,
    stringsAsFactors = FALSE
  )
}
volumes <- do.call(rbind, vol_rows)
write.csv(volumes, file.path(indir, "volumes.csv"), row.names = FALSE, fileEncoding = "UTF-8")

# Absolute paths for config (fixtures stay portable via resolve fallback to basename)
root_win <- normalizePath(root, winslash = "\\")
inp_win <- normalizePath(indir, winslash = "\\")

cfg <- list(
  config_version = "1.0.0",
  engine_min_version = "0.1.0",
  created_at = "2026-09-18T00:00:00Z",
  language = "fr",
  company = list(
    name = "Exemple SAS",
    fiscal_year_end_month = 12,
    currency = "EUR",
    industry_tag = "services",
    entity_scope = "one_company_total"
  ),
  paths = list(
    working_root_hint = root_win,
    pnl_actuals = file.path(inp_win, "pnl.csv"),
    opening_bs = file.path(inp_win, "bs-opening.csv"),
    budget = file.path(inp_win, "budget.csv"),
    assumptions = file.path(inp_win, "parametres.csv"),
    decisions = file.path(inp_win, "investissements-emprunts.csv"),
    ops_other = file.path(inp_win, "volumes.csv")
  ),
  sources = list(
    list(
      source_id = "src-pnl",
      source_name = "Compte de resultat",
      system = "ledger",
      grain = "month",
      history_months = 24,
      availability_lag_days = 20,
      refresh_cadence = "mensuel",
      snapshot_or_history = "history",
      owner = "DAF",
      state = "schema_mapped",
      file_role = "pnl_actuals",
      headers_declared = list("libelle", "periode", "montant"),
      incoming_kind = "ledger",
      file_layout = "long",
      column_map = list(libelle = "libelle", periode = "periode", montant = "montant"),
      column_period = structure(list(), names = character()),
      exercice_n = 2025,
      periods = list()
    ),
    list(
      source_id = "src-bs",
      source_name = "Bilan d'ouverture",
      system = "ledger",
      grain = "month",
      history_months = 1,
      availability_lag_days = 20,
      refresh_cadence = "annuel",
      snapshot_or_history = "snapshot",
      owner = "DAF",
      state = "schema_mapped",
      file_role = "opening_bs",
      headers_declared = list("libelle", "montant"),
      incoming_kind = "ledger",
      file_layout = "long",
      column_map = list(libelle = "libelle", montant = "montant"),
      column_period = structure(list(), names = character()),
      exercice_n = 2023,
      periods = list()
    ),
    list(
      source_id = "src-budget",
      source_name = "Budget",
      system = "ledger",
      grain = "month",
      history_months = 24,
      availability_lag_days = 0,
      refresh_cadence = "annuel",
      snapshot_or_history = "history",
      owner = "DAF",
      state = "schema_mapped",
      file_role = "budget",
      headers_declared = list("libelle", "periode", "montant"),
      incoming_kind = "ledger",
      file_layout = "long",
      column_map = list(libelle = "libelle", periode = "periode", montant = "montant"),
      column_period = structure(list(), names = character()),
      exercice_n = 2025,
      periods = list()
    )
  ),
  line_map = list(
    "Chiffre d'affaires" = "REV",
    "Achats" = "COGS",
    "Salaires" = "PAYROLL",
    "Charges externes" = "OPEX_OTHER",
    "Amortissements" = "D_A",
    "Charges financieres" = "FIN",
    "Impot sur les societes" = "TAX"
  ),
  line_map_bs = list(
    "Tresorerie" = "CASH",
    "Clients" = "AR",
    "Stocks" = "INV",
    "Immobilisations nettes" = "PPE",
    "Autres actifs courants" = "OTHER_CA",
    "Fournisseurs" = "AP",
    "Dettes CT" = "ST_DEBT",
    "Dettes LT" = "LT_DEBT",
    "Autres passifs courants" = "OTHER_CL",
    "Capitaux propres" = "EQUITY"
  ),
  line_bindings = list(),
  mechanisms = list(
    qty_x_price = FALSE,
    customers_x_spend = FALSE,
    capacity_x_util_x_rate = FALSE,
    revenue_growth_pct = FALSE,
    variable_cost_pct_or_unit = FALSE,
    headcount_x_cost = FALSE,
    fixed_monthly_expense = TRUE,
    dso_dpo_dio = TRUE,
    capex_schedule = TRUE,
    debt_schedule = TRUE,
    effective_tax_rate = TRUE
  ),
  assumption_keys = list(
    list(id = "taux.impot", enabled = TRUE, origin = "catalog", engine = "wired", meaning = "IS effectif"),
    list(id = "dso", enabled = TRUE, origin = "catalog", engine = "wired", meaning = "Jours clients"),
    list(id = "dpo", enabled = TRUE, origin = "catalog", engine = "wired", meaning = "Jours fournisseurs"),
    list(id = "dio", enabled = TRUE, origin = "catalog", engine = "wired", meaning = "Jours de stock"),
    list(id = "charge.fixe.loyer", enabled = TRUE, origin = "catalog", engine = "wired", meaning = "Loyer")
  ),
  file_schemas = list(),
  labels = list("imported_actual", "mechanical_rule", "management_assumption")
)

jsonlite::write_json(
  cfg,
  file.path(cfgdir, "fpa-config.json"),
  auto_unbox = TRUE,
  pretty = TRUE
)

message("Fixtures written to ", indir)
