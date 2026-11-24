# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Three businesses the product was not built around, run end to end:
# a SaaS, a restaurant with three sites (one row per site per month), and a
# factory with a yield in percent. Each gets 36 months of P&L history, its own
# activity file, driver_bindings and driver_formulas, like the wizard writes.
# The factory is also run through the assembled fpa-engine.R a subscriber downloads.
root <- "."
setwd(root)
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

ok <- 0L
fail <- 0L
check <- function(cond, label) {
  if (isTRUE(cond)) {
    ok <<- ok + 1L
    cat("PASS", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
}

months <- format(seq(as.Date("2023-09-01"), by = "month", length.out = 36), "%Y-%m")
season <- 1 + 0.25 * sin(2 * pi * (seq_along(months) + 2) / 12)

bind <- function(col, label, kind, target, confidence = "high") {
  list(series_id = paste0("src-ops__", col), source_id = "src-ops", column = col,
       label = label, kind = kind, target_line = target, formula = NULL, unit = "",
       origin = "ai", confidence = confidence, confirm = FALSE, description = "", reason = "")
}
formula <- function(target, factors) {
  list(id = paste0(target, "=", paste(factors, collapse = "*")), label = target,
       target = target, factors = as.list(factors), origin = "rule",
       confidence = "high", confirm = FALSE, reason = "")
}

make_pack <- function(name, ops, pnl_rows, line_map, bindings, formulas, mechanisms = list()) {
  dir <- file.path(tempdir(), paste0("pack-", name))
  unlink(dir, recursive = TRUE)
  dir.create(file.path(dir, "input"), recursive = TRUE)
  dir.create(file.path(dir, "work"), recursive = TRUE)
  ops_path <- file.path(dir, "input", "activite.csv")
  utils::write.csv(ops, ops_path, row.names = FALSE)
  utils::write.csv(pnl_rows, file.path(dir, "input", "pnl-historique.csv"), row.names = FALSE, fileEncoding = "UTF-8")
  cfg <- list(
    config_version = "1.0.0", language = "fr",
    company = list(name = name, fiscal_year_end_month = 12, currency = "EUR"),
    paths = list(working_root_hint = dir, ops_other = ops_path),
    sources = list(list(
      source_id = "src-ops", source_name = "Activite", file_role = "ops_other", grain = "month",
      availability_lag_days = 10, headers_declared = as.list(names(ops)),
      column_map = stats::setNames(as.list(names(ops)), names(ops))
    )),
    line_map = line_map,
    mechanisms = mechanisms,
    driver_bindings = bindings,
    driver_formulas = formulas
  )
  path <- file.path(dir, "fpa-config.json")
  jsonlite::write_json(cfg, path, auto_unbox = TRUE, null = "null", pretty = TRUE)
  path
}

pnl_long <- function(lines) {
  do.call(rbind, lapply(names(lines), function(lab) {
    data.frame(libelle = lab, periode = months, montant = round(lines[[lab]], 2), stringsAsFactors = FALSE)
  }))
}

form_row <- function(pass, target_prefix) {
  f <- pass$formulas
  f[startsWith(f$id, target_prefix), , drop = FALSE]
}

# --- SaaS ---------------------------------------------------------------
set.seed(101)
clients <- round(400 * (1 + 0.02 * seq_along(months)) * (1 + rnorm(36, 0, 0.03)))
arpu <- round(55 * (1 + 0.005 * seq_along(months)) * (1 + rnorm(36, 0, 0.02)), 2)
etp <- round(12 + 0.15 * seq_along(months) + rnorm(36, 0, 0.4), 1)
saas_ops <- data.frame(mois = months, clients_actifs = clients, arpu = arpu,
                       nouveaux_clients = pmax(0, round(20 + rnorm(36, 0, 5))), etp = etp)
saas_pnl <- pnl_long(list(
  "Ventes abonnements" = clients * arpu,
  "Salaires et charges" = etp * 5200 + rnorm(36, 0, 800),
  "Hebergement" = 3000 + clients * 2 + rnorm(36, 0, 150)
))
saas_cfg <- make_pack(
  "saas", saas_ops, saas_pnl,
  list("Ventes abonnements" = "REV", "Salaires et charges" = "PAYROLL", "Hebergement" = "OPEX_OTHER"),
  list(bind("clients_actifs", "clients actifs", "customers", "REV"),
       bind("arpu", "revenu moyen par client", "spend", "REV"),
       bind("nouveaux_clients", "nouveaux clients", "flow", "REV", "medium"),
       bind("etp", "équivalents temps plein", "headcount", "PAYROLL")),
  list(formula("LINE:REV", c("clients_actifs", "arpu")))
)
saas <- fpa_driver_pass(saas_cfg, write = FALSE)
sg <- saas$grades
f1 <- form_row(saas, "LINE:REV")
check(nrow(f1) == 1L && isTRUE(f1$verified) && f1$n == 36L, "saas: revenue = customers x ARPU proven on 36 months")
check(all(sg$relation[sg$series_name %in% c("clients_actifs", "arpu")] == "part_whole"), "saas: formula factors become a mechanism")
e <- sg[sg$series_name == "etp", , drop = FALSE]
check(identical(as.character(e$target_line), "PAYROLL") && e$evidence_r > 0.8, "saas: headcount tested against payroll, r > 0.8")
check(identical(as.character(sg$target_line[sg$series_name == "nouveaux_clients"]), "REV"), "saas: a flow keeps its declared line")

# --- Restaurant, three sites -------------------------------------------
set.seed(202)
sites <- c(Lyon = 1.0, Annecy = 0.7, Grenoble = 1.3)
tickets <- c(Lyon = 28, Annecy = 36, Grenoble = 24)
resto_ops <- do.call(rbind, lapply(names(sites), function(s) {
  data.frame(
    periode = months, site = s,
    couverts = round(3000 * sites[[s]] * season * (1 + rnorm(36, 0, 0.05))),
    ticket_moyen = round(tickets[[s]] * (1 + rnorm(36, 0, 0.03)), 2),
    places = round(80 * sites[[s]]),
    stringsAsFactors = FALSE
  )
}))
resto_ops <- resto_ops[order(resto_ops$periode, resto_ops$site), ]
rev_by_month <- tapply(resto_ops$couverts * resto_ops$ticket_moyen, resto_ops$periode, sum)
cov_by_month <- tapply(resto_ops$couverts, resto_ops$periode, sum)
resto_pnl <- pnl_long(list(
  "Ventes restauration" = as.numeric(rev_by_month[months]),
  "Achats de marchandises" = 0.3 * as.numeric(rev_by_month[months]) + rnorm(36, 0, 500),
  "Salaires" = 42000 + 3 * as.numeric(cov_by_month[months]) + rnorm(36, 0, 900)
))
resto_cfg <- make_pack(
  "resto", resto_ops, resto_pnl,
  list("Ventes restauration" = "REV", "Achats de marchandises" = "COGS", "Salaires" = "PAYROLL"),
  list(bind("couverts", "couverts", "volume", "REV"),
       bind("ticket_moyen", "ticket moyen", "spend", "REV"),
       bind("places", "places", "capacity", "REV")),
  list(formula("LINE:REV", c("couverts", "ticket_moyen")))
)
resto_config <- fpa_load_config(resto_cfg)
prof <- fpa_profile_drivers(resto_config, fpa_paths(resto_config))
cov_series <- prof$series[["src-ops__couverts"]]
check(abs(sum(cov_series$values) - sum(resto_ops$couverts)) < 1e-6 && length(cov_series$values) == 36L,
      "resto: covers summed over the three sites, 36 months")
tk <- prof$series[["src-ops__ticket_moyen"]]
m1 <- resto_ops[resto_ops$periode == months[[1]], ]
check(abs(tk$values[[1]] - sum(m1$couverts * m1$ticket_moyen) / sum(m1$couverts)) < 1e-9,
      "resto: ticket weighted by covers, not a plain mean")
check(abs(tk$values[[1]] - mean(m1$ticket_moyen)) > 0.05, "resto: the plain mean would have been wrong")
resto <- fpa_driver_pass(resto_cfg, write = FALSE)
rf <- form_row(resto, "LINE:REV")
check(nrow(rf) == 1L && isTRUE(rf$verified), "resto: revenue = covers x ticket proven after the site rule")
rg <- resto$grades
check(any(grepl("3 lignes par mois", rg$segment_note)), "resto: the screen note names the site rule")
check(identical(as.character(rg$bottom_n[rg$series_name == "couverts"]), "36") ||
        rg$bottom_n[rg$series_name == "couverts"] == 36L, "resto: the result test also sees 36 collapsed months")

# --- Factory: yield in percent -------------------------------------------
set.seed(303)
tonnes <- round(1200 * season * (1 + rnorm(36, 0, 0.04)))
rendement <- round(92 + rnorm(36, 0, 1.2), 1)
prix <- round(410 * (1 + 0.004 * seq_along(months)) * (1 + rnorm(36, 0, 0.02)), 2)
kwh <- round(tonnes * 310 * (1 + rnorm(36, 0, 0.03)))
prix_kwh <- round(0.14 * (1 + 0.01 * seq_along(months)) * (1 + rnorm(36, 0, 0.03)), 4)
plant_ops <- data.frame(periode = months, tonnes_produites = tonnes, rendement_pct = rendement,
                        prix_tonne = prix, kwh = kwh, prix_kwh = prix_kwh)
plant_pnl <- pnl_long(list(
  "Ventes de produits finis" = tonnes * rendement / 100 * prix,
  "Achats d'energie" = kwh * prix_kwh,
  "Salaires" = 95000 + rnorm(36, 0, 1500)
))
plant_forms <- list(
  formula("LINE:REV", c("tonnes_produites", "rendement_pct", "prix_tonne")),
  formula("LINE:COGS", c("kwh", "prix_kwh")),
  formula("LINE:OPEX_OTHER", c("tonnes_produites", "prix_kwh"))
)
plant_cfg <- make_pack(
  "plant", plant_ops, plant_pnl,
  list("Ventes de produits finis" = "REV", "Achats d'energie" = "COGS", "Salaires" = "PAYROLL"),
  list(bind("tonnes_produites", "tonnes produites", "volume", "REV"),
       bind("rendement_pct", "rendement", "ratio", "REV", "medium"),
       bind("prix_tonne", "prix à la tonne", "rate", "REV"),
       bind("kwh", "kWh consommés", "volume", "COGS"),
       bind("prix_kwh", "prix du kWh", "input_price", "COGS")),
  plant_forms
)
plant <- fpa_driver_pass(plant_cfg, write = FALSE)
pf <- plant$formulas
check(isTRUE(pf$verified[pf$id == "LINE:REV=tonnes_produites*rendement_pct*prix_tonne"]),
      "plant: revenue = tonnes x yield (in %) x price proven, three factors")
check(isTRUE(pf$verified[pf$id == "LINE:COGS=kwh*prix_kwh"]), "plant: energy purchases = kWh x price proven")
check(!isTRUE(pf$verified[pf$id == "LINE:OPEX_OTHER=tonnes_produites*prix_kwh"]), "plant: a formula that does not hold is rejected")
pg <- plant$grades
check(identical(as.character(pg$target_line[pg$series_name == "prix_kwh"]), "COGS"), "plant: input price tested against purchases")

# --- Same factory through the subscriber file ---------------------------
bundle <- file.path(tempdir(), "fpa-engine-bundle.R")
rs <- file.path(R.home("bin"), "Rscript.exe")
out <- system2(rs, c("tools/assemble-engine.R", shQuote(bundle)), stdout = TRUE, stderr = TRUE)
check(file.exists(bundle), "bundle: fpa-engine.R assembled from R/")
env <- new.env()
options(fpa.skip_main = TRUE)
loaded <- tryCatch({
  sys.source(bundle, envir = env)
  TRUE
}, error = function(e) {
  cat("bundle load error:", conditionMessage(e), "\n")
  FALSE
})
check(loaded, "bundle: loads on its own")
if (loaded) {
  b <- env$fpa_driver_pass(plant_cfg, write = FALSE)
  same <- isTRUE(all.equal(b$formulas$median_gap, pf$median_gap)) &&
    identical(b$formulas$verified, pf$verified) &&
    isTRUE(all.equal(b$grades$evidence_r, pg$evidence_r))
  check(same, "bundle: same formulas and same r as the R/ folder")
}

cat("RESULT", ok, "pass", fail, "fail\n")
quit(status = if (fail == 0L) 0L else 1L)
