# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Manual ledger: moves cash / P&L / BFR without breaking identity or the budget.
# Runs on a temp copy of Hotel Alpes. Never touches the live hotel files.
# From the opensourcefpa-engine root: Rscript tests/check-manual-entries.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    pass <<- pass + 1L
    cat("PASS", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
}

hotel <- fpa_skip_if_no_optional_data("manual ledger checks", dataset = "hotel-alpes")
box <- file.path(tempdir(), paste0("fpa-manual-", as.integer(runif(1, 1, 1e9))))
dir.create(box, recursive = TRUE, showWarnings = FALSE)
# Copy only what the engine needs. Skip the assembled engine binary-size file if huge.
for (nm in c("fpa-config.json", "fpa-config.local.json", "input", "work")) {
  src <- file.path(hotel, nm)
  if (!file.exists(src) && !dir.exists(src)) next
  if (dir.exists(src)) {
    dest <- file.path(box, nm)
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    file.copy(list.files(src, full.names = TRUE), dest, recursive = TRUE, overwrite = TRUE)
  } else {
    file.copy(src, file.path(box, nm), overwrite = TRUE)
  }
}

cfg_path <- file.path(box, "fpa-config.json")
cfg <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
# Point every path that lived under the live hotel at the temp copy.
rewrite <- function(p) {
  if (is.null(p) || !nzchar(as.character(p))) return(p)
  p <- as.character(p)
  p <- gsub("\\\\", "/", p, fixed = FALSE)
  live <- gsub("\\\\", "/", hotel, fixed = FALSE)
  gsub(live, gsub("\\\\", "/", box, fixed = FALSE), p, fixed = TRUE)
}
for (nm in names(cfg$paths)) cfg$paths[[nm]] <- rewrite(cfg$paths[[nm]])
cfg$paths$working_root_hint <- gsub("\\\\", "/", box, fixed = FALSE)
# Keep capex/debt off (plan default). Strip decisions path if present.
cfg$mechanisms$capex_schedule <- FALSE
cfg$mechanisms$debt_schedule <- FALSE
jsonlite::write_json(cfg, cfg_path, auto_unbox = TRUE, pretty = TRUE, null = "null")

# Empty ledger (or missing): baseline.
ledger_path <- file.path(box, "input", "saisies-manuelles.csv")
if (file.exists(ledger_path)) file.remove(ledger_path)

base <- fpa_run(cfg_path, write = TRUE, stop_on_error = FALSE)
check("baseline runs", !is.null(base$cash) && nrow(base$cash) > 0)
check("baseline identity holds", isTRUE(base$identity$ok))
base_close <- as.numeric(base$cash$cash_close[base$cash$periode == "2026-12"])
base_ni <- sum(as.numeric(base$pnl$NI), na.rm = TRUE)
base_oct <- as.numeric(base$cash$cash_close[base$cash$periode == "2026-10"])
base_nov <- as.numeric(base$cash$cash_close[base$cash$periode == "2026-11"])
base_ar_oct <- as.numeric(base$wc$AR[["2026-10"]])
budget_path <- cfg$paths$budget
budget_hash <- if (file.exists(budget_path)) {
  digest <- tryCatch(tools::md5sum(budget_path)[[1]], error = function(e) NA_character_)
  digest
} else {
  NA_character_
}

# Empty ledger file written explicitly: same close as missing file.
utils::write.csv(fpa_manual_empty(), ledger_path, row.names = FALSE, fileEncoding = "UTF-8")
empty_run <- fpa_run(cfg_path, write = FALSE, stop_on_error = FALSE)
empty_close <- as.numeric(empty_run$cash$cash_close[empty_run$cash$periode == "2026-12"])
check("empty ledger matches baseline Dec cash", abs(empty_close - base_close) < 0.5)

# Closed-month nouveau is excluded.
closed_entry <- list(
  id = "m_closed", saisi_le = "2026-10-13", nature = "nouveau", sens = "in",
  montant = 1000, date_effet = "2026-03-15", date_prevue = "",
  ligne = "OTHER_OP_INC", quantite = NA, unite = "", libelle = "trop tot",
  statut = "actif", journal_id = ""
)
invisible(fpa_manual_write(empty_run$config, fpa_manual_as_df(list(closed_entry))))
closed_run <- fpa_run(cfg_path, write = FALSE, stop_on_error = FALSE)
led <- closed_run$manual$ledger
check(
  "closed-month nouveau excluded",
  any(led$id == "m_closed" & led$state == "excluded" & grepl("mois clos", led$reason))
)
closed_close <- as.numeric(closed_run$cash$cash_close[closed_run$cash$periode == "2026-12"])
check("closed-month entry does not move Dec cash", abs(closed_close - base_close) < 0.5)

# Timing: 3500 in from Oct (prevue) to Nov (effet). NI unchanged, Dec cash unchanged.
timing <- list(
  id = "m_timing", saisi_le = "2026-10-13", nature = "decalage", sens = "in",
  montant = 3500, date_effet = "2026-11-15", date_prevue = "2026-10-20",
  ligne = "", quantite = NA, unite = "", libelle = "client late",
  statut = "actif", journal_id = ""
)
invisible(fpa_manual_write(empty_run$config, fpa_manual_as_df(list(timing))))
t_run <- fpa_run(cfg_path, write = FALSE, stop_on_error = FALSE)
t_ni <- sum(as.numeric(t_run$pnl$NI), na.rm = TRUE)
t_oct <- as.numeric(t_run$cash$cash_close[t_run$cash$periode == "2026-10"])
t_nov <- as.numeric(t_run$cash$cash_close[t_run$cash$periode == "2026-11"])
t_dec <- as.numeric(t_run$cash$cash_close[t_run$cash$periode == "2026-12"])
t_ar_oct <- as.numeric(t_run$wc$AR[["2026-10"]])
check("timing identity holds", isTRUE(t_run$identity$ok))
check("timing NI unchanged", abs(t_ni - base_ni) < 0.5)
check("timing Oct cash -3500", abs((t_oct - base_oct) - (-3500)) < 1)
check("timing Nov cash back to baseline", abs(t_nov - base_nov) < 1)
check("timing Dec cash unchanged", abs(t_dec - base_close) < 1)
check("timing Oct AR +3500", abs((t_ar_oct - base_ar_oct) - 3500) < 1)

# Nouveau income 3500 in October.
nouveau <- list(
  id = "m_new", saisi_le = "2026-10-13", nature = "nouveau", sens = "in",
  montant = 3500, date_effet = "2026-10-20", date_prevue = "",
  ligne = "OTHER_OP_INC", quantite = NA, unite = "", libelle = "creance recue",
  statut = "actif", journal_id = ""
)
invisible(fpa_manual_write(empty_run$config, fpa_manual_as_df(list(nouveau))))
n_run <- fpa_run(cfg_path, write = FALSE, stop_on_error = FALSE)
n_ni <- sum(as.numeric(n_run$pnl$NI), na.rm = TRUE)
n_dec <- as.numeric(n_run$cash$cash_close[n_run$cash$periode == "2026-12"])
# Marginal tax on the taxable-base change (loss months: only the profit slice is taxed).
rate <- tryCatch({
  a <- fpa_read_assumptions(n_run$config)
  fpa_assumption_num(a$wide, "2026-10", "taux.impot", default = 0.15)
}, error = function(e) 0.15)
i_oct <- match("2026-10", as.character(base$pnl$periode))
oi0 <- as.numeric(base$pnl$OI[[i_oct]])
fin0 <- as.numeric(base$pnl$FIN[[i_oct]])
ex0 <- as.numeric(base$pnl$EXCEP[[i_oct]])
base0 <- max(oi0 - fin0 - ex0, 0)
base1 <- max(oi0 + 3500 - fin0 - ex0, 0)
expect_ni <- 3500 - rate * (base1 - base0)
check("nouveau identity holds", isTRUE(n_run$identity$ok))
check(
  paste0("nouveau NI +", round(expect_ni), " (marginal tax)"),
  abs((n_ni - base_ni) - expect_ni) < 2
)
check(
  "nouveau Dec cash moves with NI",
  abs((n_dec - base_close) - (n_ni - base_ni)) < 2
)
labs <- n_run$pnl_long
hit <- labs[
  as.character(labs$periode) == "2026-10" & as.character(labs$line_id) == "OTHER_OP_INC",
  ,
  drop = FALSE
]
check(
  "nouveau label saisie_manuelle",
  nrow(hit) > 0 && identical(as.character(hit$evidence_label[[1]]), FPA_LABEL_MANUAL)
)

# Retire restores baseline.
retired <- nouveau
retired$statut <- "retire"
invisible(fpa_manual_write(empty_run$config, fpa_manual_as_df(list(retired))))
r_run <- fpa_run(cfg_path, write = FALSE, stop_on_error = FALSE)
r_dec <- as.numeric(r_run$cash$cash_close[r_run$cash$periode == "2026-12"])
r_ni <- sum(as.numeric(r_run$pnl$NI), na.rm = TRUE)
check("retire restores Dec cash", abs(r_dec - base_close) < 0.5)
check("retire restores NI", abs(r_ni - base_ni) < 0.5)

# Switch off: the row stays in the CSV, the calculation returns to baseline.
invisible(fpa_manual_write(empty_run$config, fpa_manual_as_df(list(nouveau))))
check("missing apply flag means on", isTRUE(fpa_manual_applied(empty_run$config)))
fpa_manual_set_applied(empty_run$config, FALSE)
off_run <- fpa_run(cfg_path, write = FALSE, stop_on_error = FALSE)
off_dec <- as.numeric(off_run$cash$cash_close[off_run$cash$periode == "2026-12"])
off_ni <- sum(as.numeric(off_run$pnl$NI), na.rm = TRUE)
still <- fpa_manual_active(fpa_manual_read(empty_run$config)$data)
off_labs <- off_run$pnl_long
off_hit <- off_labs[
  as.character(off_labs$periode) == "2026-10" & as.character(off_labs$line_id) == "OTHER_OP_INC",
  ,
  drop = FALSE
]
check("switch off restores Dec cash", abs(off_dec - base_close) < 0.5)
check("switch off restores NI", abs(off_ni - base_ni) < 0.5)
check("switch off keeps the CSV row", nrow(still) == 1L && identical(as.character(still$statut[[1]]), "actif"))
check("switch off identity holds", isTRUE(off_run$identity$ok))
check(
  "switch off drops the manual label",
  !nrow(off_hit) || !identical(as.character(off_hit$evidence_label[[1]]), FPA_LABEL_MANUAL)
)
fpa_manual_set_applied(empty_run$config, TRUE)
on_run <- fpa_run(cfg_path, write = FALSE, stop_on_error = FALSE)
on_dec <- as.numeric(on_run$cash$cash_close[on_run$cash$periode == "2026-12"])
check("switch on puts the line back", abs(on_dec - n_dec) < 0.5)

# Budget file hash unchanged after all writes (we never touch it).
budget_hash2 <- if (file.exists(budget_path)) {
  tryCatch(tools::md5sum(budget_path)[[1]], error = function(e) NA_character_)
} else {
  NA_character_
}
check("budget file hash unchanged", identical(budget_hash, budget_hash2))

# Scenario base equals engine close (fpa_scenario_run → fpa_run, same ledger).
invisible(fpa_manual_write(empty_run$config, fpa_manual_as_df(list(nouveau))))
eng <- fpa_run(cfg_path, write = FALSE, stop_on_error = FALSE)
ids <- fpa_enabled_assumption_ids(eng$config)
a <- fpa_read_assumptions(eng$config)
base_vals <- fpa_scenario_base_values(a$wide, ids)
scn <- fpa_scenario_run(cfg_path, base_vals, list(), list())
eng_close <- as.numeric(eng$cash$cash_close[eng$cash$periode == "2026-12"])
scn_close <- as.numeric(scn$cash$cash_close[scn$cash$periode == "2026-12"])
check("scenario base equals engine Dec cash", abs(eng_close - scn_close) < 0.5)

# Ledger path is by convention, not a config key.
check(
  "ledger path under input/",
  identical(normalizePath(fpa_manual_path(eng$config), winslash = "/", mustWork = FALSE),
            normalizePath(ledger_path, winslash = "/", mustWork = FALSE))
)

check(
  "floor is 2% of the year cash, rounded, at least 500",
  identical(fpa_manual_floor(data.frame(cash_open = 10000, cash_close = 53739.21)), 900L)
)
check(
  "floor stays at 500 when the year is small",
  identical(fpa_manual_floor(data.frame(cash_open = 1000, cash_close = 3000)), 500L)
)
check("floor is 500 when cash is missing", identical(fpa_manual_floor(NULL), 500L))

cat("\nPASS", pass, "FAIL", fail, "\n")
if (fail > 0L) quit(status = 1)
