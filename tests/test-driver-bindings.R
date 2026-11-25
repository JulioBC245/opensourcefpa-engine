# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# driver_bindings from the wizard must replace the hotel-word guesses.
# The hotel files are copied with neutral column names that no fallback rule
# recognises; the engine has to find volume, rate and target lines from the block.
root <- "."
setwd(root)
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

base <- fpa_skip_if_no_optional_data("driver bindings")

tmp <- file.path(tempdir(), "fpa-bindings")
unlink(tmp, recursive = TRUE)
dir.create(file.path(tmp, "input"), recursive = TRUE)
dir.create(file.path(tmp, "work"), recursive = TRUE)
for (f in list.files(file.path(base, "input"), full.names = TRUE)) {
  if (!dir.exists(f)) file.copy(f, file.path(tmp, "input", basename(f)))
}
for (f in c("pnl-monthly.csv", "period-status.csv")) {
  file.copy(file.path(base, "work", f), file.path(tmp, "work", f))
}

rename <- c(
  capacite = "rooms_open", taux_utilisation = "occ_rate", tarif = "adr_eur",
  nuitees_vendues = "rooms_sold", effectif = "fte", cout_moyen = "cost_per_fte"
)
vol_path <- file.path(tmp, "input", "volumes-mensuel-2026.csv")
vol <- utils::read.csv(vol_path, stringsAsFactors = FALSE, check.names = FALSE)
names(vol) <- ifelse(names(vol) %in% names(rename), rename[names(vol)], names(vol))
utils::write.csv(vol, vol_path, row.names = FALSE)

cfg <- jsonlite::fromJSON(file.path(base, "fpa-config.json"), simplifyVector = FALSE)
swap <- function(x) {
  if (is.list(x)) return(lapply(x, swap))
  if (is.character(x)) return(gsub(base, tmp, gsub("\\\\", "/", x), fixed = TRUE))
  x
}
cfg <- swap(cfg)
cfg$paths$working_root_hint <- tmp
for (i in seq_along(cfg$sources)) {
  if (identical(cfg$sources[[i]]$source_id, "src-ops_other")) {
    h <- unlist(cfg$sources[[i]]$headers_declared)
    cfg$sources[[i]]$headers_declared <- as.list(ifelse(h %in% names(rename), rename[h], h))
  }
}
row <- function(col, label, kind, target, formula = NULL) {
  list(
    series_id = paste0("src-ops_other__", col), source_id = "src-ops_other", column = col,
    label = label, kind = kind, target_line = target, formula = formula,
    unit = "", origin = "ai", confirm = FALSE, reason = ""
  )
}
cfg$driver_bindings <- list(
  row("rooms_open", "chambres ouvertes", "capacity", "REV", "capacity_x_util_x_rate"),
  row("occ_rate", "taux d'occupation", "utilisation", "REV", "capacity_x_util_x_rate"),
  row("adr_eur", "prix moyen", "rate", "REV", "capacity_x_util_x_rate"),
  row("rooms_sold", "chambres vendues", "volume", "REV", "capacity_x_util_x_rate"),
  row("fte", "équivalents temps plein", "headcount", "PAYROLL", "headcount_x_cost"),
  row("cost_per_fte", "coût par ETP", "unit_cost", "PAYROLL", "headcount_x_cost")
)
cfg_path <- file.path(tmp, "fpa-config.json")
jsonlite::write_json(cfg, cfg_path, auto_unbox = TRUE, null = "null", pretty = TRUE)

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

config <- fpa_load_config(cfg_path)
check(identical(fpa_driver_bound_columns(config, "volume"), "rooms_sold"), "volume read from the block")
check(is.na(fpa_driver_bottom_volume_name("rooms_sold")), "fallback words do not know rooms_sold")

res <- fpa_run(cfg_path, write = FALSE, stop_on_error = FALSE)
pass <- fpa_driver_pass(cfg_path, res = res, write = FALSE)
g <- pass$grades
by <- function(name) g[g$series_name == name, , drop = FALSE]
check(nrow(g) >= 6L, "neutral columns all graded")
check(identical(as.character(by("cost_per_fte")$target_line), "PAYROLL"), "unit cost tested against payroll")
check(identical(as.character(by("fte")$target_line), "PAYROLL"), "headcount tested against payroll")
check(identical(as.character(by("adr_eur")$bottom_control), "rooms_sold"), "rate controlled for the declared volume")
check(identical(as.character(by("rooms_sold")$bottom_control), "adr_eur"), "volume controlled for the declared rate")
check(identical(as.character(by("occ_rate")$tier), "mechanism"), "capacity x utilisation still proven on the numbers")
hotel <- utils::read.csv(file.path(base, "work/drivers/driver-grades.csv"), stringsAsFactors = FALSE)
check(abs(by("adr_eur")$evidence_r - hotel$evidence_r[hotel$series_name == "tarif"]) < 1e-9, "same r as the hotel names")
check(is.na(by("adr_eur")$impact_eur) || identical(as.character(by("adr_eur")$impact_status), "not_available"), "a wizard row does not invent euros")

cat("RESULT", ok, "pass", fail, "fail\n")
quit(status = if (fail == 0L) 0L else 1L)
