# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Forecast cash and balance sheet: the cell trail must tie to the stored month.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

hotel <- fpa_optional_data_config()
if (is.null(hotel)) fpa_skip_if_no_optional_data("statement cell checks")
cfg <- fpa_load_config(hotel)
paths <- fpa_paths(cfg)
cash <- utils::read.csv(file.path(paths$work, "cash-bridge-monthly.csv"), stringsAsFactors = FALSE)
bs <- utils::read.csv(file.path(paths$work, "bs-monthly.csv"), stringsAsFactors = FALSE)
pnl <- utils::read.csv(file.path(paths$work, "pnl-monthly.csv"), stringsAsFactors = FALSE)

stopifnot(isFALSE(fpa_cash_cell_read(cfg, "2026-09", paths)$ok))
stopifnot(isFALSE(fpa_bs_cell_read(cfg, "2026-09", paths)$ok))

notes_of <- function(read) {
  paste(vapply(read$rows, function(r) as.character(r$note %||% ""), character(1)), collapse = "\n")
}
row_of <- function(read, id) {
  hit <- Filter(function(r) identical(r$id, id), read$rows)
  stopifnot(length(hit) == 1L)
  hit[[1]]
}

dec <- fpa_cash_cell_read(cfg, "2026-12", paths, "cash_close")
stopifnot(isTRUE(dec$ok))
stored <- cash[cash$periode == "2026-12", , drop = FALSE]
stopifnot(fpa_cell_tie(dec$close, stored$cash_close))
rel <- if ("wc_opening_release" %in% names(stored)) stored$wc_opening_release else 0
stopifnot(fpa_cell_tie(dec$delta, stored$NI + stored$D_A + stored$wc_impact + rel - stored$capex + stored$debt_net))
stopifnot(fpa_cell_tie(dec$close, stored$cash_open + dec$delta))
stopifnot(identical(dec$focus, "cash_close"))
stopifnot(identical(dec$screen, format(round(dec$close), big.mark = " ", scientific = FALSE, trim = TRUE)))
bfr <- row_of(dec, "wc_impact")
stopifnot(length(bfr$factors) == 4L)
stopifnot(grepl("parametres.csv", bfr$note))
stopifnot(isFALSE(dec$capex_gap))
stopifnot(isFALSE(dec$debt_skipped))
debt <- row_of(dec, "debt_net")
stopifnot(fpa_cell_tie(debt$amount, -4200))
stopifnot(grepl("calendrier-capex-dette-2026.csv", debt$note))
nov <- fpa_cash_cell_read(cfg, "2026-11", paths, "capex")
stopifnot(isTRUE(nov$ok))
stopifnot(isFALSE(nov$capex_gap))
stopifnot(identical(nov$focus, "capex"))
stopifnot(fpa_cell_tie(row_of(nov, "capex")$amount, 18500))
stopifnot(grepl("calendrier-capex-dette-2026.csv", notes_of(nov)))
stopifnot(fpa_cell_tie(nov$close, cash$cash_close[cash$periode == "2026-11"]))

sheet <- fpa_bs_cell_read(cfg, "2026-12", paths, "AR")
stopifnot(isTRUE(sheet$ok))
stopifnot(abs(sheet$gap) < 0.05)
stopifnot(identical(sheet$focus, "AR"))
bs_row <- bs[bs$periode == "2026-12", , drop = FALSE]
pnl_row <- pnl[pnl$periode == "2026-12", , drop = FALSE]
prev <- bs[bs$periode == "2026-11", , drop = FALSE]
stopifnot(fpa_cell_tie(sheet$assets, bs_row$CASH + bs_row$AR + bs_row$INV + bs_row$PPE + bs_row$OTHER_CA))
stopifnot(fpa_cell_tie(sheet$claims, sheet$assets))
md <- fpa_wc_month_days("2026-12")
stopifnot(fpa_cell_tie(bs_row$AR, pnl_row$REV * 12 / md))
stopifnot(fpa_cell_tie(bs_row$INV, pnl_row$COGS * 8 / md))
stopifnot(fpa_cell_tie(bs_row$AP, (pnl_row$COGS + pnl_row$OPEX_OTHER) * 35 / md))
stopifnot(fpa_cell_tie(bs_row$OTHER_CL, pnl_row$PAYROLL * 15 / md))
stopifnot(fpa_cell_tie(bs_row$PPE, prev$PPE + stored$capex - stored$D_A))
stopifnot(fpa_cell_tie(bs_row$EQUITY, prev$EQUITY + stored$NI))
stopifnot(fpa_cell_tie(bs_row$CASH, stored$cash_close))
ar <- row_of(sheet, "AR")
stopifnot(length(ar$factors) == 3L)
stopifnot(grepl("parametres.csv", ar$note))
stopifnot(grepl("bs-opening.xlsx", notes_of(sheet)))
stopifnot(grepl("calendrier-capex-dette-2026.csv", notes_of(sheet)))
stopifnot(identical(sheet$screen, format(round(bs_row$AR), big.mark = " ", scientific = FALSE, trim = TRUE)))

message("PASS cell statements 38")
