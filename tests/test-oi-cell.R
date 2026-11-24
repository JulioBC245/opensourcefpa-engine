# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Forecast operating income: the cell trail must tie to the stored month.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

hotel <- "../fpa-local-test/fpa-config.json"
stopifnot(file.exists(hotel))
cfg <- fpa_load_config(hotel)
paths <- fpa_paths(cfg)
pnl <- utils::read.csv(file.path(paths$work, "pnl-monthly.csv"), stringsAsFactors = FALSE)

closed <- fpa_oi_cell_read(cfg, "2026-09", paths)
stopifnot(isFALSE(closed$ok))

check <- function(periode) {
  read <- fpa_oi_cell_read(cfg, periode, paths)
  stopifnot(isTRUE(read$ok))
  stored <- pnl[pnl$periode == periode, , drop = FALSE]
  stopifnot(abs(read$oi - as.numeric(stored$OI)) < 0.05)
  stopifnot(abs(read$gp - (read$rev + read$other - read$cogs)) < 0.05)
  stopifnot(abs(read$ebitda - (read$gp - read$pay - read$opex)) < 0.05)
  stopifnot(abs(read$oi - (read$ebitda - read$da)) < 0.05)
  stopifnot(abs(read$ni - (read$oi - read$fin - read$excep - read$tax)) < 0.05)
  stopifnot(nzchar(read$rev_eq))
  stopifnot(nzchar(read$pay_eq))
  stopifnot(nzchar(read$cogs_eq))
  stopifnot(!is.null(read$opex_driven))
  stopifnot(abs(read$opex_driven$amount + read$opex_rest - read$opex) < 0.05)
  stopifnot(identical(fpa_cell_screen(read$oi), format(round(read$oi), big.mark = " ", scientific = FALSE, trim = TRUE)))
  read
}

oct <- check("2026-10")
nov <- check("2026-11")
dec <- check("2026-12")
stopifnot(abs(dec$oi - (-3153.693)) < 0.01)
stopifnot(identical(dec$screen, "-3 154"))
stopifnot(grepl("217", dec$rev_eq))
stopifnot(abs(dec$nights - 39) < 0.05)
stopifnot(dec$oi < oct$oi)

stopifnot(identical(dec$rev_factors[[1]]$label, "Capacité"))
stopifnot(grepl("217", dec$rev_factors[[1]]$value))
stopifnot(identical(dec$rev_factors[[2]]$label, "taux d'utilisation"))
stopifnot(identical(dec$rev_factors[[3]]$label, "tarif"))
stopifnot(identical(dec$cogs_factors[[2]]$value, "3,20"))
stopifnot(identical(dec$opex_driven$factors[[2]]$value, "12,10"))
stopifnot(identical(dec$pay_factors[[2]]$value, "1 886,80"))
stopifnot(identical(fpa_cell_amt(10.5), "10,50"))
stopifnot(identical(fpa_cell_amt(11.56545), "11,57"))
stopifnot(identical(fpa_cell_amt(23), "23"))
stopifnot(identical(fpa_cell_amt(55000), "55 000"))
stopifnot(identical(fpa_cell_amt(3510.01), "3 510,01"))
stopifnot(identical(fpa_cell_amt(-3153.693, digits = 2), "\u22123 153,69"))

vol_sheet <- fpa_file_edit_sheet(cfg, "volumes-mensuel-2026.csv", paths)
stopifnot(isTRUE(vol_sheet$ok))
stopifnot(identical(sort(unique(vol_sheet$ym[vol_sheet$open_row])), c("2026-10", "2026-11", "2026-12")))
stopifnot(!any(vol_sheet$ym[vol_sheet$open_row] == "2026-09"))
hist_sheet <- fpa_file_edit_sheet(cfg, "pnl-mensuel-2026.csv", paths)
stopifnot(isTRUE(hist_sheet$ok))
stopifnot(!length(hist_sheet$edits))

edit_dir <- file.path(tempdir(), "fpa-file-edit")
dir.create(file.path(edit_dir, "input"), recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(edit_dir, "work"), showWarnings = FALSE)
utils::write.csv(
  data.frame(
    periode = c("2026-09", "2026-10"),
    capacite = c(210, 217),
    tarif = c(120, 98),
    stringsAsFactors = FALSE
  ),
  file.path(edit_dir, "input", "volumes-mini.csv"),
  row.names = FALSE
)
utils::write.csv(
  data.frame(periode = c("2026-09", "2026-10"), status = c("actual", "forecast"), stringsAsFactors = FALSE),
  file.path(edit_dir, "work", "period-status.csv"),
  row.names = FALSE
)
mini <- fpa_file_edit_sheet(list(), "volumes-mini.csv", list(
  input = file.path(edit_dir, "input"),
  work = file.path(edit_dir, "work")
))
stopifnot(length(mini$edits) == 2L)
stopifnot(all(vapply(mini$edits, function(e) mini$ym[[e$row]] == "2026-10", logical(1))))
vals <- list()
vals[[mini$edits[[1]]$id]] <- "200"
vals[[mini$edits[[2]]$id]] <- "99,5"
# A forged id for the closed month must not be on the sheet, so it cannot be written.
stopifnot(!any(vapply(mini$edits, function(e) mini$ym[[e$row]] == "2026-09", logical(1))))
applied <- fpa_file_edit_apply(mini, vals)
stopifnot(identical(applied$n, 2L))
back <- utils::read.csv(mini$path, stringsAsFactors = FALSE)
stopifnot(identical(as.numeric(back$capacite[[1]]), 210))
stopifnot(identical(as.numeric(back$capacite[[2]]), 200))
stopifnot(abs(as.numeric(back$tarif[[2]]) - 99.5) < 1e-8)

message("PASS oi cell 3")
