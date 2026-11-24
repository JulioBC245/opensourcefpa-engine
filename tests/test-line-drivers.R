# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Line drivers: coefficients measured on the history, then summed back into each bucket.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

checks <- 0L
check <- function(ok, what) {
  if (!isTRUE(ok)) stop("FAIL ", what, call. = FALSE)
  checks <<- checks + 1L
}

pers <- c(sprintf("2024-%02d", 1:12), sprintf("2025-%02d", 1:12))
nights <- rep(c(30, 40, 60, 80, 120, 140, 180, 190, 130, 80, 40, 35), 2)
rev <- nights * 100
volume <- setNames(c(nights, 82), c(pers, "2026-10"))
hist <- rbind(
  data.frame(libelle = "Achats de matières", periode = pers, montant = 4 * nights),
  data.frame(libelle = "Autres achats et charges externes", periode = pers, montant = 1000 + 12 * nights),
  data.frame(libelle = "Autres charges d'exploitation", periode = pers, montant = 200),
  data.frame(libelle = "Commissions OTA", periode = pers, montant = 0.15 * rev),
  data.frame(libelle = "Loyer", periode = pers, montant = c(rep(900, 21), 950, 950, 950)),
  stringsAsFactors = FALSE
)
rows <- data.frame(
  user_label = c("Achats de matières", "Autres achats et charges externes",
                 "Autres charges d'exploitation", "Commissions OTA", "Loyer"),
  functional_label = c("COGS", "OPEX_OTHER", "OPEX_OTHER", "OPEX_OTHER", "OPEX_OTHER"),
  driver = c("volume", "mixed", "unknown", "revenue_pct", "contract"),
  origin = "ai",
  stringsAsFactors = FALSE
)
fit <- fpa_line_driver_fit(rows, hist, volume, setNames(rev, pers), before = "2026-10")
row <- function(lab) fit[fit$user_label == lab, , drop = FALSE]

check(abs(row("Achats de matières")$per_volume - 4) < 1e-9, "volume ratio 4 per night")
check(abs(row("Autres achats et charges externes")$fixed - 1000) < 1e-6, "mixed fixed part 1000")
check(abs(row("Autres achats et charges externes")$per_volume - 12) < 1e-9, "mixed per night 12")
check(row("Autres achats et charges externes")$r2 > 0.999, "mixed fit R2")
check(row("Autres achats et charges externes")$method == "fit_ols", "mixed method")
check(row("Autres charges d'exploitation")$method == "seasonal", "open line stays seasonal")
check(abs(row("Commissions OTA")$pct_rev - 0.15) < 1e-9, "revenue share 15 %")
check(abs(row("Loyer")$fixed - 950) < 1e-9, "contract = median of last 3 months")

flat <- hist[hist$libelle == "Autres achats et charges externes", ]
flat$montant <- 1500 - 0.5 * nights
hist_flat <- rbind(hist[hist$libelle != "Autres achats et charges externes", ], flat)
fit_flat <- fpa_line_driver_fit(rows, hist_flat, volume, setNames(rev, pers), before = "2026-10")
check(fit_flat$method[fit_flat$user_label == "Autres achats et charges externes"] == "fit_flat",
      "a mixed line that falls with volume is held flat")

share <- fpa_line_rest_share(hist, fit, "OPEX_OTHER", 10)
oct_opex <- 2 * (1000 + 12 * 80 + 200 + 0.15 * 8000) + 900 + 950
check(abs(share - 400 / oct_opex) < 1e-9,
      sprintf("seasonal share = open lines / bucket, same month (got %.6f, want %.6f)", share, 400 / oct_opex))

pnl <- data.frame(
  periode = "2026-10", REV = 8200, OTHER_OP_INC = 0, COGS = 213, PAYROLL = 5000,
  OPEX_OTHER = 4000, D_A = 400, FIN = 100, EXCEP = 0, TAX = 0, stringsAsFactors = FALSE
)
pnl <- fpa_pnl_derive(pnl)
out <- fpa_pnl_apply_line_drivers(pnl, fit, hist, volume, "2026-10")
check(abs(out$COGS - 4 * 82) < 1e-9, "COGS = 4 x 82 nights, the 2.6 % is gone")
want_opex <- 4000 * share + (1000 + 12 * 82) + 0.15 * 8200 + 950
check(abs(out$OPEX_OTHER - want_opex) < 1e-6, "OPEX = seasonal share + mixed + revenue share + contract")
check(abs(out$OI - (8200 - out$COGS - 5000 - out$OPEX_OTHER - 400)) < 1e-6, "OI re-derived")

same <- fpa_pnl_apply_line_drivers(pnl, fit[0, ], hist, volume, "2026-10")
check(identical(same$OPEX_OTHER, pnl$OPEX_OTHER), "no driver rows leaves the statement alone")

cfg <- list(line_drivers = list(lines = list(
  list(user_label = "Loyer", functional_label = "OPEX_OTHER", driver = "contract", origin = "user")
)))
check(nrow(fpa_line_driver_rows(cfg)) == 1L, "config rows read from a list")
check(nrow(fpa_line_driver_rows(list())) == 0L, "no line_drivers block")

vol_df <- data.frame(periode = c("2026-10", "2026-11"), capacite = c(217, 210),
                     taux_utilisation = c(0.37788, 0.219048), stringsAsFactors = FALSE)
v <- fpa_volume_by_month(vol_df)
check(abs(v[["2026-10"]] - 82) < 0.01, "nights = rooms x occupancy when nights are absent")

message(sprintf("PASS line-drivers (%d checks)", checks))
