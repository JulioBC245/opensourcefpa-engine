# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Landing cards: fiscal coverage, unknown status, pace off the default surface.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
say <- function(ok, msg) {
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  cat(if (isTRUE(ok)) "PASS " else "FAIL ", msg, "\n", sep = "")
}

pnl <- data.frame(
  periode = paste0("2026-", sprintf("%02d", 1:12)),
  REV = c(rep(10000, 9), 4000, 4000, 3645),
  OI = c(rep(3000, 9), -1000, -1000, -500),
  stringsAsFactors = FALSE
)
status <- data.frame(
  periode = pnl$periode,
  status = c(rep("actual", 9), rep("forecast", 3)),
  stringsAsFactors = FALSE
)
frame <- fpa_synthese_frame(pnl, status, NULL)
say(all(frame$etat[1:9] == "réalisé") && all(frame$etat[10:12] == "prévision"), "status maps to réalisé / prévision")

# Unknown months stay unknown and out of booked + forecast totals.
status_u <- status
status_u$status[10] <- "mystery"
frame_u <- fpa_synthese_frame(pnl, status_u, NULL)
say(identical(frame_u$etat[[10]], "inconnu"), "unknown status stays unknown")
L_u <- fpa_synthese_land(frame_u, NULL, fy_months = pnl$periode)
say(L_u$n_unknown == 1L && abs(L_u$rev_f - (4000 + 3645)) < 1, "unknown month is excluded from forecast sum")
say(!isTRUE(L_u$pace_off), "seasonality-blind pace never flags on the landing object")

bud_partial <- data.frame(
  periode = paste0("2026-", sprintf("%02d", 1:3)),
  functional_label = "REV",
  montant = 1000,
  stringsAsFactors = FALSE
)
cov_p <- fpa_synthese_budget_coverage(bud_partial, pnl$periode, "REV")
say(isTRUE(cov_p$partial) && !isTRUE(cov_p$covers), "three budget months are partial, not full-year")

bud_full <- data.frame(
  periode = pnl$periode,
  functional_label = "REV",
  montant = 1000,
  stringsAsFactors = FALSE
)
cov_f <- fpa_synthese_budget_coverage(bud_full, pnl$periode, "REV")
say(isTRUE(cov_f$covers) && abs(cov_f$total - 12000) < 0.1, "twelve fiscal months cover and sum to 12 000")

# Non-calendar fiscal year (Apr–Mar).
fy <- c(paste0("2025-", sprintf("%02d", 4:12)), paste0("2026-", sprintf("%02d", 1:3)))
pnl_fy <- data.frame(
  periode = fy,
  REV = rep(1000, 12),
  OI = rep(100, 12),
  stringsAsFactors = FALSE
)
st_fy <- data.frame(periode = fy, status = rep("actual", 12), stringsAsFactors = FALSE)
bud_fy <- data.frame(
  periode = fy, functional_label = "REV", montant = 500, stringsAsFactors = FALSE
)
L_fy <- fpa_synthese_land(fpa_synthese_frame(pnl_fy, st_fy, NULL), bud_fy, fy_months = fy)
say(isTRUE(L_fy$bud_covers) && abs(L_fy$bud_rev - 6000) < 0.1, "Apr–Mar budget covers all twelve fiscal months")
say(abs(L_fy$rev_y - 12000) < 0.1, "Apr–Mar revenue lands at 12 000")

# Empty / all-zero / single-row extremes.
say(!isTRUE(fpa_synthese_land(data.frame(), NULL)$ok), "empty frame is not ok")
z <- data.frame(periode = "2026-01", REV = 0, OI = 0, etat = "réalisé", stringsAsFactors = FALSE)
Lz <- fpa_synthese_land(z, NULL, fy_months = "2026-01")
say(isTRUE(Lz$ok) && Lz$rev_y == 0 && Lz$oi_y == 0, "single zero row lands at zero")

cat("PASS ", pass, " FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
