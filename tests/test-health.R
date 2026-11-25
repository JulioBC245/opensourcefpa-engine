# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Ratios the hotel books can support. No cost per tonne, no buyer multiple.
root <- "."
setwd(root)
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

bs <- data.frame(
  periode = "2026-09", CASH = 100, AR = 20, INV = 10, AP = 30, PPE = 200,
  ST_DEBT = 0, LT_DEBT = 50, OTHER_CA = 0, OTHER_CL = 0, EQUITY = 250,
  stringsAsFactors = FALSE
)
pnl <- data.frame(
  periode = "2026-09", REV = 80, COGS = 20, GP = 60, EBITDA = 30, OI = 25, FIN = 5,
  stringsAsFactors = FALSE
)
got <- fpa_health_month(bs, pnl, "2026-09")
val <- function(id) got$value[got$id == id]
stopifnot(abs(val("current_ratio") - 130 / 30) < 1e-9)
stopifnot(abs(val("quick_ratio") - 120 / 30) < 1e-9)
stopifnot(abs(val("cash_ratio") - 100 / 30) < 1e-9)
stopifnot(abs(val("debt_to_equity") - 50 / 250) < 1e-9)
stopifnot(abs(val("debt_to_assets") - 50 / 330) < 1e-9)
stopifnot(abs(val("interest_coverage") - 5) < 1e-9)
stopifnot(abs(val("dso_days") - 20 / 80 * 30) < 1e-9)
stopifnot(abs(val("dio_days") - 10 / 20 * 30) < 1e-9)
stopifnot(abs(val("asset_turnover") - 80 / 330) < 1e-9)
stopifnot(abs(val("gross_margin") - 60 / 80) < 1e-9)
stopifnot(abs(val("oi_margin") - 25 / 80) < 1e-9)
stopifnot(abs(val("book_ev") - (50 + 250 - 100)) < 1e-9)
stopifnot(abs(val("roce") - 25 / 200) < 1e-9)
stopifnot(!any(c("energy_eur_per_t", "it_eur_per_t", "screen_ev_mid") %in% got$id))
stopifnot(identical(got$id, fpa_health_ids()))
for (id in fpa_health_ids()) {
  ex <- fpa_health_explain(id)
  keys <- fpa_health_copy_keys(id)
  stopifnot(!is.null(ex))
  stopifnot(length(ex$parts) >= 2L)
  stopifnot(ex$files %in% c("bs", "pnl", "both"))
  stopifnot(length(keys) == 2L)
  stopifnot(all(nzchar(keys)))
}

# Zero current liabilities: the ratio is absent, not an invented large number.
bs$AP <- 0
thin <- fpa_health_month(bs, pnl, "2026-09")
stopifnot(is.na(thin$value[thin$id == "current_ratio"]))

status <- data.frame(periode = c("2026-08", "2026-09", "2026-10"), status = c("actual", "actual", "forecast"), stringsAsFactors = FALSE)
stopifnot(identical(fpa_health_closed_month(status), "2026-09"))
stopifnot(identical(fpa_fy_start_month(list(company = list(fiscal_year_end_month = 12L))), 1L))
stopifnot(identical(fpa_fy_start_month(list(company = list(fiscal_year_end_month = 3L))), 4L))
months <- sprintf("2026-%02d", 1:9)
pnl_y <- data.frame(
  periode = months, REV = 10, COGS = 4, GP = 6, EBITDA = 3, OI = 2, FIN = 1,
  stringsAsFactors = FALSE
)
apr <- fpa_health_month(bs, pnl_y, "2026-09", fy_start = 4L)
stopifnot(abs(apr$a[apr$id == "asset_turnover"] - 60) < 1e-9)
stopifnot(abs(apr$a[apr$id == "roa"] - 12) < 1e-9)
jan <- fpa_health_month(bs, pnl_y, "2026-09", fy_start = 1L)
stopifnot(abs(jan$a[jan$id == "asset_turnover"] - 90) < 1e-9)
cross_pers <- c("2025-01", "2025-02", "2025-03", sprintf("2025-%02d", 4:12), "2026-01", "2026-02")
pnl_x <- data.frame(
  periode = cross_pers, REV = 10, COGS = 1, GP = 9, EBITDA = 5, OI = 4, FIN = 1,
  stringsAsFactors = FALSE
)
bs_x <- bs
bs_x$periode <- "2026-02"
cross <- fpa_health_month(bs_x, pnl_x, "2026-02", fy_start = 4L)
stopifnot(abs(cross$a[cross$id == "asset_turnover"] - 110) < 1e-9)
liq <- fpa_health_fold_mix(c("current_ratio", "quick_ratio", "cash_ratio"), c(152, 152, 151))
stopifnot(liq$n_used == 3L && abs(liq$pos - 1) < 1e-9)
heavy <- fpa_health_fold_mix("debt_to_equity", 3)
stopifnot(abs(heavy$pos) < 1e-9)
light <- fpa_health_fold_mix("debt_to_equity", 0.45)
stopifnot(light$pos > 0.8)
turn <- fpa_health_fold_mix(c("dso_days", "dio_days", "asset_turnover"), c(1, 1, 0.7))
stopifnot(turn$n_used == 2L && turn$n_all == 3L && turn$pos > 0.9)
stopifnot(identical(fpa_health_fold_zone(0.2), "attn"))
stopifnot(identical(fpa_health_fold_zone(1 / 3), "mid"))
stopifnot(identical(fpa_health_fold_zone(0.5), "mid"))
stopifnot(identical(fpa_health_fold_zone(2 / 3), "good"))
stopifnot(identical(fpa_health_fold_zone(1), "good"))
screen <- fpa_health_fold_mix(c("book_ev", "roce"), c(96000, 0.42))
stopifnot(screen$n_used == 1L && abs(screen$pos - 1) < 1e-9)
roa_txt <- fpa_txt("health_def_roa", "avril")
stopifnot(grepl("avril", roa_txt, fixed = TRUE))
stopifnot(grepl("premier mois de l'exercice", roa_txt, fixed = TRUE))
stopifnot(!grepl("janvier", roa_txt, fixed = TRUE))
z <- fpa_health_gauge_zone("current_ratio", 1.4)
stopifnot(identical(z$key, "health_band_ordinary"))
z2 <- fpa_health_gauge_zone("cash_ratio", 2700)
stopifnot(identical(z2$key, "health_band_ample") && isTRUE(z2$beyond))
z3 <- fpa_health_gauge_zone("debt_to_equity", 0.45)
stopifnot(identical(z3$key, "health_band_light"))

opt <- fpa_optional_data_dir()
hotel_bs <- if (is.null(opt)) "" else file.path(opt, "work", "bs-monthly.csv")
hotel_pnl <- if (is.null(opt)) "" else file.path(opt, "work", "pnl-monthly.csv")
if (nzchar(hotel_bs) && file.exists(hotel_bs) && file.exists(hotel_pnl)) {
  hbs <- utils::read.csv(hotel_bs, stringsAsFactors = FALSE)
  hpn <- utils::read.csv(hotel_pnl, stringsAsFactors = FALSE)
  live <- fpa_health_month(hbs, hpn, "2026-09")
  stopifnot(nrow(live) == 15L)
  stopifnot(is.finite(live$value[live$id == "cash_ratio"]))
  stopifnot(is.finite(live$value[live$id == "book_ev"]))
  cat("hotel cash_ratio", live$value[live$id == "cash_ratio"],
      "book_ev", live$value[live$id == "book_ev"],
      "oi_margin", live$value[live$id == "oi_margin"], "\n")
} else {
  fpa_notify_skip_optional_data("hotel health checks")
}
cat("PASS health\n")
