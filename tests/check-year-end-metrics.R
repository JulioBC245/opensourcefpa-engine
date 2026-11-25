# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Year-end metrics: next fiscal close, the months it sums, and the hotel tie-out
# (cash 37 099, fiscal-year OI 33 215, December OI −3 462).
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "tests", "helpers.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(root)

fail <- 0L
n_ok <- 0L
say <- function(ok, msg) {
  if (!isTRUE(ok)) fail <<- fail + 1L else n_ok <<- n_ok + 1L
  cat(if (isTRUE(ok)) "ok   " else "FAIL ", msg, "\n", sep = "")
}
ym_seq <- function(from, n) {
  y <- as.integer(substr(from, 1, 4)); m <- as.integer(substr(from, 6, 7))
  idx <- (y * 12L + m - 1L) + 0:(n - 1L)
  sprintf("%04d-%02d", idx %/% 12L, idx %% 12L + 1L)
}
status_for <- function(pers, closed) {
  data.frame(periode = pers, status = ifelse(pers <= closed, "actual", "forecast"), stringsAsFactors = FALSE)
}

say(identical(fpa_fy_months("2027-03"), ym_seq("2026-04", 12)), "fiscal months April to March")
say(identical(fpa_fy_months("2026-12"), ym_seq("2026-01", 12)), "fiscal months January to December")
say(identical(fpa_ym_fr("2026-12"), "décembre 2026"), "month label in French")

cfg_dec <- list(company = list(fiscal_year_end_month = 12L))
cfg_mar <- list(company = list(fiscal_year_end_month = 3L))
hotel_pers <- ym_seq("2026-01", 12)

dec <- fpa_year_end_info(cfg_dec, status_for(hotel_pers, "2026-09"), hotel_pers)
say(identical(dec$close, "2026-12") && identical(dec$measured, "2026-12") && !dec$beyond,
    paste("December company, closed September: close", dec$close))
say(identical(dec$months, hotel_pers), "December company sums January to December")
say(identical(fpa_year_end_month(cfg_dec, status_for(hotel_pers, "2026-09"), hotel_pers), "2026-12"),
    "December close unchanged")

mar_pers <- ym_seq("2026-04", 12)
mar <- fpa_year_end_info(cfg_mar, status_for(mar_pers, "2026-09"), mar_pers)
say(identical(mar$close, "2027-03") && identical(mar$measured, "2027-03") && !mar$beyond,
    paste("March company on April–March horizon: close", mar$close))
say(identical(mar$months, mar_pers), "March company sums April 2026 to March 2027")

mar_h <- fpa_year_end_info(cfg_mar, status_for(hotel_pers, "2026-09"), hotel_pers)
say(identical(mar_h$close, "2027-03") && identical(mar_h$measured, "2026-12") && isTRUE(mar_h$beyond),
    paste("March close past the hotel horizon: close", mar_h$close, "measured", mar_h$measured))
say(identical(mar_h$months, ym_seq("2026-04", 9)), "past-horizon fiscal sum keeps the months present")

fake_pnl <- data.frame(periode = mar_pers, OI = seq_len(12), stringsAsFactors = FALSE)
fake_cash <- data.frame(periode = mar_pers, cash_close = 1000 * seq_len(12), stringsAsFactors = FALSE)
fake <- list(pnl = fake_pnl, cash = fake_cash)
say(fpa_objectif_read(fake, "oi_fy", "2027-03") == sum(1:12), "oi_fy April–March sum")
say(fpa_objectif_read(fake, "oi_ye", "2027-03") == 12, "oi_ye reads March")
say(fpa_objectif_read(fake, "cash_ye", "2027-03") == 12000, "cash_ye reads March")
say(fpa_objectif_read(list(cash = fake_cash[1:9, ]), "cash_ye", "2027-03") == 9000,
    "cash past the horizon reads the last month before the close")

one <- list(pnl = data.frame(periode = "2026-12", OI = -5), cash = data.frame(periode = "2026-12", cash_close = 10))
k1 <- fpa_scenario_cash_kpi(one, 0, "2026-12")
say(k1$oi_fy == -5 && k1$oi_last == -5 && k1$oi == k1$oi_fy, "single month: year and month agree")
k0 <- fpa_scenario_cash_kpi(NULL, 0, "2026-12")
say(is.na(k0$oi_fy) && is.na(k0$oi_last), "empty run gives NA")

cfg <- fpa_optional_data_config()
if (!is.null(cfg)) {
  config <- fpa_load_config(cfg)
  b <- fpa_run(cfg, write = FALSE, stop_on_error = FALSE)
  info <- fpa_year_end_info(config, b$period_status, as.character(b$pnl$periode))
  say(identical(info$close, "2026-12"), paste("hotel close", info$close))
  cash <- fpa_objectif_read(b, "cash_ye", info$close)
  oi_fy <- fpa_objectif_read(b, "oi_fy", info$close)
  oi_ye <- fpa_objectif_read(b, "oi_ye", info$close)
  # Expectations follow a fresh fpa_run of the optional private hotel dataset (not the last work CSVs).
  say(abs(cash - (-36642.19)) < 1, paste("hotel year-end cash", round(cash, 2)))
  say(abs(oi_fy - 31090.19) < 1, paste("hotel fiscal-year OI", round(oi_fy, 2)))
  say(abs(oi_ye - (-4170.36)) < 1, paste("hotel December OI", round(oi_ye, 2)))
  kpi <- fpa_scenario_cash_kpi(b, 0, info$close)
  say(abs(kpi$close - cash) < 0.01 && abs(kpi$oi_fy - oi_fy) < 0.01 && abs(kpi$oi_last - oi_ye) < 0.01,
      "scenario KPI ties to the objective read")
  snap <- fpa_snapshot_build(b$pnl, b$cash, b$period_status, list(act = 0L, watch = 0L), config = config)
  say(identical(snap$year_end_month, "2026-12") && abs(snap$oi_year_end - oi_ye) < 0.01 && abs(snap$oi_fy - oi_fy) < 0.01,
      "snapshot reads the same close month")
  snap_mar <- fpa_snapshot_build(b$pnl, b$cash, b$period_status, list(act = 0L, watch = 0L), config = cfg_mar)
  say(identical(snap_mar$fy_close, "2027-03") && identical(snap_mar$year_end_month, "2026-12"),
      "snapshot with a March close names the month it could read")
} else {
  fpa_notify_skip_optional_data("hotel year-end metric checks")
}

cat(sprintf("\n%d passed, %d failed\n", n_ok, fail))
cat(if (fail) paste0(fail, " CHECK(S) FAILED\n") else "ALL CHECKS PASSED\n")
quit(save = "no", status = if (fail) 1L else 0L)
