# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Close-review flags. Books that tie stay quiet. A September that misses
# the driver, or the same month in prior years, does not.
root <- "."
setwd(root)
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

bs_ok <- data.frame(
  periode = "2026-09", CASH = 100, AR = 10, INV = 0, AP = 20, PPE = 50,
  ST_DEBT = 0, LT_DEBT = 40, OTHER_CA = 0, OTHER_CL = 0, EQUITY = 100,
  stringsAsFactors = FALSE
)
# assets 160, claims 20+40+100 = 160
cash_ok <- data.frame(
  periode = "2026-09", cash_open = 80, delta_cash = 20, cash_close = 100,
  stringsAsFactors = FALSE
)
books <- fpa_anomaly_books(bs_ok, cash_ok)
stopifnot(nrow(books) == 1L && books$level[[1]] == "clear")

bs_bad <- bs_ok
bs_bad$CASH <- 1000
bad <- fpa_anomaly_books(bs_bad, cash_ok)
stopifnot(any(bad$line_id == "BS" & bad$level == "break"))

pnl <- data.frame(
  periode = c("2026-09", "2026-10"),
  REV = c(9300, 8036),
  PAYROLL = c(4300, 5850),
  COGS = c(250, 262),
  OPEX_OTHER = c(2450, 2844),
  stringsAsFactors = FALSE
)
vol <- data.frame(
  periode = c("2026-09", "2026-10"),
  capacite = c(210, 217),
  taux_utilisation = c(0.60, 0.3779),
  tarif = c(120, 98),
  effectif = c(2.5, 2.69),
  cout_moyen = c(1720, 2174.8),
  stringsAsFactors = FALSE
)
drv <- fpa_anomaly_driver(pnl, vol)
stopifnot(any(drv$periode == "2026-09" & drv$line_id == "REV"))
stopifnot(!any(drv$periode == "2026-10" & drv$line_id == "REV"))

hist <- data.frame(
  libelle = c("Chiffre d'affaires", "Chiffre d'affaires"),
  periode = c("2024-09", "2025-09"),
  montant = c(18026, 18677),
  stringsAsFactors = FALSE
)
map <- c("Chiffre d'affaires" = "REV")
season <- fpa_anomaly_season(pnl, hist, map)
stopifnot(any(season$periode == "2026-09" & season$line_id == "REV"))
stopifnot(season$reference[season$periode == "2026-09" & season$line_id == "REV"] == stats::median(c(18026, 18677)))

thin_bs <- bs_ok
thin_bs$CASH <- 65530
thin_bs$AP <- 24
thin_bs$EQUITY <- 100 + (65530 - 100)
thin <- fpa_anomaly_thin(thin_bs, "2026-09")
stopifnot(nrow(thin) == 1L && thin$line_id[[1]] == "cash_ratio")

full <- fpa_anomaly_scan(bs_ok, pnl, cash_ok, vol, hist, map, "2026-09")
stopifnot(!any(full$line_id %in% c("beneish", "sloan", "energy_eur_per_t")))

hotel_bs <- "../fpa-local-test/work/bs-monthly.csv"
hotel_pnl <- "../fpa-local-test/work/pnl-monthly.csv"
hotel_cash <- "../fpa-local-test/work/cash-bridge-monthly.csv"
if (file.exists(hotel_bs)) {
  cfg <- fpa_load_config("../fpa-local-test/fpa-config.json")
  paths <- fpa_paths(cfg)
  h <- fpa_anomaly_scan(
    utils::read.csv(hotel_bs, stringsAsFactors = FALSE),
    utils::read.csv(hotel_pnl, stringsAsFactors = FALSE),
    utils::read.csv(hotel_cash, stringsAsFactors = FALSE),
    fpa_read_volumes(cfg, paths)$data,
    fpa_line_history(cfg, paths),
    fpa_line_map(cfg, "pnl"),
    c("2026-09", "2026-12")
  )
  cat("hotel families", paste(unique(h$family), collapse = ","), " rows", nrow(h), "\n")
  cat("breaks", sum(h$level == "break"), " watches", sum(h$level == "watch"), "\n")
  show <- h[h$level != "clear", c("family", "periode", "line_id", "gap")]
  print(utils::head(show, 12))
}
cat("PASS anomaly\n")
