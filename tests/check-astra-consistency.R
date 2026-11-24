# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
#' Astra consistency: anomaly focus/counts, cash period picks, period chip labels.
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(root)

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

status <- data.frame(
  periode = c("2026-08", "2026-09", "2026-10", "2026-11", "2026-12"),
  status = c("actual", "actual", "forecast", "forecast", "forecast"),
  stringsAsFactors = FALSE
)
focus <- fpa_anomaly_focus_periods(status)
say(identical(focus, c("2026-09", "2026-12")), paste("focus closed+YE:", paste(focus, collapse = ",")))

scan <- data.frame(
  family = c("books", "ratio", "driver", "season", "driver", "books"),
  level = c("watch", "watch", "watch", "watch", "clear", "clear"),
  stringsAsFactors = FALSE
)
counts <- fpa_anomaly_counts(scan)
say(identical(counts$act, 2L), paste("act count:", counts$act))
say(identical(counts$watch, 2L), paste("watch count:", counts$watch))
say(identical(counts$n, 4L), paste("total non-clear:", counts$n))
say(identical(fpa_anomaly_counts(NULL)$n, 0L), "empty scan is zero")

cash <- data.frame(
  periode = c("2026-08", "2026-09", "2026-10", "2026-11", "2026-12"),
  cash_close = c(70000, 65942, 64000, 61000, 59799),
  stringsAsFactors = FALSE
)
picks <- fpa_cash_period_picks(cash, status)
say(identical(picks$closed_month, "2026-09"), paste("closed month:", picks$closed_month))
say(abs(picks$closed - 65942) < 0.5, paste("closed cash:", picks$closed))
say(identical(picks$year_end_month, "2026-12"), paste("YE month:", picks$year_end_month))
say(abs(picks$year_end - 59799) < 0.5, paste("YE cash:", picks$year_end))
say(identical(picks$low_month, "2026-12"), paste("low month:", picks$low_month))

chip_a <- fpa_period_chip_label("actual", "2026-09")
chip_f <- fpa_period_chip_label("forecast", "2026-12")
say(grepl("^réalisé", chip_a), paste("actual chip:", chip_a))
say(grepl("^prévision", chip_f), paste("forecast chip:", chip_f))
say(!nzchar(fpa_period_chip_label("actual", "")), "empty period chip is blank")

hl <- fpa_synthese_pnl_headline(data.frame(periode = character(), REV = numeric()), span = NA)
say(nzchar(hl), "NA span headline guarded")
hl2 <- fpa_synthese_pnl_headline(NULL, span = NULL)
say(nzchar(hl2), "NULL pnl headline guarded")

cat(if (fail) paste0("\n", fail, " CHECK(S) FAILED\n") else "\nALL CHECKS PASSED\n")
quit(save = "no", status = if (fail) 1L else 0L)
