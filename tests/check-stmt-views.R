# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Waterfall tie-out and percent-of-base.
source("R/fpa-stmt-view.R", encoding = "UTF-8")

row <- data.frame(
  REV = 100, OTHER_OP_INC = 10, COGS = 40, GP = 70,
  PAYROLL = 20, OPEX_OTHER = 10, EBITDA = 40,
  D_A = 5, OI = 35, FIN = 3, EXCEP = 0, TAX = 8, NI = 24,
  stringsAsFactors = FALSE
)
built <- fpa_pnl_waterfall_build(row)
stopifnot(!is.null(built))
stopifnot(isTRUE(built$ok))
last <- built$bars[nrow(built$bars), , drop = FALSE]
stopifnot(identical(last$id, "NI"))
stopifnot(abs(last$end - 24) < 1e-6)
# Dropped zero exceptional item
stopifnot(!"EXCEP" %in% built$bars$id)
# Connectors are horizontal (same y on both ends is stored as one y)
stopifnot(!is.null(built$connectors))
stopifnot(abs(built$connectors$y[[1]] - 100) < 1e-6)
gap <- built$connectors$x1 - built$connectors$x0
stopifnot(all(gap > 0.5))

stopifnot(abs(fpa_stmt_ratio(35, 100) - 35) < 1e-9)
stopifnot(is.na(fpa_stmt_ratio(35, 0)))
stopifnot(abs(fpa_stmt_delta(110, 100) - 10) < 1e-9)
stopifnot(is.na(fpa_stmt_delta(110, NA)))

# Broken books must fail the tie
bad <- row
bad$NI <- 99
built_bad <- fpa_pnl_waterfall_build(bad)
stopifnot(isFALSE(built_bad$ok))

# Hover carries a margin line on GP
gp <- built$bars[built$bars$id == "GP", , drop = FALSE]
stopifnot(grepl("Marge brute", gp$hover_extra[[1]], fixed = TRUE))
stopifnot(grepl("% du chiffre", gp$hover_extra[[1]], fixed = TRUE))

# Two months sum
pnl <- rbind(
  cbind(periode = "2026-01", row),
  cbind(periode = "2026-02", row)
)
summed <- fpa_pnl_waterfall_sum(pnl, c("2026-01", "2026-02"))
stopifnot(abs(summed$REV - 200) < 1e-9)
built2 <- fpa_pnl_waterfall_build(summed)
stopifnot(isTRUE(built2$ok))
stopifnot(abs(built2$bars$end[built2$bars$id == "NI"] - 48) < 1e-6)

# Range compare on share of CA + FIFO
row_a <- row
row_b <- row
row_b$COGS <- 80
row_b$GP <- 30
row_b$OPEX_OTHER <- 5
cmp <- fpa_pnl_waterfall_compare(row_a, row_b, top_n = 3L)
stopifnot(!is.null(cmp) && nrow(cmp) >= 1L)
stopifnot("COGS" %in% cmp$id)
stopifnot(abs(cmp$pct[cmp$id == "COGS"] - 40) < 1e-6)
# REV is never ranked (always 100 % of itself)
stopifnot(!"REV" %in% cmp$id)
mem <- list()
mem <- fpa_pnl_fifo_push(mem, "one", 5L)
mem <- fpa_pnl_fifo_push(mem, "two", 5L)
mem <- fpa_pnl_fifo_push(mem, "three", 5L)
mem <- fpa_pnl_fifo_push(mem, "four", 5L)
mem <- fpa_pnl_fifo_push(mem, "five", 5L)
mem <- fpa_pnl_fifo_push(mem, "six", 5L)
stopifnot(length(mem) == 5L)
stopifnot(identical(mem[[1]], "six"))
stopifnot(identical(mem[[5]], "two"))
stopifnot(identical(fpa_pnl_range_label(c("2026-01", "2026-02", "2026-03"), 1L, 3L), "2026-01\u20132026-03"))

# Year span: calendar month 1 is January. Live months replace history.
live <- data.frame(
  periode = c("2026-01", "2026-06"),
  REV = c(10, 20),
  stringsAsFactors = FALSE
)
hist <- data.frame(
  periode = c("2023-09", "2024-01", "2024-06", "2025-01", "2026-01"),
  REV = c(1, 100, 200, 300, 999),
  stringsAsFactors = FALSE
)
book <- fpa_pnl_wf_book(live, hist)
stopifnot(identical(fpa_pnl_wf_years(book), c("2023", "2024", "2025", "2026")))
stopifnot(abs(book$REV[book$periode == "2026-01"] - 10) < 1e-9)
stopifnot("2026-06" %in% book$periode)
stopifnot(abs(book$REV[book$periode == "2024-01"] - 100) < 1e-9)
span24 <- fpa_pnl_wf_span(book, "2024", c(1L, 6L))
stopifnot(identical(span24$found, c("2024-01", "2024-06")))
stopifnot("2024-02" %in% span24$missing)
stopifnot(length(span24$missing) == 4L)
span23 <- fpa_pnl_wf_span(book, "2023", c(1L, 6L))
stopifnot(!length(span23$found))
stopifnot(length(span23$missing) == 6L)
span23b <- fpa_pnl_wf_span(book, "2023", c(9L, 12L))
stopifnot(identical(span23b$found, "2023-09"))
stopifnot(length(span23b$missing) == 3L)
stopifnot(identical(fpa_pnl_wf_year_pick(NULL, c("2024", "2026")), "2026"))
stopifnot(identical(fpa_pnl_wf_year_pick("2024", c("2024", "2026")), "2024"))
stopifnot(identical(fpa_pnl_wf_year_pick("1999", c("2024", "2026")), "2026"))
summed_y <- fpa_pnl_waterfall_sum(book, span24$found)
stopifnot(abs(summed_y$REV - 300) < 1e-9)
# January–June of a full year is six months, position 1 through 6.
full <- data.frame(
  periode = sprintf("2025-%02d", 1:12),
  REV = 1:12,
  stringsAsFactors = FALSE
)
h1 <- fpa_pnl_wf_span(full, "2025", c(1L, 6L))
stopifnot(identical(h1$found, sprintf("2025-%02d", 1:6)))
stopifnot(!length(h1$missing))
stopifnot(abs(fpa_pnl_waterfall_sum(full, h1$found)$REV - sum(1:6)) < 1e-9)

# Health fold bands mirror Anomalies act/watch/hold
source("R/fpa-health.R", encoding = "UTF-8")
stopifnot(identical(fpa_health_fold_band("cash_ratio", 0.05), "act"))
stopifnot(identical(fpa_health_fold_band("cash_ratio", 0.3), "hold"))
stopifnot(identical(fpa_health_fold_band("debt_to_equity", 0.5), "hold"))
stopifnot(identical(fpa_health_fold_band("debt_to_equity", 2.5), "act"))
stopifnot(identical(fpa_health_fold_band("book_ev", 1000), "hold"))

cat("PASS 49\n")
