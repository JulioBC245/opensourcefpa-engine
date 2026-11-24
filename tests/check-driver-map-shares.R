# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Driver map: each P&L line's share in the spread of its subtotal.
# Shares of one subtotal must add to 100 %. Run from fpa-local: Rscript tests/check-driver-map-shares.R

`%||%` <- function(a, b) if (is.null(a)) b else a
root <- if (file.exists("app.R")) "." else ".."
source(file.path(root, "R", "fpa-driver-map.R"), encoding = "UTF-8")

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}
sums_to_one <- function(s) {
  all(vapply(s$nodes, function(n) abs(sum(n$share) - 1) < 1e-9, logical(1))) && abs(sum(s$to_oi) - 1) < 1e-9
}
build <- function(rev, cogs, payroll, opex, da, other = rep(0, length(rev))) {
  gp <- rev + other - cogs
  ebitda <- gp - payroll - opex
  data.frame(
    periode = sprintf("2025-%02d", seq_along(rev)) , REV = rev, OTHER_OP_INC = other, COGS = cogs,
    PAYROLL = payroll, OPEX_OTHER = opex, D_A = da, GP = gp, EBITDA = ebitda, OI = ebitda - da
  )
}

hotel_dir <- file.path(root, "..", "fpa-local-test", "work")
pnl <- utils::read.csv(file.path(hotel_dir, "pnl-monthly.csv"), stringsAsFactors = FALSE)
status <- utils::read.csv(file.path(hotel_dir, "period-status.csv"), stringsAsFactors = FALSE)
s <- fpa_driver_map_shares(pnl, status)
n_real <- sum(status$status != "forecast" & status$periode %in% pnl$periode)
check(sprintf("hotel: %d realised months used, forecast left out", n_real), isTRUE(s$ok) && s$n == n_real && s$n < nrow(pnl))
check("hotel: every subtotal rebuilds from its lines", all(vapply(s$nodes, function(n) n$gap < 2, logical(1))))
check("hotel: shares of each subtotal and of the result add to 100 %", sums_to_one(s))
real <- pnl[pnl$periode %in% status$periode[status$status != "forecast"], ]
by_hand <- -stats::cov(real$PAYROLL, real$EBITDA) / stats::var(real$EBITDA)
check(sprintf("hotel: seasonal payroll dampens the EBE by %s, same as by hand", fpa_driver_map_share_txt(by_hand)),
  !("PAYROLL" %in% s$fixed) && by_hand < 0 && abs(s$nodes$EBITDA$share[["PAYROLL"]] - by_hand) < 1e-12)
check("hotel: flat dotations weigh 0 % of the result", "D_A" %in% s$fixed && abs(s$nodes$OI$share[["D_A"]]) < 1e-12)
cat(sprintf("   hotel EBE: marge %s, salaires %s, autres charges %s\n",
  fpa_driver_map_share_txt(s$nodes$EBITDA$share[["GP"]]),
  fpa_driver_map_share_txt(s$nodes$EBITDA$share[["PAYROLL"]]),
  fpa_driver_map_share_txt(s$nodes$EBITDA$share[["OPEX_OTHER"]])))

rev <- c(100, 140, 180, 220, 260, 300)
t <- build(rev, cogs = 0.3 * rev, payroll = rep(50, 6), opex = rep(20, 6), da = rep(5, 6))
st <- fpa_driver_map_shares(t)
check("cost at 30 % of sales: sales 143 %, purchases −43 % of the margin",
  abs(st$nodes$GP$share[["REV"]] - 1 / 0.7) < 1e-9 && abs(st$nodes$GP$share[["COGS"]] + 0.3 / 0.7) < 1e-9)
check("negative share prints with a real minus", identical(fpa_driver_map_share_txt(st$nodes$GP$share[["COGS"]]), "\u221243 %"))
check("unused other income is listed as zero", "OTHER_OP_INC" %in% st$zero)

set.seed(7)
m <- 24
season <- 1 + 0.4 * sin(2 * pi * (1:m) / 12)
rev24 <- round(3000 * season + rnorm(m, 0, 150))
full <- build(rev24, cogs = round(0.03 * rev24), payroll = round(2800 + 300 * season), opex = round(1500 + rnorm(m, 0, 200)),
              da = rep(400, m), other = round(0.005 * rev24))
sf <- fpa_driver_map_shares(full)
check("24 months, every line moving: all shares add to 100 %", isTRUE(sf$ok) && sf$n == 24L && sums_to_one(sf))

one <- fpa_driver_map_shares(full[1, , drop = FALSE])
check("one month: shares refused, no crash", !isTRUE(one$ok) && identical(one$n, 1L))
check("two months: shares refused", !isTRUE(fpa_driver_map_shares(full[1:2, , drop = FALSE])$ok))
check("empty table: returns NULL", is.null(fpa_driver_map_shares(full[0, , drop = FALSE])))

flat <- build(rep(100, 5), rep(10, 5), rep(30, 5), rep(10, 5), rep(5, 5))
sfl <- fpa_driver_map_shares(flat)
check("nothing moves: shares are empty, not zero, not an error", isTRUE(sfl$ok) && all(is.na(sfl$nodes$GP$share)))

bad <- t
bad$GP[3] <- bad$GP[3] + 50
sb <- fpa_driver_map_shares(bad)
check("tampered margin: the check flags the gap of 50 EUR", abs(sb$nodes$GP$gap - 50) < 1e-9)

no_sub <- t[, setdiff(names(t), c("GP", "EBITDA")), drop = FALSE]
sn <- fpa_driver_map_shares(no_sub)
check("missing subtotal columns are rebuilt from the lines", isTRUE(sn$ok) && sums_to_one(sn))

check("hotel: displayed whole percents of each subtotal add to 100",
  all(vapply(s$nodes, function(n) sum(n$pct) == 100L, logical(1))) && sum(s$to_oi_pct) == 100L)
cat(sprintf("   hotel marge: %s\n", paste(names(s$nodes$GP$pct), s$nodes$GP$pct, collapse = ", ")))
check("rounding 1.024 / -0.031 / 0.007 shows 102, -3, 1 (sum 100)",
  identical(unname(fpa_driver_map_round_pct(c(a = 1.024, b = -0.031, c = 0.007))), c(102L, -3L, 1L)))
check("rounding keeps NA as NA", is.na(fpa_driver_map_round_pct(c(a = NA_real_, b = 1))[["a"]]))
check("formats: 0.824 -> 82 %, NA -> empty", identical(fpa_driver_map_share_txt(0.824), "82 %") && identical(fpa_driver_map_share_txt(NA), ""))

kind_of <- function(r, n, crit, sig, line) fpa_driver_map_edge_kind(r, n, crit, sig, line)$kind
check("12 months, |r| 0.29 under 0.58 is noise", identical(kind_of(0.29, 12, 0.58, FALSE, "REV"), "noise"))
check("24 months, |r| 0.70 over 0.40 stays an r", identical(kind_of(0.70, 24, 0.40, TRUE, "REV"), "r"))
check("payroll residual below zero says less cost", identical(kind_of(-0.57, 40, 0.34, TRUE, "PAYROLL"), "pay_down"))
check("payroll residual above zero says more cost", identical(kind_of(0.52, 40, 0.33, TRUE, "PAYROLL"), "pay_up"))
check("pairs with no correlation are constant", identical(kind_of(NA, 40, NA, FALSE, "REV"), "constant"))
check("a short blank series stays unlabeled", identical(kind_of(NA, 2, NA, NA, "REV"), "none"))

grades <- utils::read.csv(file.path(hotel_dir, "drivers", "driver-grades.csv"), stringsAsFactors = FALSE)
links <- fpa_driver_map_links(grades)
row_kind <- function(name) {
  i <- which(links$series_name == name)
  fpa_driver_map_edge_kind(
    links$r[[i]], links$n[[i]], links$r_crit[[i]], links$sig[[i]], links$target_line[[i]], links$series_name[[i]]
  )$kind
}
check("hotel precipitations edge is noise", identical(row_kind("precipitations_mm"), "noise"))
check("hotel temperature edge still clears its bar", identical(row_kind("temperature_c"), "r"))
check("hotel headcount raises payroll", identical(row_kind("effectif"), "pay_factor"))
check("hotel average cost raises payroll", identical(row_kind("cout_moyen"), "pay_factor"))
check("hotel capacity is constant", identical(row_kind("capacite"), "constant"))
temp <- grades[grades$series_name == "temperature_c", , drop = FALSE]
check("hotel temperature against the result is noise",
  identical(fpa_driver_map_edge_kind(temp$bottom_r, temp$bottom_n, NA, temp$bottom_sig, "")$kind, "noise"))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
