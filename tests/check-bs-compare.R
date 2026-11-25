# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Two balance-sheet dates: side deltas tie to the side total.
# Run from the opensourcefpa-engine root: Rscript tests/check-bs-compare.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    pass <<- pass + 1L
    cat("PASS", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
}

bs <- data.frame(
  periode = c("2025-08", "2026-08"),
  PPE = c(400, 360),
  CASH = c(100, 150),
  AR = c(0, 0),
  INV = c(0, 0),
  OTHER_CA = c(0, 20),
  EQUITY = c(250, 310),
  LT_DEBT = c(200, 180),
  AP = c(50, 40),
  OTHER_CL = c(0, 0),
  ST_DEBT = c(0, 0),
  stringsAsFactors = FALSE
)
cmp <- fpa_bs_compare(bs, "2025-08", "2026-08")
asset <- cmp$lines[cmp$lines$side == "asset", , drop = FALSE]
claim <- cmp$lines[cmp$lines$side == "claim", , drop = FALSE]
asset_side <- cmp$sides[cmp$sides$side == "asset", , drop = FALSE]
claim_side <- cmp$sides[cmp$sides$side == "claim", , drop = FALSE]

check("asset lines stay on the asset side", all(asset$side == "asset"))
check("claim lines stay on the claim side", all(claim$side == "claim"))
check("assets are listed before claims", identical(cmp$lines$side[[1]], "asset") && identical(utils::tail(cmp$lines$side, 1), "claim"))
check("asset order is fixed", identical(asset$id, c("PPE", "CASH", "OTHER_CA")))
check("zero lines are hidden", !any(c("AR", "INV", "OTHER_CL", "ST_DEBT") %in% cmp$lines$id))
check("asset deltas sum to the asset change", abs(sum(asset$delta) - asset_side$delta) < 0.01)
check("claim deltas sum to the claim change", abs(sum(claim$delta) - claim_side$delta) < 0.01)
check("balanced sheet: both sides move by 30", abs(asset_side$delta - 30) < 0.01 && abs(claim_side$delta - 30) < 0.01)
check("cash is up 50 percent", {
  row <- cmp$lines[cmp$lines$id == "CASH", , drop = FALSE]
  identical(row$move, "up") && abs(row$pct - 50) < 0.01 && abs(row$delta - 50) < 0.01
})
check("PPE is down 10 percent", {
  row <- cmp$lines[cmp$lines$id == "PPE", , drop = FALSE]
  identical(row$move, "down") && abs(row$pct - (-10)) < 0.01
})
check("a new asset is marked new", identical(cmp$lines$move[cmp$lines$id == "OTHER_CA"], "new"))
check("cash share of assets rises", {
  row <- cmp$lines[cmp$lines$id == "CASH", , drop = FALSE]
  abs(row$share_a - 100 / 500) < 1e-9 && abs(row$share_b - 150 / 530) < 1e-9 && row$share_pts > 0
})
check("asset shares sum to 1", abs(sum(asset$share_a, na.rm = TRUE) - 1) < 1e-9 && abs(sum(asset$share_b, na.rm = TRUE) - 1) < 1e-9)
check("claim shares sum to 1", abs(sum(claim$share_a, na.rm = TRUE) - 1) < 1e-9 && abs(sum(claim$share_b, na.rm = TRUE) - 1) < 1e-9)
check("AP is down", identical(cmp$lines$move[cmp$lines$id == "AP"], "down") && cmp$lines$delta[cmp$lines$id == "AP"] < 0)

cat(sprintf("PASS %d FAIL %d\n", pass, fail))
if (fail > 0L) quit(status = 1)
