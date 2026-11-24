# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Gate 6 — Equity roll + A=L+E every month; deliberate error must break identity
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()
cfg_path <- fpa_ensure_fixtures(root)

res <- fpa_run(cfg_path, write = FALSE, stop_on_error = TRUE)
ident <- fpa_assert_identity(res$bs, tol = 1)
stopifnot(isTRUE(ident$ok), length(ident$messages) == 0)

# Equity roll check: E_t = E_{t-1} + NI_t (- break costs; none here)
eq0 <- fpa_open_num(res$opening, "EQUITY")
stopifnot(identical(fpa_open_num(numeric(0), "AP"), 0))
stopifnot(identical(fpa_open_num(c(AR = 10), "AP"), 0))
stopifnot(identical(fpa_open_num(c(AP = 5), "AP"), 5))
for (i in seq_len(nrow(res$bs))) {
  expect <- eq0 + res$pnl$NI[[i]]
  stopifnot(abs(res$bs$EQUITY[[i]] - expect) < 1)
  eq0 <- res$bs$EQUITY[[i]]
}

broken <- fpa_bs_break_identity(res$bs, amount = 5000)
ident2 <- fpa_assert_identity(broken, tol = 1)
stopifnot(!isTRUE(ident2$ok), length(ident2$messages) >= 1)

message("PASS gate 6")
