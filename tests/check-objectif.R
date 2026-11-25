# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Objectif G2: year-end month, metric read, grid, reverse solve on hotel pack.
root <- "."
source(file.path(root, "tests", "helpers.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(root)

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

say(identical(fpa_objectif_grid(-20, 20, 5), seq(-20, 20, by = 5)), "grid includes zero and step")
say(identical(fpa_objectif_grid(10, -10, 5), seq(-10, 10, by = 5)), "grid swaps min/max")
say(identical(fpa_objectif_snap(7, -20, 20, 5), 5), "snap 7 to step 5")
pct_cash <- fpa_objectif_pct(33600, 59799)
say(is.finite(pct_cash) && abs(pct_cash - (100 * (33600 - 59799) / 59799)) < 1e-9,
    paste("pct vs projection", round(pct_cash, 1)))
say(is.na(fpa_objectif_pct(33600, 0)), "pct undefined when projection is zero")
pct_neg <- fpa_objectif_pct(1000, -3462)
say(is.finite(pct_neg) && pct_neg > 0 && abs(pct_neg - (100 * 4462 / 3462)) < 1e-6,
    paste("pct above a negative projection", round(pct_neg, 1)))
say(fpa_objectif_pct(-5000, -3462) < 0, "a lower target on a loss is a negative pct")
say(identical(fpa_objectif_line_step(5), 1), "line search step caps a 5-point tick at 1")
say(identical(fpa_objectif_line_step(0.5), 0.5), "line search keeps a step already under 1")
ranked <- list(
  list(lever = list(kind = "line", id = "REV")),
  list(lever = list(kind = "line", id = "COGS")),
  list(lever = list(kind = "assum", id = "DSO"))
)
recs <- fpa_objectif_recipes(ranked, 5)
say(length(recs) >= 3L && length(recs) <= 5L, paste("recipes count", length(recs)))
say(identical(recs[[1]]$key, "line:REV"), paste("first recipe", recs[[1]]$key))
say(any(vapply(recs, function(r) identical(r$key, "line:COGS+line:REV"), logical(1))), "pair recipe present")

cfg_dec <- list(company = list(fiscal_year_end_month = 12L))
st <- data.frame(periode = c("2026-09", "2026-10"), status = c("actual", "forecast"), stringsAsFactors = FALSE)
ye <- fpa_year_end_month(cfg_dec, st, c("2026-09", "2026-10", "2026-11", "2026-12"))
say(identical(ye, "2026-12"), paste("YE Dec from closed sept:", ye))

cfg_mar <- list(company = list(fiscal_year_end_month = 3L))
ye_m <- fpa_year_end_month(cfg_mar, st, c("2026-01", "2026-02", "2026-03", "2026-04"))
say(identical(ye_m, "2027-03"), paste("March close after a September close is the next one:", ye_m))
st_feb <- data.frame(periode = c("2026-02", "2026-03"), status = c("actual", "forecast"), stringsAsFactors = FALSE)
say(identical(fpa_year_end_month(cfg_mar, st_feb, c("2026-01", "2026-02", "2026-03")), "2026-03"),
    "March close after a February close is this March")

fake <- list(
  cash = data.frame(periode = c("2026-11", "2026-12"), cash_close = c(50000, 60000), stringsAsFactors = FALSE),
  pnl = data.frame(periode = c("2026-11", "2026-12"), OI = c(1000, 2000), stringsAsFactors = FALSE)
)
say(isTRUE(all.equal(fpa_objectif_read(fake, "cash_ye", "2026-12"), 60000)), "read cash YE")
say(isTRUE(all.equal(fpa_objectif_read(fake, "oi_ye", "2026-12"), 2000)), "read OI YE")
say(isTRUE(all.equal(fpa_objectif_read(fake, "oi_fy", "2026-12"), 3000)), "read OI fiscal year")

cfg <- fpa_optional_data_config()
if (is.null(cfg)) {
  fpa_notify_skip_optional_data("live objective solve")
  cat(if (fail) paste0("\n", fail, " CHECK(S) FAILED\n") else "\nALL CHECKS PASSED\n")
  quit(save = "no", status = if (fail) 1L else 0L)
}

config <- fpa_load_config(cfg)
paths <- fpa_paths(config)
ids <- fpa_enabled_assumption_ids(config)
a <- fpa_read_assumptions(config, paths)
base <- fpa_scenario_base_values(a$wide, ids)
b <- fpa_run(cfg, write = FALSE, stop_on_error = FALSE)
status <- b$period_status
ye_live <- fpa_year_end_month(config, status, as.character(b$cash$periode))
base_cash <- fpa_objectif_read(b, "cash_ye", ye_live)
say(is.finite(base_cash), paste("live base cash YE", ye_live, round(base_cash)))

rev <- list(id = "REV", kind = "line", min = -20, max = 20, step = 5)
keys <- fpa_enabled_assumptions(config)

# Aim slightly above base with REV — should find a move, or closest / infeasible.
target <- base_cash + 500
got <- fpa_objectif_solve(
  cfg, base, keys, levers = list(rev),
  target = target, metric = "cash_ye", ye_month = ye_live,
  status = status, other_assum = list(), other_lines = list(),
  tol = 200, max_evals = 10L
)
d_rev <- got$deltas$lines$REV %||% NA_real_
say(isTRUE(got$ok) || identical(got$reason, "infeasible") || identical(got$reason, "closest"),
    paste("solve finished:", got$reason, "REV", d_rev, "value", round(got$value %||% NA), "evals", got$evals))
say(got$evals <= 10L, paste("evals capped:", got$evals))
if (isTRUE(got$ok) && identical(got$reason, "hit")) {
  say(is.finite(got$gap) && abs(got$gap) <= 200, paste("hit within tol, gap", round(got$gap)))
}

# 0.3% above the projection. A 5-point tick cannot land here; a 1-point step can.
rev_fine <- list(id = "REV", kind = "line", min = -20, max = 20, step = 1)
small_target <- base_cash + max(200, abs(base_cash) * 0.003)
small_tol <- max(50, abs(small_target) * 0.002)
got_small <- fpa_objectif_solve(
  cfg, base, keys, levers = list(rev_fine),
  target = small_target, metric = "cash_ye", ye_month = ye_live,
  tol = small_tol, max_evals = 16L
)
d_small <- got_small$deltas$lines$REV %||% NA_real_
say(
  identical(got_small$reason, "hit") && is.finite(d_small) && d_small != 0 && abs(d_small) <= 5,
  paste("0.3% cash via REV", got_small$reason, "REV", d_small, "value", round(got_small$value %||% NA), "gap", round(got_small$gap %||% NA))
)

# Two levers, joint effort, same modest target.
cogs <- list(id = "COGS", kind = "line", min = -20, max = 20, step = 5)
two <- fpa_objectif_solve(
  cfg, base, keys, levers = list(rev, cogs),
  target = target, metric = "cash_ye", ye_month = ye_live,
  tol = 200, max_evals = 12L
)
say(two$reason %in% c("hit", "closest", "infeasible", "already"),
    paste("two-lever:", two$reason, "evals", two$evals))

# Target far above reachable band → infeasible.
far <- fpa_objectif_solve(
  cfg, base, keys, levers = list(rev),
  target = base_cash + 5e6, metric = "cash_ye", ye_month = ye_live,
  tol = 50, max_evals = 8L
)
say(identical(far$reason, "infeasible") || identical(far$reason, "closest"),
    paste("far target:", far$reason))

# Auto: up to five mixes from the two P&L dials.
pool <- list(rev, cogs)
auto <- fpa_objectif_auto(
  cfg, base, keys, pool,
  target = target, metric = "cash_ye", ye_month = ye_live,
  tol = 200, n_mixes = 5L, max_evals_each = 8L
)
say(auto$reason %in% c("hit", "closest", "already", "infeasible", "no_help", "no_mix"),
    paste("auto:", auto$reason, "mixes", length(auto$mixes %||% list()), "evals", auto$evals))
say(length(auto$mixes %||% list()) <= 5L, paste("auto mix cap", length(auto$mixes %||% list())))

# December operating income sits under zero. Revenue past the ±20 dial is the path to 1 000.
oi_now <- fpa_objectif_read(b, "oi_ye", ye_live)
past <- fpa_objectif_past_line(
  cfg, base, keys, rev_fine,
  target = 1000, metric = "oi_ye", ye_month = ye_live,
  tol = 50, base_v = oi_now
)
d_past <- past$deltas$lines$REV %||% NA_real_
say(
  isTRUE(past$ok) && identical(past$reason, "hit") && is.finite(d_past) && d_past > 20 && abs(past$value - 1000) <= 50,
  paste("OI 1000 via REV past the dial:", d_past, "% →", round(past$value %||% NA), "from", round(oi_now))
)

# A leftover sales move is not the base. The screen solves from the reference.
opex <- list(id = "OPEX_OTHER", kind = "line", min = -20, max = 20, step = 5)
held <- fpa_objectif_solve(
  cfg, base, keys, levers = list(opex),
  target = 1000, metric = "oi_ye", ye_month = ye_live,
  other_lines = list(REV = 127),
  tol = 50, max_evals = 8L
)
say(
  identical(held$reason, "already") && is.finite(held$base) && abs(held$base - 1000) <= 50,
  paste("dirty board is already", held$reason, round(held$base %||% NA))
)
clean <- fpa_objectif_solve(
  cfg, base, keys, levers = list(opex),
  target = 1000, metric = "oi_ye", ye_month = ye_live,
  other_lines = list(REV = 0),
  tol = 50, max_evals = 8L
)
say(
  !identical(clean$reason, "already") && is.finite(clean$base) && clean$base < 0,
  paste("clean board", clean$reason, "base", round(clean$base %||% NA))
)

# Every lever except sales. The dial budget must not abort the solve, and the
# cost lines may leave ±20 together to reach 1 000.
payroll <- list(id = "PAYROLL", kind = "line", min = -20, max = 20, step = 5)
costs <- fpa_objectif_solve(
  cfg, base, keys, levers = list(payroll, opex, cogs),
  target = 1000, metric = "oi_ye", ye_month = ye_live,
  tol = 50, max_evals = 24L
)
say(
  identical(costs$reason, "hit") && is.finite(costs$value) && abs(costs$value - 1000) <= 50,
  paste("OI 1000 via cost lines:", costs$reason, round(costs$value %||% NA),
        "payroll", costs$deltas$lines$PAYROLL %||% NA,
        "opex", costs$deltas$lines$OPEX_OTHER %||% NA,
        "cogs", costs$deltas$lines$COGS %||% NA)
)

# Fiscal-year operating income is its own target: 2 000 above the year sum via sales.
oi_fy_now <- fpa_objectif_read(b, "oi_fy", ye_live)
say(is.finite(oi_fy_now) && abs(oi_fy_now - sum(b$pnl$OI[substr(b$pnl$periode, 1, 4) == substr(ye_live, 1, 4)])) < 0.01,
    paste("fiscal-year OI read", round(oi_fy_now)))
fy_goal <- fpa_objectif_solve(
  cfg, base, keys, levers = list(rev_fine),
  target = oi_fy_now + 2000, metric = "oi_fy", ye_month = ye_live,
  tol = 50, max_evals = 16L
)
say(
  identical(fy_goal$reason, "hit") && abs(fy_goal$value - (oi_fy_now + 2000)) <= 50,
  paste("fiscal-year OI +2000 via REV:", fy_goal$reason, fy_goal$deltas$lines$REV %||% NA, round(fy_goal$value %||% NA))
)

# The five ticks from the Impact screen. Cash-timing dials and variable cost
# do not move fiscal-year income on this file. Purchases do, and they stay in the answer.
day_lev <- function(id) {
  spec <- fpa_scenario_lever_spec(id, fpa_scenario_find(keys, id))
  list(id = id, kind = "assum", min = spec$min, max = spec$max, step = spec$step)
}
mix5 <- c(
  lapply(c("dso", "dpo", "dio", "cout.variable.pct"), day_lev),
  list(list(id = "COGS", kind = "line", min = -20, max = 20, step = 1))
)
mix_got <- fpa_objectif_solve(
  cfg, base, keys, levers = mix5,
  target = oi_fy_now + 211, metric = "oi_fy", ye_month = ye_live,
  tol = 50, max_evals = 48L
)
inert_ids <- vapply(mix_got$inert %||% list(), function(L) as.character(L$id), character(1))
say(
  identical(mix_got$reason, "hit") && is.finite(mix_got$deltas$lines$COGS %||% NA_real_) &&
    all(c("dso", "dpo", "dio", "cout.variable.pct") %in% inert_ids) && !"COGS" %in% inert_ids,
  paste("five ticks:", mix_got$reason, "COGS", mix_got$deltas$lines$COGS %||% NA,
        "inert", paste(inert_ids, collapse = ","), "value", round(mix_got$value %||% NA))
)

cat(if (fail) paste0("\n", fail, " CHECK(S) FAILED\n") else "\nALL CHECKS PASSED\n")
quit(save = "no", status = if (fail) 1L else 0L)
