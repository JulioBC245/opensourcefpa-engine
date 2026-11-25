# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
#' Live check on the hotel file: levers read their base, the engine moves, and a
#' run is fast enough for a debounced slider.
root <- "."
source(file.path(root, "tests", "helpers.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(root)

cfg <- fpa_optional_data_config()
if (is.null(cfg)) fpa_skip_if_no_optional_data("live scenario checks")
config <- fpa_load_config(cfg)
paths <- fpa_paths(config)
ids <- fpa_enabled_assumption_ids(config)
cat("enabled ids:", paste(ids, collapse = ", "), "\n")

a <- fpa_read_assumptions(config, paths)
cat("assumptions readable:", isTRUE(a$ok), "\n")
base <- fpa_scenario_base_values(a$wide, ids)
for (n in names(base)) {
  cat("  base", n, "=", base[[n]], "slider", fpa_scenario_lever_spec(n)$kind, "\n")
}

t0 <- Sys.time()
b <- fpa_run(cfg, write = FALSE, stop_on_error = FALSE)
secs_base <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
kb <- fpa_scenario_cash_kpi(b, 0)
cat("base run", round(secs_base, 2), "s | close", round(kb$close),
    "| low", round(kb$low), kb$low_month, "| under", kb$under, "\n")

t1 <- Sys.time()
s <- fpa_scenario_run(cfg, base, list(dso = 14), list(REV = -10))
secs_scen <- as.numeric(difftime(Sys.time(), t1, units = "secs"))
ks <- fpa_scenario_cash_kpi(s, 0)
cat("scenario run", round(secs_scen, 2), "s | close", round(ks$close),
    "| delta", round(ks$close - kb$close), "\n")
cat("moves:", paste(fpa_scenario_moves(base, list(dso = 14), list(REV = -10)), collapse = " | "), "\n")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}
say(isTRUE(a$ok) && length(base) > 0, paste("levers found:", length(base)))
say(any(vapply(names(base), function(i) identical(fpa_scenario_lever_spec(i)$kind, "days"), logical(1))),
    "at least one day lever")
say(is.finite(kb$close), "base closing cash computed")
say(is.finite(ks$close) && abs(ks$close - kb$close) > 1, "moving levers changes closing cash")
say(secs_scen < 6, paste("scenario run under 6 s:", round(secs_scen, 2)))

only_dso <- fpa_scenario_run(cfg, base, list(dso = 14), list())
k_dso <- fpa_scenario_cash_kpi(only_dso, 0)
say(abs(k_dso$close - kb$close) > 1, "DSO alone moves cash")
say(abs(k_dso$close - ks$close) > 1, "two levers together differ from one alone")
say(abs(sum(only_dso$pnl$REV, na.rm = TRUE) - sum(b$pnl$REV, na.rm = TRUE)) < 1,
    "DSO does not move revenue")

cat(if (fail) paste0("\n", fail, " CHECK(S) FAILED\n") else "\nALL CHECKS PASSED\n")
