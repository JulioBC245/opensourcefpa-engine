# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
#' Snapshot diff: threshold, ranking, empty case, first run with no prior snapshot.
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(root)

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

before <- list(
  timestamp = "2026-09-01 10:00:00",
  oi_year_end = 100000,
  cash_year_end = 60000,
  cash_closed = 65000,
  cash_closed_month = "2026-08",
  cash_low = 55000,
  cash_low_month = "2026-07",
  anom_act = 1L,
  anom_watch = 2L,
  stale = character(),
  cash_floor = 0,
  under_floor_month = ""
)
after_small <- before
after_small$oi_year_end <- 100500
# 500 EUR move under max(5% of 100k, 1000) = 5000 → ignored
d0 <- fpa_snapshot_diff(before, after_small)
say(length(d0) == 0L, "sub-threshold OI move ignored")

after <- before
after$oi_year_end <- 80000
after$cash_year_end <- 40000
after$anom_act <- 3L
after$under_floor_month <- "2026-11"
after$cash_floor <- 50000
d <- fpa_snapshot_diff(before, after)
say(length(d) <= 3L, paste("top-n capped at 3:", length(d)))
say(length(d) >= 1L, paste("material moves found:", length(d)))
ids <- vapply(d, function(r) r$id, character(1))
say("oi_year_end" %in% ids || "cash_year_end" %in% ids || "floor" %in% ids, paste("ranked ids:", paste(ids, collapse = ",")))
say(all(diff(vapply(d, function(r) r$abs_delta, numeric(1))) <= 0), "rows ranked by abs_delta desc")

say(length(fpa_snapshot_diff(NULL, after)) == 0L, "no prior snapshot → empty diff")
say(length(fpa_snapshot_diff(before, NULL)) == 0L, "no after snapshot → empty diff")

tmp <- tempfile("fpa-snap")
dir.create(tmp)
paths <- list(work = tmp)
built <- fpa_snapshot_build(
  data.frame(periode = "2026-12", OI = 12),
  data.frame(periode = c("2026-09", "2026-12"), cash_close = c(65942, 59799)),
  data.frame(periode = c("2026-09", "2026-12"), status = c("actual", "forecast")),
  list(act = 2L, watch = 4L),
  stale = "ops.xlsx",
  cash_floor = 70000
)
say(isTRUE(fpa_snapshot_write(paths, built)), "snapshot write")
got <- fpa_snapshot_read(paths)
say(!is.null(got) && identical(as.integer(got$anom_act), 2L), "snapshot read round-trip")
say(nzchar(got$under_floor_month), paste("under floor month:", got$under_floor_month))
say(identical(built$year_end_month, "2026-12") && built$oi_year_end == 12 && built$oi_fy == 12,
    "snapshot close month and both income readings")
mar_built <- fpa_snapshot_build(
  data.frame(periode = c("2026-09", "2026-12"), OI = c(5, 12)),
  data.frame(periode = c("2026-09", "2026-12"), cash_close = c(65942, 59799)),
  data.frame(periode = c("2026-09", "2026-12"), status = c("actual", "forecast")),
  list(act = 0L, watch = 0L),
  config = list(company = list(fiscal_year_end_month = 3L))
)
say(identical(mar_built$fy_close, "2027-03") && identical(mar_built$year_end_month, "2026-12") && mar_built$oi_fy == 17,
    "March close past the horizon reads December and sums the months present")
b2 <- before
b2$year_end_month <- "2026-12"
b2$oi_fy <- 30000
a2 <- b2
a2$oi_fy <- 20000
a2$oi_year_end <- 80000
d2 <- fpa_snapshot_diff(b2, a2)
labs <- vapply(d2, function(r) r$label, character(1))
say(any(labs == "Résultat d'exploitation de l'exercice") && any(labs == "Résultat d'exploitation de décembre 2026"),
    paste("diff labels:", paste(labs, collapse = " | ")))

unlink(tmp, recursive = TRUE)
cat(if (fail) paste0("\n", fail, " CHECK(S) FAILED\n") else "\nALL CHECKS PASSED\n")
quit(save = "no", status = if (fail) 1L else 0L)
