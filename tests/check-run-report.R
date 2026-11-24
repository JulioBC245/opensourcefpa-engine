# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Run report: one post after a clean real run. Nothing on a failed run,
# offline, in local mode, or from a scenario or sensitivity call.
# Run from fpa-local: Rscript tests/check-run-report.R

`%||%` <- function(a, b) if (is.null(a)) b else a
root <- if (file.exists("R/fpa-sync.R")) "." else ".."
source(file.path(root, "R", "fpa-sync.R"), encoding = "UTF-8")

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}

good <- list(
  pnl = data.frame(periode = "2026-01", REV = 1),
  identity = list(ok = TRUE),
  findings = data.frame(severity = "warning", message = "x", stringsAsFactors = FALSE)
)
empty <- good
empty$pnl <- good$pnl[0, , drop = FALSE]
unbalanced <- good
unbalanced$identity <- list(ok = FALSE)
errored <- good
errored$findings <- data.frame(severity = "error", message = "x", stringsAsFactors = FALSE)

check("clean run is reportable", isTRUE(fpa_sync_run_ok(good)))
check("empty P&L is not reportable", !isTRUE(fpa_sync_run_ok(empty)))
check("unbalanced balance sheet is not reportable", !isTRUE(fpa_sync_run_ok(unbalanced)))
check("an error finding is not reportable", !isTRUE(fpa_sync_run_ok(errored)))
check("a missing result is not reportable", !isTRUE(fpa_sync_run_ok(NULL)))

box <- tempfile("report")
dir.create(box)
cfg <- file.path(box, "fpa-config.json")
jsonlite::write_json(list(
  project = list(id = "p1", key = "k-secret-123", site = "http://127.0.0.1:9", sync_revision = 4L)
), cfg, auto_unbox = TRUE)
writeLines("# fpa-engine.R · v0.2.2 · test", file.path(box, "fpa-engine.R"))
writeLines("# app.R · v0.2.2 · test", file.path(box, "app.R"))

options(fpa.run_report_n = 0L)
options(fpa.offline = TRUE)
t0 <- Sys.time()
off <- fpa_sync_report_run(box, cfg, good)
check("offline: no post", !isTRUE(off) && getOption("fpa.run_report_n") == 0L &&
  as.numeric(difftime(Sys.time(), t0, units = "secs")) < 1)

options(fpa.offline = FALSE)
fpa_sync_set_mode(cfg, "local")
loc <- fpa_sync_report_run(box, cfg, good)
check("local mode: no post", !isTRUE(loc) && getOption("fpa.run_report_n") == 0L)
fpa_sync_set_mode(cfg, "connected")

bad <- fpa_sync_report_run(box, cfg, errored)
check("error finding: no post", !isTRUE(bad) && getOption("fpa.run_report_n") == 0L)

warned <- character(0)
out <- capture.output(res <- withCallingHandlers(
  fpa_sync_report_run(box, cfg, good),
  warning = function(w) { warned <<- c(warned, conditionMessage(w)); invokeRestart("muffleWarning") },
  message = function(m) { warned <<- c(warned, conditionMessage(m)); invokeRestart("muffleMessage") }
), type = "output")
out <- c(out, warned)
check("dead server: one attempt, not a success", getOption("fpa.run_report_n") == 1L && !isTRUE(res))
check("dead server: key never printed", !any(grepl("k-secret-123", out, fixed = TRUE)))

body <- jsonlite::fromJSON(file.path(box, "fpa-config.json"))
check("revision read from sync_revision", identical(as.integer(body$project$sync_revision), 4L))

has_call <- function(path) any(grepl("fpa_sync_report_run", readLines(path, warn = FALSE), fixed = TRUE))
check("fpa_run does not post", !has_call(file.path(root, "R", "fpa-run.R")))
check("scenarios do not post", !has_call(file.path(root, "R", "fpa-scenario.R")) &&
  !has_call(file.path(root, "R", "fpa-sensitivity.R")))
check("the engine entry posts", has_call(file.path(root, "fpa-engine.R")))
check("the screen posts", has_call(file.path(root, "app.R")))

banner <- readLines(file.path(root, "bundle-banner.txt"), warn = FALSE, encoding = "UTF-8")
need <- c("jsonlite", "shiny", "bslib", "plotly", "readxl", "openxlsx")
check("banner names the packages", all(vapply(need, function(p) any(grepl(p, banner, fixed = TRUE)), TRUE)))
check("banner says when openxlsx is needed", any(grepl("classeur", banner, fixed = TRUE)))

engine_line <- readLines(file.path(root, "fpa-engine.R"), n = 1L, warn = FALSE, encoding = "UTF-8")
screen_line <- readLines(file.path(root, "app.R"), n = 1L, warn = FALSE, encoding = "UTF-8")
check("engine header is v0.2.2", grepl("v0\\.2\\.2", engine_line))
check("screen header is v0.2.2", grepl("v0\\.2\\.2", screen_line))

cat(sprintf("PASS %d FAIL %d\n", pass, fail))
quit(status = if (fail) 1L else 0L)
