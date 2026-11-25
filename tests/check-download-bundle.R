# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Prove the subscriber download works in a folder that has no R/ directory.
# The assembler writes a temp fpa-engine.R. This script never overwrites
# the repository's fpa-engine.R source or any assembled copy.

root <- "."
source(file.path(root, "tests", "helpers.R"), encoding = "UTF-8")
cfg <- fpa_optional_data_config()
if (is.null(cfg)) {
  fpa_notify_skip_optional_data("download-bundle statement run")
  cfg <- ""
}
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")
dest_dir <- tempfile("fpa-download-")
dir.create(dest_dir, recursive = TRUE)
dest <- file.path(dest_dir, "fpa-engine.R")
asm <- system2(
  rscript,
  c(file.path(root, "tools", "assemble-engine.R"), dest),
  stdout = TRUE,
  stderr = TRUE
)
status <- attr(asm, "status")
if (!is.null(status) && !identical(as.integer(status), 0L)) {
  stop("assemble failed:\n", paste(asm, collapse = "\n"), call. = FALSE)
}
copied <- file.copy(file.path(root, "app.R"), file.path(dest_dir, "app.R"), overwrite = TRUE)
if (!isTRUE(copied)) stop("app.R was not copied into the download folder")
if (dir.exists(file.path(dest_dir, "R"))) stop("the download folder must not contain R/")
if (!file.exists(dest)) stop("assembled engine missing")

probe <- file.path(dest_dir, "probe.R")
probe_src <- sprintf('
options(fpa.skip_main = TRUE)
app_root <- normalizePath(getwd(), winslash = "/")
if (dir.exists(file.path(app_root, "R"))) stop("R/ must be absent")
moteur <- file.path(app_root, "fpa-engine.R")
if (!file.exists(moteur)) stop("fpa-engine.R missing beside app.R")
sys.source(moteur, envir = .GlobalEnv)
need <- c(
  "fpa_journal_read", "fpa_journal_write", "fpa_outlook_weeks", "fpa_outlook_read",
  "fpa_objectif_solve", "fpa_objectif_read", "fpa_oi_cell_read", "fpa_cash_cell_read",
  "fpa_bs_cell_read", "fpa_stmt_ratio", "fpa_line_nature", "fpa_period_chip_label",
  "fpa_run", "fpa_app"
)
miss <- need[!vapply(need, exists, logical(1), mode = "function")]
if (length(miss)) stop("functions missing from the download: ", paste(miss, collapse = ", "))
txt <- paste(readLines(moteur, warn = FALSE, encoding = "UTF-8"), collapse = "\\n")
if (grepl("astra", txt, ignore.case = TRUE)) stop("the download still contains the builder name")
marks <- c(
  "fpa-cell-read.R", "fpa-objectif.R", "fpa-op-fin.R", "fpa-stmt-view.R",
  "fpa-common.R", "fpa-journal.R", "fpa-outlook.R"
)
for (mark in marks) {
  if (!grepl(paste0("# === ", mark, " ==="), txt, fixed = TRUE)) stop("marker missing: ", mark)
}
screen <- paste(readLines("app.R", warn = FALSE, encoding = "UTF-8"), collapse = "\\n")
if (!grepl("fpa_journal_read", screen, fixed = TRUE)) stop("app.R does not call the journal")
if (!grepl("fpa_outlook_read", screen, fixed = TRUE)) stop("app.R does not call the four-week outlook")
if (!grepl("fpa_objectif", screen, fixed = TRUE)) stop("app.R does not call the year-end target")
weeks <- fpa_outlook_weeks(as.Date("2026-09-30"), 4L)
if (length(weeks) != 4L) stop("four-week outlook did not return 4 weeks")
if (abs(fpa_stmt_ratio(35, 100) - 35) > 1e-9) stop("statement percent is wrong")
paths <- list(work = file.path(app_root, "work"))
dir.create(paths$work, recursive = TRUE, showWarnings = FALSE)
ok <- fpa_journal_write(paths, list(list(type = "observation", title = "probe", amount = 12)))
if (!isTRUE(ok)) stop("journal write failed")
got <- fpa_journal_read(paths)
if (length(got) != 1L || !identical(got[[1]]$title, "probe")) stop("journal read failed")
cfg <- "%s"
if (nzchar(cfg) && file.exists(cfg)) {
  res <- fpa_run(cfg, write = FALSE, stop_on_error = TRUE)
  if (is.null(res$pnl) || !nrow(res$pnl)) stop("statements did not run")
  if (is.null(res$cash) || !nrow(res$cash)) stop("cash bridge did not run")
  if (is.null(res$bs) || !nrow(res$bs)) stop("balance sheet did not run")
  config <- fpa_load_config(cfg)
  ye <- fpa_year_end_month(config, res$status, as.character(res$pnl$periode))
  cash <- fpa_objectif_read(res, "cash_ye", ye)
  oi <- fpa_objectif_read(res, "oi_ye", ye)
  cat("year-end ", ye, " cash ", round(cash), " oi ", round(oi), "\\n", sep = "")
  if (!is.finite(cash) || !is.finite(oi)) stop("year-end target has no figure")
  per <- as.character(res$pnl$periode[[nrow(res$pnl)]])
  oi_trail <- fpa_oi_cell_read(config, per)
  cash_trail <- fpa_cash_cell_read(config, per)
  bs_trail <- fpa_bs_cell_read(config, per)
  if (!is.list(oi_trail) || !is.list(cash_trail) || !is.list(bs_trail)) stop("calculation trail did not return")
  outlook <- fpa_outlook_read(config)
  if (!is.list(outlook)) stop("four-week outlook did not read")
  cat("download bundle ok\\n")
} else {
  cat("download bundle ok (statements skipped: optional private test data absent)\\n")
}
', gsub("\\\\", "/", cfg))
con <- file(probe, "wb")
writeBin(charToRaw(enc2utf8(probe_src)), con)
close(con)

old <- getwd()
setwd(dest_dir)
on.exit(setwd(old), add = TRUE)
out <- system2(rscript, "probe.R", stdout = TRUE, stderr = TRUE)
code <- attr(out, "status")
cat(paste(out, collapse = "\n"), "\n", sep = "")
if (!is.null(code) && !identical(as.integer(code), 0L)) {
  stop("isolated download failed", call. = FALSE)
}
if (!any(grepl("download bundle ok", out, fixed = TRUE))) {
  stop("isolated download did not finish", call. = FALSE)
}
cat("check-download-bundle PASS\n")
