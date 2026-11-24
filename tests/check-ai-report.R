# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Report file: append, refuse after seal, hash, mixed mode, no key, no BOM.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-ai.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-ai-report.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

box <- tempfile("fpa-ai-rep-")
dir.create(box)
cfg <- file.path(box, "fpa-config.json")
writeLines("{}", cfg)
path <- fpa_ai_report_start(cfg, "Note de gestion", "2026-09", "sans", "openai", "gpt-4o-mini")
fpa_ai_report_append(path, "Gaps", "The gap is 12.3%.", "sent sans", "sans")
fpa_ai_report_append(path, "Cash", "Cash is 20.0%.", "sent avec hidden", "avec")
again <- fpa_ai_report_append(path, "Gaps", "The gap is 12.3%.", "sent sans", "sans")
text <- fpa_ai_read_utf8(path)
say(is.na(again), "same answer is not added twice")
say(isTRUE(fpa_ai_report_contains(path, "The gap is 12.3%.")), "exact answer is found")
say(isTRUE(fpa_ai_report_contains(path, "Cash is 20.0%.\n\n## Annexe\nLater text")), "saved section matches the start of a longer note")
say(!isTRUE(fpa_ai_report_contains(path, "A different answer.")), "a new answer is not already in")
say(length(gregexpr("The gap is 12.3%", text, fixed = TRUE)[[1]]) == 1L, "the gap appears once")
say(grepl("modes: sans,avec", text, fixed = TRUE), "mixed modes in the header")
raw <- readBin(path, "raw", 3)
say(!(length(raw) >= 3 && identical(as.integer(raw), c(239L, 187L, 191L))), "no BOM")
say(!grepl("sk-", text, fixed = TRUE), "no key in the report")
say(!fpa_ai_report_sealed(text), "a note works before any seal")

fpa_ai_report_seal(path)
sealed <- fpa_ai_read_utf8(path)
hash <- sub(".*sha256:", "", sealed)
hash <- trimws(strsplit(hash, "\n")[[1]][[1]])
body <- sub("\n---\nScellé.*", "", sealed)
say(identical(hash, fpa_ai_sha256(body)), "hash matches the body")
refused <- tryCatch({
  fpa_ai_report_append(path, "Later", "no", "no", "sans")
  FALSE
}, error = function(e) grepl("sealed", e$message))
say(isTRUE(refused), "append after seal is refused")

sans_only <- fpa_ai_report_start(cfg, "Sans", "2026-09", "sans", "openai", "m")
fpa_ai_report_append(sans_only, "One", "Share is 37.4%.", "no euros here", "sans")
msgs <- fpa_ai_report_finalize_messages(sans_only, "fr")
say(grepl("Do not add any", msgs[[2]]$content, fixed = TRUE), "finalize of a sans note forbids euros")
say(!grepl("74 882", fpa_ai_read_utf8(sans_only), fixed = TRUE), "sans report has no euro print")

unlink(box, recursive = TRUE)
cat("FAILS ", fail, "\n", sep = "")
quit(status = if (fail) 1 else 0)
