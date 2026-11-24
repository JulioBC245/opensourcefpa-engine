# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Fiscal-year snapshot, month tags, note body, print HTML.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-common.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-report.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

box <- tempfile("fpa-scope-")
dir.create(file.path(box, "work"), recursive = TRUE)
cfg <- file.path(box, "fpa-config.json")
writeLines("{}", cfg)
months <- format(seq(as.Date("2026-01-01"), as.Date("2028-03-01"), by = "month"), "%Y-%m")
status <- ifelse(months <= "2026-09", "actual", "forecast")
utils::write.csv(
  data.frame(periode = months, status = status, stringsAsFactors = FALSE),
  file.path(box, "work", "period-status.csv"),
  row.names = FALSE, fileEncoding = "UTF-8"
)
snap <- fpa_rep_snapshot(box, scenario = "Attendu", currency = "EUR")
say(identical(snap$year, "2026"), "fiscal year is the cutoff year")
say(identical(snap$cutoff, "2026-09"), "cutoff is the last actual")
say(identical(snap$months, months[substr(months, 1, 4) == "2026"]), "months stay inside 2026")
say(identical(snap$bs_months, c("2026-09", "2026-12")), "balance sheet is cutoff and year end")
say(identical(unname(snap$status[["2026-09"]]), "actual"), "September is actual")
say(identical(unname(snap$status[["2026-10"]]), "forecast"), "October is forecast")

pnl <- data.frame(
  periode = months,
  REV = seq_along(months) * 1000,
  OI = seq_along(months) * 100,
  NI = seq_along(months) * 10,
  stringsAsFactors = FALSE
)
fr <- fpa_rep_pnl_frame(pnl, months = snap$months, status = snap$status)
say(!any(grepl("^2027", names(fr))), "income statement has no 2027 column")
say("2026-09 R" %in% names(fr), "actual month is tagged R")
say("2026-10 P" %in% names(fr), "forecast month is tagged P")
say(sum(grepl("^[0-9]{4}-[0-9]{2}", names(fr))) == 12L, "twelve months of the year")

bs <- data.frame(periode = months, CASH = seq_along(months), AR = 1, stringsAsFactors = FALSE)
bsf <- fpa_rep_bs_frame(bs, months = snap$bs_months, status = snap$status, line_name_fn = function(x) if (x == "CASH") "Trésorerie" else x)
say(identical(bsf$Poste, c("Trésorerie", "AR")), "balance-sheet code becomes a business name")
say(identical(names(bsf), c("Poste", "2026-09 R", "2026-12 P")), "balance sheet columns are the two anchors")

got <- fpa_rep_table_html(fr)
say(grepl("fpa-rep-wide", got$html, fixed = TRUE), "twelve month columns use the landscape page")

md <- paste(c(
  "title: Test", "provider: openai", "model: x", "started: 2026",
  "## Note", "La note utile.", "## Annexe", "provider: secret"
), collapse = "\n")
say(grepl("La note utile", fpa_rep_note_body(md, FALSE), fixed = TRUE), "note section is kept")
say(!grepl("Annexe", fpa_rep_note_body(md, FALSE), fixed = TRUE), "appendix stays out by default")
say(grepl("Annexe", fpa_rep_note_body(md, TRUE), fixed = TRUE), "audit appendix is optional")
say(!grepl("^provider:", fpa_rep_note_body("provider: openai\nBonjour", FALSE)), "header line is dropped when there is no Note heading")

html <- fpa_rep_html(
  list(list(id = "t", kind = "text", title = "T", text = "Valeur %s. Survolez un mois.", ai = "")),
  list(title = "Revue", company = "Hotel", currency = "EUR", period = "Exercice 2026", lang = "en")
)
say(grepl("lang=\"en\"", html, fixed = TRUE), "html language follows the screen")
say(!grepl("%s", html, fixed = TRUE), "a leftover placeholder is removed")
say(!grepl("Survolez", html, fixed = TRUE), "hover instruction is removed")

paths <- fpa_rep_start_paths(cfg, "Revue")
say(grepl("-[0-9]{6}-", basename(paths$pdf)), "file stamp includes seconds")
dir.create(dirname(paths$pdf), recursive = TRUE, showWarnings = FALSE)
writeLines("x", paths$pdf)
paths2 <- fpa_rep_start_paths(cfg, "Revue")
say(!identical(paths$pdf, paths2$pdf), "an existing file gets a new name")

pre <- fpa_rep_presets()
say(identical(pre[[1]]$ids, c("syn_cards", "syn_cash", "pnl_wf", "var_bridge", "ai_note")), "monthly preset")
say(identical(fpa_rep_section_slice("pnl_table"), c("scope", "pnl_months", "headline")), "income-statement slice")
say(identical(fpa_rep_section_slice("ai_note"), character(0)), "the workshop note sends no slice")

cat("FAILS", fail, "\n")
quit(status = if (fail) 1 else 0)
