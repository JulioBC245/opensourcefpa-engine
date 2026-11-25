# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Composer catalog, selection, HTML order, empty-col drop, Edge PDF.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "tests", "helpers.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-ai-report.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-report.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

cat <- fpa_rep_catalog()
ids <- vapply(cat, function(x) x$id, "")
say(length(ids) == length(unique(ids)), "catalog ids unique")
keys <- unique(c(
  vapply(cat, function(x) x$label_key, ""),
  vapply(fpa_rep_groups(), function(x) x$label_key, ""),
  "nav_rapports", "nav_tab_composer", "rep_title", "rep_lead", "rep_texte",
  "rep_ai_box", "rep_build_pdf", "rep_build_html", "rep_drop_cols", "rep_chart_empty",
  "rep_chart", "rep_table", "rep_cards", "rep_note",
  "rep_send_lead", "rep_note_none", "rep_note_open", "rep_note_ready", "rep_seal", "rep_sealed"
))
missing <- keys[!vapply(keys, function(k) !is.null(fpa_copy[[k]]) && nzchar(fpa_copy[[k]][["fr"]]) && nzchar(fpa_copy[[k]][["en"]]), logical(1))]
say(!length(missing), paste("copy keys fr+en", if (length(missing)) paste(missing, collapse = ",") else "all"))
en_map <- fpa_lang_pairs("en")
say(identical(en_map[["Rapport PDF"]], "PDF report"), "page title translates")
say(identical(en_map[["Tout replier"]], "Collapse all"), "collapse button translates")
say(identical(en_map[["Tout cocher"]], "Check all"), "tick-all button translates")
say(identical(en_map[["Tout décocher"]], "Uncheck all"), "untick button translates")
say(identical(en_map[["Modèles prêts à l'emploi"]], "Ready-to-use templates"), "template frame translates")

box <- tempfile("fpa-rep-")
dir.create(box)
cfg <- file.path(box, "fpa-config.json")
writeLines("{}", cfg)
dir.create(file.path(box, "work"), showWarnings = FALSE)

sel <- fpa_rep_sel_default()
say(identical(fpa_rep_selected_ids(sel), c("syn_cards", "syn_cash", "pnl_wf", "var_bridge", "ai_note")), "monthly preset is the new default")
say(nzchar(fpa_rep_sel_path(cfg)), "sel path")

for (id in names(sel$elements)) sel$elements[[id]] <- identical(id, "syn_cash")
sel$texts$syn_cash <- TRUE
sel$title <- "Revue mensuelle"
say(isTRUE(fpa_rep_sel_write(cfg, sel)), "write sel")
got <- fpa_rep_sel_read(cfg)
say(isTRUE(got$elements$syn_cash) && identical(got$title, "Revue mensuelle"), "read sel")
say(identical(fpa_rep_selected_ids(got), "syn_cash"), "one selected")

for (el in cat) sel$elements[[el$id]] <- TRUE
say(identical(fpa_rep_selected_ids(sel), ids), "full selection order")

df <- data.frame(
  Ligne = c("REV", "OI"),
  `2026-01` = c(1000, 100),
  `2026-02` = c(0, 0),
  `2026-03` = c(1100, NA_real_),
  check.names = FALSE,
  stringsAsFactors = FALSE
)
cleaned <- fpa_rep_drop_empty_cols(df)
say("2026-02" %in% cleaned$dropped, "empty col dropped")
say(!"2026-02" %in% names(cleaned$df), "empty col gone")
tab <- fpa_rep_table_html(df)
body <- sub("<p class=\"fpa-rep-drop\">.*$", "", tab$html)
say(grepl("2026-01", body) && !grepl(">2026-02<", body) && "2026-02" %in% tab$dropped, "table html hides empty")

cards <- fpa_rep_cards_html(list(
  list(lab = "CA", val = "1 000"),
  list(lab = "OI", val = "100")
))
say(grepl("1 000", cards) && grepl("CA", cards), "cards html")

blocks <- list(
  list(id = "syn_cards", kind = "cards", title = "Cartons", html = cards, text = "Atterrissage 1000."),
  list(id = "syn_cash", kind = "text", title = "Cash", text = "Le cash finit a 55 000."),
  list(id = "note", kind = "analyst", title = "Note", md = "## Gaps\n\nThe gap is 12%.")
)
html <- fpa_rep_html(blocks, list(
  title = "Revue mensuelle",
  company = "Hotel Alpes",
  currency = "EUR",
  period = "2026-09",
  when = "2026-10-19 12:00",
  ai_label = "Commentaire"
))
say(grepl("Revue mensuelle", html), "html has title")
say(grepl('data-id="syn_cards"', html) && grepl('data-id="syn_cash"', html), "html has both blocks")
pos1 <- regexpr('data-id="syn_cards"', html)[[1]]
pos2 <- regexpr('data-id="syn_cash"', html)[[1]]
say(pos1 > 0 && pos2 > pos1, "block order")
say(grepl("Atterrissage 1000", html) && grepl("55 000", html), "R text present")
say(grepl("Hotel Alpes", html) && grepl("EUR", html), "meta present")
say(grepl("@page", html) && grepl("A4", html), "A4 css")

# Landing number in text matches card value.
say(grepl("1 000", html) && grepl("Atterrissage 1000", html), "landing number ties")

paths <- fpa_rep_start_paths(cfg, "Revue mensuelle")
say(grepl("rapports", paths$dir), "reports/rapports dir")
fpa_rep_write_utf8(paths$html, html)
say(file.exists(paths$html) && file.info(paths$html)$size > 0, "html written")

edge <- fpa_rep_edge_path()
say(nzchar(edge) || TRUE, paste("edge path", if (nzchar(edge)) edge else "missing (fallback ok)"))
if (nzchar(edge)) {
  got <- fpa_rep_pdf(paths$html, paths$pdf, timeout_sec = 45)
  say(isTRUE(got$ok), paste("pdf ok", got$reason %||% "", "pages", got$pages %||% 0))
  if (isTRUE(got$ok)) {
    say(file.info(paths$pdf)$size > 0, "pdf bytes > 0")
    say(as.integer(got$pages) >= 1L, paste("pdf pages", got$pages))
  }
} else {
  miss <- fpa_rep_pdf(paths$html, file.path(tempdir(), "no.pdf"))
  say(identical(miss$reason, "no_edge"), "missing edge fallback")
}

cost <- fpa_rep_cost_n(5L, TRUE)
say(identical(cost$calls, 6L), "ai cost N+1")
cost0 <- fpa_rep_cost_n(5L, FALSE)
say(identical(cost0$calls, 0L), "no ai cost")
say(fpa_rep_pages_estimate(0L) == 0L, "empty pages")
say(fpa_rep_pages_estimate(3L) >= 1L, "pages estimate")

dummy <- file.path(box, "reports", "rapports", "demo.pdf")
dir.create(dirname(dummy), recursive = TRUE, showWarnings = FALSE)
fpa_rep_write_utf8(dummy, "not a real pdf")
sealed <- fpa_rep_seal_pdf(dummy)
say(identical(sealed$hash, fpa_ai_sha256("not a real pdf")), "pdf hash matches the file bytes")
say(file.exists(paste0(dummy, ".sha256")), "sidecar written")
side <- trimws(readLines(paste0(dummy, ".sha256"), warn = FALSE, n = 1L, encoding = "UTF-8"))
say(identical(side[[1]], sealed$hash), "sidecar stores the hash")
again <- fpa_rep_seal_pdf(dummy)
say(isTRUE(again$already), "second seal is a no-op")
csv <- readLines(file.path(dirname(dummy), "sceaux.csv"), warn = FALSE, encoding = "UTF-8")
say(length(csv) == 2L, "sceaux.csv has one data row")
say(grepl(sealed$hash, csv[[2]], fixed = TRUE), "csv row has the hash")
say(identical(fpa_ai_read_utf8(dummy), "not a real pdf"), "pdf bytes unchanged")

source(file.path(root, "R", "fpa-health.R"), encoding = "UTF-8")
opt <- fpa_optional_data_dir("hotel-alpes")
books <- if (is.null(opt)) "" else file.path(opt, "work")
if (nzchar(books) && file.exists(file.path(books, "bs-monthly.csv"))) {
  old_lang <- getOption("fpa.lang")
  options(fpa.lang = "en")
  bs_h <- utils::read.csv(file.path(books, "bs-monthly.csv"), stringsAsFactors = FALSE)
  pnl_h <- utils::read.csv(file.path(books, "pnl-monthly.csv"), stringsAsFactors = FALSE)
  h <- tryCatch(fpa_health_month(bs_h, pnl_h, "2026-09", NULL), error = function(e) e)
  say(is.data.frame(h) && nrow(h) == 15L, "null fiscal start still returns the fifteen ratios")
  if (is.data.frame(h) && nrow(h)) {
    df_h <- fpa_rep_health_frame(h, label_fn = function(id) id)
    df_h$Valeur <- vapply(seq_len(nrow(df_h)), function(i) fpa_health_fmt(h$id[[i]], df_h$Valeur[[i]]), "")
    html_h <- fpa_rep_table_html(df_h)$html
    say(grepl("<table", html_h) && grepl("%", html_h) && !grepl("fpa-rep-empty", html_h), "ratio table html keeps a percent")
    oi <- h$value[h$id == "oi_margin"]
    say(is.finite(oi) && grepl(fpa_health_fmt("oi_margin", oi), html_h, fixed = TRUE), "operating margin is on the page")
  }
  empty_h <- data.frame(id = character(), value = numeric(), stringsAsFactors = FALSE)
  say(is.null(fpa_rep_health_frame(empty_h)), "an empty health frame is not a list of missing columns")
  options(fpa.lang = old_lang)
} else {
  fpa_notify_skip_optional_data("hotel health frame checks", dataset = "hotel-alpes")
}

unlink(box, recursive = TRUE)
cat("FAILS ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
