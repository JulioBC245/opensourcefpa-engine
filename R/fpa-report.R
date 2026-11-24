# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Composed management report: A4 HTML + Edge print-to-pdf.
# Selection lives in work/report-composer.json. Never published.

fpa_rep_dir <- function(config_path) {
  if (!nzchar(config_path %||% "")) return("")
  file.path(dirname(config_path), "reports", "rapports")
}

fpa_rep_sel_path <- function(config_path) {
  if (!nzchar(config_path %||% "")) return("")
  file.path(dirname(config_path), "work", "report-composer.json")
}

fpa_rep_cache_path <- function(config_path) {
  if (!nzchar(config_path %||% "")) return("")
  file.path(dirname(config_path), "work", "report-ai-cache.json")
}

fpa_rep_catalog <- function() {
  # kind: chart | table | text | analyst | cards
  # has_text: TRUE means a Texte sub-tick is offered (on by default).
  list(
    list(id = "syn_cards", group = "synthese", kind = "cards", has_text = FALSE, label_key = "rep_el_syn_cards"),
    list(id = "syn_cash", group = "synthese", kind = "chart", has_text = TRUE, label_key = "rep_el_syn_cash"),
    list(id = "pnl_cone", group = "pnl", kind = "chart", has_text = TRUE, label_key = "rep_el_pnl_cone"),
    list(id = "pnl_wf", group = "pnl", kind = "chart", has_text = TRUE, label_key = "rep_el_pnl_wf"),
    list(id = "pnl_table", group = "pnl", kind = "table", has_text = TRUE, label_key = "rep_el_pnl_table"),
    list(id = "cash_bridge", group = "cash", kind = "chart", has_text = TRUE, label_key = "rep_el_cash_bridge"),
    list(id = "cash_table", group = "cash", kind = "table", has_text = TRUE, label_key = "rep_el_cash_table"),
    list(id = "bs_table", group = "bs", kind = "table", has_text = TRUE, label_key = "rep_el_bs_table"),
    list(id = "var_main", group = "var", kind = "chart", has_text = TRUE, label_key = "rep_el_var_main"),
    list(id = "var_bridge", group = "var", kind = "chart", has_text = TRUE, label_key = "rep_el_var_bridge"),
    list(id = "var_pv", group = "var", kind = "chart", has_text = TRUE, label_key = "rep_el_var_pv"),
    list(id = "var_table", group = "var", kind = "table", has_text = TRUE, label_key = "rep_el_var_table"),
    list(id = "health_table", group = "health", kind = "table", has_text = TRUE, label_key = "rep_el_health_table"),
    list(id = "ai_note", group = "analyst", kind = "analyst", has_text = FALSE, label_key = "rep_el_ai_note")
  )
}

fpa_rep_groups <- function() {
  list(
    list(id = "synthese", label_key = "rep_grp_synthese"),
    list(id = "pnl", label_key = "rep_grp_pnl"),
    list(id = "cash", label_key = "rep_grp_cash"),
    list(id = "bs", label_key = "rep_grp_bs"),
    list(id = "var", label_key = "rep_grp_var"),
    list(id = "health", label_key = "rep_grp_health"),
    list(id = "analyst", label_key = "rep_grp_analyst")
  )
}

fpa_rep_sel_default <- function() {
  cat <- fpa_rep_catalog()
  ids <- vapply(cat, function(x) x$id, "")
  on <- c("syn_cards", "syn_cash", "pnl_wf", "var_bridge", "ai_note")
  els <- setNames(as.list(ids %in% on), ids)
  txt <- setNames(
    as.list(rep(TRUE, sum(vapply(cat, function(x) isTRUE(x$has_text), logical(1))))),
    vapply(Filter(function(x) isTRUE(x$has_text), cat), function(x) x$id, "")
  )
  list(
    title = "",
    elements = els,
    texts = txt,
    ai_comment = FALSE,
    audit = FALSE,
    last_html = "",
    last_pdf = ""
  )
}

fpa_rep_sel_read <- function(config_path) {
  out <- fpa_rep_sel_default()
  path <- fpa_rep_sel_path(config_path)
  if (!nzchar(path) || !file.exists(path)) return(out)
  got <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(got)) return(out)
  if (nzchar(got$title %||% "")) out$title <- as.character(got$title)
  if (is.list(got$elements)) {
    for (nm in names(got$elements)) {
      if (nm %in% names(out$elements)) out$elements[[nm]] <- isTRUE(got$elements[[nm]])
    }
  }
  if (is.list(got$texts)) {
    for (nm in names(got$texts)) {
      if (nm %in% names(out$texts)) out$texts[[nm]] <- isTRUE(got$texts[[nm]])
    }
  }
  out$ai_comment <- isTRUE(got$ai_comment)
  out$audit <- isTRUE(got$audit)
  out$last_html <- as.character(got$last_html %||% "")
  out$last_pdf <- as.character(got$last_pdf %||% "")
  out
}

fpa_rep_sel_write <- function(config_path, sel) {
  path <- fpa_rep_sel_path(config_path)
  if (!nzchar(path)) return(FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  body <- list(
    title = as.character(sel$title %||% ""),
    elements = sel$elements %||% list(),
    texts = sel$texts %||% list(),
    ai_comment = isTRUE(sel$ai_comment),
    audit = isTRUE(sel$audit),
    last_html = as.character(sel$last_html %||% ""),
    last_pdf = as.character(sel$last_pdf %||% "")
  )
  ok <- tryCatch({
    jsonlite::write_json(body, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    TRUE
  }, error = function(e) FALSE)
  invisible(isTRUE(ok))
}

fpa_rep_selected_ids <- function(sel) {
  els <- sel$elements %||% list()
  ids <- names(els)[vapply(els, isTRUE, logical(1))]
  order <- vapply(fpa_rep_catalog(), function(x) x$id, "")
  intersect(order, ids)
}

fpa_rep_slug <- function(title) {
  x <- tolower(gsub("[^a-zA-Z0-9]+", "-", as.character(title %||% "rapport")))
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) x <- "rapport"
  substr(x, 1L, 40L)
}

fpa_rep_write_utf8 <- function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), con)
  invisible(TRUE)
}

#' SHA-256 of the file bytes. Empty when the file is missing or the hash fails.
fpa_rep_file_sha256 <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path)) return("")
  full <- normalizePath(path, winslash = "\\", mustWork = TRUE)
  cmd <- sprintf("(Get-FileHash -Algorithm SHA256 -LiteralPath '%s').Hash", gsub("'", "''", full))
  out <- tryCatch(system2("powershell", c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = FALSE), error = function(e) "")
  hit <- regmatches(paste(out, collapse = ""), regexec("[0-9a-fA-F]{64}", paste(out, collapse = "")))[[1]]
  if (length(hit)) tolower(hit[[1]]) else ""
}

#' TRUE when the sidecar hash still matches the PDF bytes.
fpa_rep_pdf_sealed <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path)) return(FALSE)
  side <- paste0(path, ".sha256")
  if (!file.exists(side)) return(FALSE)
  old <- trimws(readLines(side, warn = FALSE, n = 1L, encoding = "UTF-8"))
  hash <- fpa_rep_file_sha256(path)
  nzchar(hash) && identical(tolower(old[[1]]), hash)
}

#' Freeze a PDF. Writes <pdf>.sha256 and one row in reports/rapports/sceaux.csv.
#' A second call on the same bytes is a no-op. The PDF itself is not modified.
fpa_rep_seal_pdf <- function(path) {
  if (!nzchar(path %||% "") || !file.exists(path)) stop("PDF not found.", call. = FALSE)
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  hash <- fpa_rep_file_sha256(path)
  if (!nzchar(hash)) stop("Hash failed.", call. = FALSE)
  side <- paste0(path, ".sha256")
  if (file.exists(side)) {
    old <- trimws(readLines(side, warn = FALSE, n = 1L, encoding = "UTF-8"))
    if (identical(tolower(old[[1]]), hash)) {
      return(invisible(list(path = path, hash = hash, already = TRUE)))
    }
  }
  fpa_rep_write_utf8(side, paste0(hash, "\n"))
  csv <- file.path(dirname(path), "sceaux.csv")
  row <- paste(basename(path), format(Sys.time(), "%Y-%m-%d %H:%M"), hash, sep = ",")
  if (!file.exists(csv)) {
    fpa_rep_write_utf8(csv, paste0("file,time,hash\n", row, "\n"))
  } else {
    con <- file(csv, "ab")
    on.exit(close(con), add = TRUE)
    writeBin(charToRaw(paste0(row, "\n")), con)
  }
  invisible(list(path = path, hash = hash, already = FALSE))
}

fpa_rep_plotly_js <- function() {
  path <- system.file("htmlwidgets/lib/plotlyjs/plotly-latest.min.js", package = "plotly")
  if (!nzchar(path) || !file.exists(path)) return("")
  raw <- readBin(path, "raw", n = file.info(path)$size)
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  txt
}

fpa_rep_fig_blank <- function(fig) {
  if (is.null(fig)) return(TRUE)
  built <- tryCatch(plotly::plotly_build(fig), error = function(e) NULL)
  data <- if (is.null(built)) NULL else built[["x"]][["data"]]
  if (is.null(data) || !length(data)) return(TRUE)
  has <- FALSE
  for (i in seq_along(data)) {
    tr <- data[[i]]
    if (!is.list(tr)) next
    xv <- tr[["x"]]
    if (is.null(xv)) next
    xv <- unlist(xv, use.names = FALSE)
    xv <- xv[!is.na(xv)]
    if (length(xv) > 0L) has <- TRUE
  }
  !has
}

fpa_rep_fig_json <- function(fig, width = 660, height = 280) {
  if (is.null(fig)) return(NULL)
  built <- tryCatch(plotly::plotly_build(fig), error = function(e) NULL)
  if (is.null(built)) return(NULL)
  x <- built$x
  if (is.null(x$layout) || !is.list(x$layout)) x$layout <- list()
  x$layout$width <- width
  x$layout$height <- height
  x$layout$autosize <- FALSE
  x$layout$margin <- x$layout$margin %||% list(l = 48, r = 24, t = 36, b = 48)
  list(
    data = x$data,
    layout = x$layout,
    config = list(staticPlot = TRUE, displayModeBar = FALSE, responsive = FALSE)
  )
}

fpa_rep_one <- function(x, fallback = "") {
  x <- tryCatch(as.character(x), error = function(e) character(0))
  if (!length(x) || is.na(x[[1]]) || !nzchar(x[[1]])) return(fallback)
  x[[1]]
}

fpa_rep_esc <- function(x) {
  x <- fpa_rep_one(x, "")
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x
}

fpa_rep_fmt_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (length(x) != 1L || is.na(x)) return("")
  neg <- x < 0
  ax <- abs(x)
  if (abs(ax - round(ax)) < 1e-9) {
    s <- format(round(ax), big.mark = " ", scientific = FALSE, trim = TRUE)
  } else {
    s <- format(round(ax, 1), big.mark = " ", nsmall = 1, scientific = FALSE, trim = TRUE)
  }
  if (neg) paste0("-", s) else s
}

fpa_rep_drop_empty_cols <- function(df) {
  if (is.null(df) || !nrow(df) || !ncol(df)) {
    return(list(df = df, dropped = character(0)))
  }
  keep <- vapply(seq_len(ncol(df)), function(j) {
    col <- df[[j]]
    if (is.character(col) || is.factor(col)) return(TRUE)
    num <- suppressWarnings(as.numeric(col))
    if (all(is.na(num))) return(FALSE)
    any(abs(num[!is.na(num)]) > 1e-12)
  }, logical(1))
  # Always keep the first column (row label) when present.
  if (length(keep)) keep[[1]] <- TRUE
  dropped <- names(df)[!keep]
  list(df = df[, keep, drop = FALSE], dropped = dropped)
}

fpa_rep_table_html <- function(df, total_rows = character(0)) {
  cleaned <- fpa_rep_drop_empty_cols(df)
  df <- cleaned$df
  if (is.null(df) || !nrow(df) || !ncol(df)) {
    return(list(html = "<p class=\"fpa-rep-empty\">—</p>", dropped = cleaned$dropped))
  }
  th <- paste0("<th>", vapply(names(df), function(nm) fpa_rep_esc(nm), character(1)), "</th>", collapse = "")
  wide <- sum(grepl("^[0-9]{4}-[0-9]{2}", names(df))) > 7L
  table_cls <- if (wide) "fpa-rep-table fpa-rep-wide" else "fpa-rep-table"
  rows <- vapply(seq_len(nrow(df)), function(i) {
    label <- fpa_rep_one(df[[1]][[i]], "")
    is_tot <- label %in% total_rows
    cls <- if (isTRUE(is_tot)) " class=\"is-total\"" else if (i %% 2L == 0L) " class=\"is-zebra\"" else ""
    cells <- vapply(seq_len(ncol(df)), function(j) {
      raw <- df[[j]][[i]]
      val <- if (length(raw)) raw[[1]] else NA
      show <- fpa_rep_one(val)
      if (is.character(val) && grepl("%$| x$| d$| j$| EUR$", show)) {
        cls_n <- if (grepl("^-", show)) " class=\"is-neg num\"" else " class=\"num\""
        return(paste0("<td", cls_n, ">", fpa_rep_esc(show), "</td>"))
      }
      if (is.numeric(val) || (is.character(val) && grepl("^-?[0-9]", show))) {
        num <- suppressWarnings(as.numeric(gsub(" ", "", fpa_rep_one(val))))
        if (length(num) == 1L && !is.na(num)) {
          cls_n <- if (num < 0) " class=\"is-neg num\"" else " class=\"num\""
          return(paste0("<td", cls_n, ">", fpa_rep_esc(fpa_rep_fmt_num(num)), "</td>"))
        }
      }
      paste0("<td>", fpa_rep_esc(val), "</td>")
    }, character(1))
    paste0("<tr", cls, ">", paste0(cells, collapse = ""), "</tr>")
  }, character(1))
  note <- ""
  if (length(cleaned$dropped)) {
    note <- paste0(
      "<p class=\"fpa-rep-drop\">",
      fpa_rep_esc(paste(cleaned$dropped, collapse = ", ")),
      "</p>"
    )
  }
  html <- paste0(
    "<table class=\"", table_cls, "\"><thead><tr>", th, "</tr></thead><tbody>",
    paste0(rows, collapse = ""),
    "</tbody></table>", note
  )
  list(html = html, dropped = cleaned$dropped)
}

fpa_rep_md_html <- function(md) {
  md <- as.character(md %||% "")
  if (!nzchar(md)) return("")
  if (requireNamespace("commonmark", quietly = TRUE)) {
    return(commonmark::markdown_html(md, smart = TRUE))
  }
  paste0("<pre class=\"fpa-rep-md\">", fpa_rep_esc(md), "</pre>")
}

fpa_rep_css <- function() {
  paste(
    "@page { size: A4 portrait; margin: 14mm 14mm 16mm 14mm;",
    "  @bottom-center { content: counter(page); font-size: 9pt; color: #5c6b73; font-family: 'Segoe UI', Calibri, Arial, sans-serif; } }",
    "@page wide { size: A4 landscape; margin: 12mm;",
    "  @bottom-center { content: counter(page); font-size: 9pt; color: #5c6b73; } }",
    "html, body { margin: 0; padding: 0; background: #fff; color: #0f2f3a;",
    "  font-family: 'Segoe UI', Calibri, Arial, sans-serif; font-size: 11pt; line-height: 1.45; }",
    ".fpa-rep { width: 182mm; max-width: 182mm; margin: 0 auto; }",
    ".fpa-rep-head { border-bottom: 2px solid #1f4e5f; padding-bottom: 8px; margin-bottom: 16px; }",
    ".fpa-rep-head h1 { margin: 0 0 4px; font-size: 18pt; color: #0f2f3a; }",
    ".fpa-rep-meta { margin: 0; font-size: 9.5pt; color: #5c6b73; font-variant-numeric: tabular-nums; }",
    ".fpa-rep-block { break-inside: auto; page-break-inside: auto; margin: 0 0 18px; }",
    ".fpa-rep-block h2 { margin: 0 0 8px; font-size: 13pt; color: #1f4e5f; break-after: avoid; }",
    ".fpa-rep-fig { width: 100%; max-width: 100%; height: 280px; margin: 0 0 8px; overflow: hidden; }",
    ".fpa-rep-text { margin: 6px 0 0; color: #3d4c54; font-size: 10.5pt; }",
    ".fpa-rep-ai { margin: 10px 0 0; padding: 8px 10px 8px 12px; border-left: 3px solid #1f4e5f;",
    "  background: #f4f8f9; border-radius: 0 6px 6px 0; }",
    ".fpa-rep-ai-label { margin: 0 0 4px; font-size: 9pt; font-weight: 700; color: #1f4e5f;",
    "  text-transform: uppercase; letter-spacing: 0.04em; }",
    ".fpa-rep-ai p { margin: 0 0 6px; }",
    ".fpa-rep-ai p:last-child { margin: 0; }",
    ".fpa-rep-table { width: 100%; border-collapse: collapse; font-variant-numeric: tabular-nums; font-size: 9.5pt; }",
    ".fpa-rep-table th { text-align: left; background: #e6edef; color: #0f2f3a; padding: 5px 6px;",
    "  border-bottom: 1px solid #c5d0d4; position: sticky; top: 0; white-space: nowrap; }",
    ".fpa-rep-table td { padding: 4px 6px; border-bottom: 1px solid #e6edef; vertical-align: top; }",
    ".fpa-rep-table td.num { text-align: right; white-space: nowrap; }",
    ".fpa-rep-table td.is-neg { color: #8c3a3a; }",
    ".fpa-rep-table tr.is-zebra td { background: #fbfdfd; }",
    ".fpa-rep-table tr.is-total td { font-weight: 700; background: #e6f1f4; }",
    ".fpa-rep-drop { margin: 4px 0 0; font-size: 9pt; color: #5c6b73; }",
    ".fpa-rep-cards { display: grid; grid-template-columns: 1fr 1fr; gap: 8px; }",
    ".fpa-rep-card { border: 1px solid #d6e1e5; border-radius: 8px; padding: 8px 10px; background: #fbfdfd; }",
    ".fpa-rep-card .lab { font-size: 9pt; color: #5c6b73; margin: 0 0 2px; }",
    ".fpa-rep-card .val { font-size: 14pt; font-weight: 700; color: #0f2f3a; font-variant-numeric: tabular-nums; margin: 0; }",
    ".fpa-rep-card .note { margin: 4px 0 0; font-size: 9pt; color: #5c6b73; }",
    ".fpa-rep-wide { page: wide; }",
    ".fpa-rep-summary { margin: 0 0 18px; padding: 10px 12px; background: #f4f8f9; border-radius: 8px;",
    "  border: 1px solid #d6e1e5; break-inside: avoid; }",
    "@media print { .fpa-rep-fig { break-inside: avoid; } }",
    sep = "\n"
  )
}

fpa_rep_block_html <- function(block, ai_label = "Commentaire de l'analyste") {
  title <- fpa_rep_esc(block$title %||% "")
  body <- ""
  kind <- as.character(block$kind %||% "text")
  if (identical(kind, "chart") && !is.null(block$fig_json)) {
    uid <- paste0("fig_", gsub("[^a-zA-Z0-9]", "", block$id %||% "x"), "_", as.integer(runif(1, 1e5, 1e6)))
    payload <- jsonlite::toJSON(block$fig_json, auto_unbox = TRUE, null = "null", digits = NA)
    body <- paste0(
      "<div class=\"fpa-rep-fig\" id=\"", uid, "\"></div>",
      "<script>(function(){var el=document.getElementById('", uid, "');",
      "var p=", payload, ";",
      "if(window.Plotly&&el){Plotly.newPlot(el,p.data,p.layout,p.config);}",
      "})();</script>"
    )
  } else if (identical(kind, "table") || identical(kind, "cards")) {
    body <- as.character(block$html %||% "")
  } else if (identical(kind, "analyst")) {
    body <- fpa_rep_md_html(block$md %||% "")
  }
  text <- trimws(as.character(block$text %||% ""))
  text_html <- if (nzchar(text)) paste0("<p class=\"fpa-rep-text\">", fpa_rep_esc(text), "</p>") else ""
  ai <- trimws(as.character(block$ai %||% ""))
  ai_html <- if (nzchar(ai)) {
    paste0(
      "<div class=\"fpa-rep-ai\"><div class=\"fpa-rep-ai-label\">", fpa_rep_esc(ai_label),
      "</div>", fpa_rep_md_html(ai), "</div>"
    )
  } else {
    ""
  }
  paste0(
    "<section class=\"fpa-rep-block\" data-id=\"", fpa_rep_esc(block$id %||% ""), "\">",
    "<h2>", title, "</h2>", body, text_html, ai_html, "</section>"
  )
}

fpa_rep_html <- function(blocks, meta = list()) {
  title <- as.character(meta$title %||% "Rapport")
  company <- as.character(meta$company %||% "")
  currency <- as.character(meta$currency %||% "")
  period <- as.character(meta$period %||% "")
  when <- as.character(meta$when %||% format(Sys.time(), "%Y-%m-%d %H:%M"))
  lang <- if (identical(as.character(meta$lang %||% "fr"), "en")) "en" else "fr"
  ai_label <- as.character(meta$ai_label %||% "Commentaire de l'analyste")
  summary_ai <- trimws(as.character(meta$summary_ai %||% ""))
  plotly_js <- fpa_rep_plotly_js()
  has_chart <- any(vapply(blocks, function(b) identical(b$kind, "chart") && !is.null(b$fig_json), logical(1)))
  meta_line <- paste(
    Filter(nzchar, c(company, currency, period, when)),
    collapse = " · "
  )
  summary_html <- if (nzchar(summary_ai)) {
    paste0(
      "<div class=\"fpa-rep-summary\"><div class=\"fpa-rep-ai-label\">", fpa_rep_esc(ai_label),
      "</div>", fpa_rep_md_html(summary_ai), "</div>"
    )
  } else {
    ""
  }
  body_blocks <- paste0(vapply(blocks, function(b) fpa_rep_block_html(b, ai_label), ""), collapse = "\n")
  script_tag <- if (has_chart && nzchar(plotly_js)) {
    paste0("<script>", plotly_js, "</script>")
  } else {
    ""
  }
  html <- paste0(
    "<!DOCTYPE html><html lang=\"", lang, "\"><head><meta charset=\"utf-8\"/>",
    "<title>", fpa_rep_esc(title), "</title>",
    "<style>", fpa_rep_css(), "</style>",
    script_tag,
    "</head><body><div class=\"fpa-rep\">",
    "<header class=\"fpa-rep-head\"><h1>", fpa_rep_esc(title), "</h1>",
    "<p class=\"fpa-rep-meta\">", fpa_rep_esc(meta_line), "</p></header>",
    summary_html, body_blocks,
    "</div></body></html>"
  )
  if (grepl("%s", html, fixed = TRUE)) {
    message("report html still had a %s placeholder")
    html <- gsub("%s", "", html, fixed = TRUE)
  }
  html <- gsub("Survolez[^.]*\\.", "", html)
  gsub("Hover a (month|bar)[^.]*\\.", "", html)
}

fpa_rep_edge_path <- function() {
  cands <- c(
    "C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Microsoft/Edge/Application/msedge.exe",
    "C:/Program Files/Google/Chrome/Application/chrome.exe",
    "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe"
  )
  hit <- cands[file.exists(cands)]
  if (length(hit)) hit[[1]] else ""
}

fpa_rep_pdf_pages <- function(pdf_path) {
  if (!nzchar(pdf_path %||% "") || !file.exists(pdf_path)) return(0L)
  raw <- tryCatch(readBin(pdf_path, "raw", n = min(file.info(pdf_path)$size, 5e6)), error = function(e) raw(0))
  if (!length(raw)) return(0L)
  # Binary PDF: keep ASCII only so page markers stay readable.
  raw[raw == as.raw(0L) | raw > as.raw(127L)] <- as.raw(32L)
  txt <- rawToChar(raw)
  hits <- gregexpr("/Type /Page", txt, fixed = TRUE)[[1]]
  if (length(hits) == 1L && hits[[1]] < 0) return(0L)
  # Exclude the /Type /Pages tree node.
  pages_tree <- gregexpr("/Type /Pages", txt, fixed = TRUE)[[1]]
  n_tree <- if (length(pages_tree) == 1L && pages_tree[[1]] < 0) 0L else length(pages_tree)
  as.integer(max(0L, length(hits) - n_tree))
}

fpa_rep_pdf <- function(html_path, pdf_path, timeout_sec = 60) {
  edge <- fpa_rep_edge_path()
  if (!nzchar(edge)) {
    return(list(ok = FALSE, reason = "no_edge", path = "", pages = 0L))
  }
  if (!file.exists(html_path)) {
    return(list(ok = FALSE, reason = "no_html", path = "", pages = 0L))
  }
  dir.create(dirname(pdf_path), recursive = TRUE, showWarnings = FALSE)
  if (file.exists(pdf_path)) unlink(pdf_path)
  html_uri <- paste0("file:///", gsub("\\\\", "/", normalizePath(html_path, winslash = "/", mustWork = TRUE)))
  args <- c(
    "--headless=new",
    "--disable-gpu",
    "--no-pdf-header-footer",
    "--print-to-pdf-no-header",
    paste0("--virtual-time-budget=", as.integer(timeout_sec * 1000)),
    paste0("--print-to-pdf=", normalizePath(pdf_path, winslash = "\\", mustWork = FALSE)),
    html_uri
  )
  status <- tryCatch({
    if (requireNamespace("processx", quietly = TRUE)) {
      processx::run(edge, args, timeout = timeout_sec, error_on_status = FALSE)$status
    } else {
      system2(edge, args, stdout = FALSE, stderr = FALSE, timeout = timeout_sec)
    }
  }, error = function(e) -1L)
  ok <- file.exists(pdf_path) && isTRUE(file.info(pdf_path)$size > 0)
  list(
    ok = ok,
    reason = if (ok) "ok" else if (identical(as.integer(status), -1L)) "timeout" else "edge_fail",
    path = if (ok) pdf_path else "",
    pages = if (ok) fpa_rep_pdf_pages(pdf_path) else 0L,
    status = as.integer(status %||% -1L)
  )
}

fpa_rep_ai_cache_read <- function(config_path) {
  path <- fpa_rep_cache_path(config_path)
  if (!nzchar(path) || !file.exists(path)) return(list())
  got <- tryCatch(jsonlite::fromJSON(path, simplifyVector = FALSE), error = function(e) NULL)
  if (!is.list(got)) list() else got
}

fpa_rep_ai_cache_write <- function(config_path, cache) {
  path <- fpa_rep_cache_path(config_path)
  if (!nzchar(path)) return(FALSE)
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  tryCatch({
    jsonlite::write_json(cache, path, auto_unbox = TRUE, pretty = TRUE, null = "null")
    TRUE
  }, error = function(e) FALSE)
}

fpa_rep_ai_cache_get <- function(cache, id, fingerprint) {
  hit <- cache[[id]]
  if (!is.list(hit)) return("")
  if (!identical(as.character(hit$fp %||% ""), as.character(fingerprint))) return("")
  as.character(hit$text %||% "")
}

fpa_rep_ai_cache_put <- function(cache, id, fingerprint, text) {
  cache[[id]] <- list(fp = as.character(fingerprint), text = as.character(text %||% ""))
  cache
}

fpa_rep_fingerprint <- function(...) {
  parts <- unlist(lapply(list(...), function(x) {
    if (is.null(x)) return("")
    if (is.atomic(x)) return(paste(as.character(x), collapse = "|"))
    tryCatch(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"), error = function(e) "")
  }))
  paste(parts, collapse = "::")
}

#' Stable hash of the commentary inputs. A changed figure, language or model changes it.
fpa_rep_fp_hash <- function(...) {
  raw <- fpa_rep_fingerprint(...)
  if (exists("fpa_ai_sha256", mode = "function")) {
    hit <- fpa_ai_sha256(raw)
    if (nzchar(hit)) return(hit)
  }
  if (requireNamespace("digest", quietly = TRUE)) return(digest::digest(raw, algo = "sha256", serialize = FALSE))
  raw
}

#' Cached commentary is used only when the user asked for new commentary.
fpa_rep_ai_map_for <- function(want_ai, cache, ids, fps) {
  if (!isTRUE(want_ai)) return(list())
  out <- list()
  for (id in ids) out[[id]] <- fpa_rep_ai_cache_get(cache, id, fps[[id]] %||% "")
  out
}

fpa_rep_cost_n <- function(n_sections, want_ai) {
  n <- as.integer(n_sections)
  if (!isTRUE(want_ai) || n <= 0L) return(list(sections = n, calls = 0L))
  list(sections = n, calls = n + 1L)
}

fpa_rep_pages_estimate <- function(n_blocks) {
  # Rough A4 estimate: ~1.2 blocks per page after the header.
  n <- max(0L, as.integer(n_blocks))
  if (n <= 0L) return(0L)
  max(1L, as.integer(ceiling(1 + n / 1.2)))
}

fpa_rep_start_paths <- function(config_path, title) {
  dir <- fpa_rep_dir(config_path)
  if (!nzchar(dir)) stop("No project folder.", call. = FALSE)
  stamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  slug <- fpa_rep_slug(title)
  base <- file.path(dir, paste0(stamp, "-", slug))
  n <- 2L
  while (file.exists(paste0(base, ".pdf")) || file.exists(paste0(base, ".pdf.sha256")) || file.exists(paste0(base, ".html"))) {
    base <- file.path(dir, paste0(stamp, "-", slug, "-", n))
    n <- n + 1L
  }
  list(html = paste0(base, ".html"), pdf = paste0(base, ".pdf"), dir = dir)
}

fpa_rep_cards_html <- function(cards) {
  # cards: list of list(lab=, val=)
  if (!length(cards)) return("<p class=\"fpa-rep-empty\">—</p>")
  parts <- lapply(cards, function(c) {
    paste0(
      "<div class=\"fpa-rep-card\"><p class=\"lab\">", fpa_rep_esc(c$lab %||% ""),
      "</p><p class=\"val\">", fpa_rep_esc(c$val %||% ""), "</p>",
      if (nzchar(c$note %||% "")) paste0("<p class=\"note\">", fpa_rep_esc(c$note), "</p>") else "",
      "</div>"
    )
  })
  paste0("<div class=\"fpa-rep-cards\">", paste0(unlist(parts), collapse = ""), "</div>")
}

fpa_rep_pnl_frame <- function(pnl, line_name_fn = NULL, max_months = 12L, months = NULL, status = NULL) {
  if (is.null(pnl) || !nrow(pnl)) return(NULL)
  lines <- c("REV", "OTHER_OP_INC", "COGS", "GP", "PAYROLL", "OPEX_OTHER", "EBITDA", "D_A", "OI", "FIN", "EXCEP", "TAX", "NI")
  lines <- intersect(lines, names(pnl))
  keep <- vapply(lines, function(line) {
    v <- suppressWarnings(as.numeric(pnl[[line]]))
    any(is.finite(v) & abs(v) >= 0.5)
  }, logical(1))
  lines <- lines[keep]
  if (!length(lines)) return(NULL)
  all_m <- as.character(pnl$periode)
  if (length(months)) {
    months <- intersect(as.character(months), all_m)
  } else if (length(all_m) > max_months) {
    months <- all_m[(length(all_m) - max_months + 1L):length(all_m)]
  } else {
    months <- all_m
  }
  if (!length(months)) return(NULL)
  lab <- vapply(lines, function(line) {
    if (is.function(line_name_fn)) fpa_rep_one(line_name_fn(line), line) else line
  }, character(1))
  out <- data.frame(Ligne = lab, stringsAsFactors = FALSE, check.names = FALSE)
  for (m in months) {
    out[[m]] <- vapply(lines, function(line) {
      v <- suppressWarnings(as.numeric(pnl[[line]][match(m, all_m)]))
      if (!length(v) || is.na(v[[1]])) NA_real_ else v[[1]]
    }, numeric(1))
  }
  fpa_rep_tag_months(out, status)
}

fpa_rep_cash_frame <- function(cash, max_months = 12L, months = NULL, status = NULL) {
  if (is.null(cash) || !nrow(cash)) return(NULL)
  cols <- intersect(c("periode", "cash_open", "delta_cash", "cash_close", "wc_impact", "D_A", "debt_net"), names(cash))
  if (!length(cols)) return(NULL)
  df <- cash[, cols, drop = FALSE]
  if (length(months)) {
    df <- df[as.character(df$periode) %in% as.character(months), , drop = FALSE]
  } else if (nrow(df) > max_months) {
    df <- df[(nrow(df) - max_months + 1L):nrow(df), , drop = FALSE]
  }
  if (!nrow(df)) return(NULL)
  rename <- c(
    periode = "Période", cash_open = "Ouverture", delta_cash = "Variation",
    cash_close = "Clôture", wc_impact = "BFR", D_A = "Dotations", debt_net = "Dette nette"
  )
  names(df) <- ifelse(names(df) %in% names(rename), unname(rename[names(df)]), names(df))
  if (!is.null(status) && "Période" %in% names(df)) {
    keys <- names(status)
    df$Période <- vapply(as.character(df$Période), function(m) {
      hit <- match(m, keys)
      if (is.na(hit)) return(m)
      st <- status[[hit]]
      if (!nzchar(st)) return(m)
      paste(m, if (identical(st, "actual")) "R" else "P")
    }, "")
  }
  df
}

fpa_rep_bs_frame <- function(bs, max_months = 6L, months = NULL, status = NULL, line_name_fn = NULL) {
  if (is.null(bs) || !nrow(bs)) return(NULL)
  skip <- c("periode", "period", "month")
  num_cols <- names(bs)[vapply(bs, is.numeric, logical(1))]
  num_cols <- setdiff(num_cols, skip)
  if (!length(num_cols)) return(NULL)
  all_m <- as.character(bs$periode)
  if (length(months)) {
    months <- intersect(as.character(months), all_m)
  } else if (length(all_m) > max_months) {
    months <- all_m[(length(all_m) - max_months + 1L):length(all_m)]
  } else {
    months <- all_m
  }
  if (!length(months)) return(NULL)
  labs <- vapply(num_cols, function(c) {
    if (is.function(line_name_fn)) fpa_rep_one(line_name_fn(c), c) else c
  }, "")
  out <- data.frame(Poste = labs, stringsAsFactors = FALSE, check.names = FALSE)
  for (m in months) {
    i <- match(m, all_m)
    out[[m]] <- vapply(num_cols, function(c) suppressWarnings(as.numeric(bs[[c]][[i]])), numeric(1))
  }
  fpa_rep_tag_months(out, status)
}

fpa_rep_health_frame <- function(h, label_fn = NULL) {
  if (is.null(h)) return(NULL)
  if (is.data.frame(h)) {
    if (!nrow(h) || !all(c("id", "value") %in% names(h))) return(NULL)
    labs <- vapply(as.character(h$id), function(id) {
      if (is.function(label_fn)) fpa_rep_one(label_fn(id), id) else id
    }, character(1))
    vals <- suppressWarnings(as.numeric(h$value))
    if (length(vals) != length(labs)) vals <- rep(NA_real_, length(labs))
    return(data.frame(
      Ratio = labs,
      Valeur = vals,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ))
  }
  if (!is.list(h)) return(NULL)
  ids <- fpa_health_ids()
  labs <- vapply(ids, function(id) {
    if (is.function(label_fn)) fpa_rep_one(label_fn(id), id) else id
  }, character(1))
  vals <- vapply(ids, function(id) {
    x <- h[[id]]
    if (is.list(x)) x <- x$value %||% x$ratio %||% x[[1]]
    v <- suppressWarnings(as.numeric(x))
    if (!length(v) || is.na(v[[1]])) NA_real_ else v[[1]]
  }, numeric(1))
  data.frame(Ratio = labs, Valeur = vals, stringsAsFactors = FALSE, check.names = FALSE)
}

fpa_rep_var_frame <- function(sl) {
  if (is.null(sl) || !is.list(sl)) return(NULL)
  df <- sl$month %||% sl$by_month %||% sl$table %||% NULL
  if (is.null(df) || !is.data.frame(df) || !nrow(df)) return(NULL)
  df
}

fpa_rep_tag_months <- function(df, status) {
  if (is.null(df) || is.null(status) || !length(status)) return(df)
  nm <- names(df)
  keys <- names(status)
  for (i in seq_along(nm)) {
    hit <- match(substr(nm[[i]], 1L, 7L), keys)
    if (is.na(hit)) next
    st <- status[[hit]]
    if (!nzchar(st) || grepl(" [RP]$", nm[[i]])) next
    nm[[i]] <- paste(nm[[i]], if (identical(st, "actual")) "R" else "P")
  }
  names(df) <- nm
  df
}

#' Fiscal year of the last closed month, plus the months and statuses inside it.
fpa_rep_snapshot <- function(home, scenario = "", currency = "EUR", fresh = "") {
  empty <- list(
    year = format(Sys.Date(), "%Y"), cutoff = "", months = character(0),
    status = stats::setNames(character(0), character(0)), bs_months = character(0),
    scenario = as.character(scenario %||% ""), currency = as.character(currency %||% "EUR"),
    fresh = as.character(fresh %||% ""), horizon = character(0)
  )
  path <- if (nzchar(home %||% "")) file.path(home, "work", "period-status.csv") else ""
  if (!nzchar(path) || !file.exists(path)) return(empty)
  st <- tryCatch(utils::read.csv(path, stringsAsFactors = FALSE, fileEncoding = "UTF-8"), error = function(e) NULL)
  if (is.null(st) || !all(c("periode", "status") %in% names(st)) || !nrow(st)) return(empty)
  months_all <- as.character(st$periode)
  status <- fpa_period_status_norm(st$status)
  actuals <- months_all[status == "actual"]
  cutoff <- if (length(actuals)) max(actuals) else ""
  year <- if (nzchar(cutoff)) substr(cutoff, 1L, 4L) else substr(months_all[[1]], 1L, 4L)
  keep <- substr(months_all, 1L, 4L) == year
  fy <- months_all[keep]
  fy_status <- stats::setNames(status[keep], fy)
  bs_months <- unique(c(cutoff, paste0(year, "-12")))
  bs_months <- bs_months[nzchar(bs_months) & bs_months %in% months_all]
  list(
    year = year, cutoff = cutoff, months = fy, status = fy_status, bs_months = bs_months,
    scenario = as.character(scenario %||% ""), currency = as.character(currency %||% "EUR"),
    fresh = as.character(fresh %||% ""), horizon = months_all
  )
}

fpa_rep_presets <- function() {
  list(
    list(id = "month", label_key = "rep_pre_month", ids = c("syn_cards", "syn_cash", "pnl_wf", "var_bridge", "ai_note")),
    list(id = "cash", label_key = "rep_pre_cash", ids = c("syn_cash", "cash_bridge", "cash_table", "health_table")),
    list(id = "full", label_key = "rep_pre_full", ids = vapply(fpa_rep_catalog(), function(x) x$id, ""))
  )
}

fpa_rep_section_slice <- function(id) {
  id <- as.character(id %||% "")
  if (grepl("^pnl_", id)) return(c("scope", "pnl_months", "headline"))
  if (grepl("^cash_|^syn_cash", id)) return(c("scope", "cash"))
  if (grepl("^var_", id)) return(c("scope", "variance_top", "landing"))
  if (identical(id, "bs_table")) return(c("scope", "bs"))
  if (identical(id, "health_table")) return(c("scope", "health"))
  if (identical(id, "syn_cards")) return(c("scope", "headline", "landing"))
  if (identical(id, "ai_note")) return(character(0))
  c("scope")
}

#' The management note, without the workshop header. The audit appendix is optional.
fpa_rep_note_body <- function(md, audit = FALSE) {
  md <- as.character(md %||% "")
  if (!nzchar(md)) return("")
  if (isTRUE(audit)) return(md)
  lines <- strsplit(md, "\n", fixed = TRUE)[[1]]
  start <- grep("^## Note\\s*$", lines)
  if (!length(start)) {
    drop <- grepl("^(closed|modes|provider|model|started):", lines)
    return(paste(lines[!drop], collapse = "\n"))
  }
  start <- start[[1]]
  end <- grep("^## ", lines)
  end <- end[end > start]
  stop_at <- if (length(end)) end[[1]] - 1L else length(lines)
  paste(lines[start:stop_at], collapse = "\n")
}

fpa_rep_content_sig <- function(pnl, months) {
  if (is.null(pnl) || !nrow(pnl) || !"REV" %in% names(pnl)) return("")
  if (length(months)) pnl <- pnl[as.character(pnl$periode) %in% as.character(months), , drop = FALSE]
  if (!nrow(pnl)) return("")
  ni <- if ("NI" %in% names(pnl)) suppressWarnings(as.numeric(pnl$NI)) else rep(0, nrow(pnl))
  paste(as.character(pnl$periode), round(suppressWarnings(as.numeric(pnl$REV)), 2), round(ni, 2), collapse = "|")
}
