# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Management note built from analyst answers. Plain markdown on disk.

fpa_ai_sha256 <- function(text) {
  tmp <- tempfile("fpa-ai-hash-")
  writeBin(charToRaw(enc2utf8(text)), tmp)
  on.exit(unlink(tmp), add = TRUE)
  cmd <- sprintf("(Get-FileHash -Algorithm SHA256 -LiteralPath '%s').Hash", gsub("'", "''", tmp))
  out <- tryCatch(system2("powershell", c("-NoProfile", "-Command", cmd), stdout = TRUE, stderr = FALSE), error = function(e) "")
  hit <- regmatches(paste(out, collapse = ""), regexec("[0-9a-fA-F]{64}", paste(out, collapse = "")))[[1]]
  if (length(hit)) tolower(hit[[1]]) else ""
}

fpa_ai_report_dir <- function(config_path) {
  if (!nzchar(config_path %||% "")) return("")
  file.path(dirname(config_path), "reports", "analyste")
}

fpa_ai_slug <- function(title) {
  x <- tolower(gsub("[^a-zA-Z0-9]+", "-", as.character(title %||% "note")))
  x <- gsub("^-+|-+$", "", x)
  if (!nzchar(x)) x <- "note"
  substr(x, 1L, 40L)
}

fpa_ai_write_utf8 <- function(path, text) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  con <- file(path, "wb")
  on.exit(close(con), add = TRUE)
  writeBin(charToRaw(enc2utf8(text)), con)
  invisible(TRUE)
}

fpa_ai_read_utf8 <- function(path) {
  if (!file.exists(path)) return("")
  raw <- readBin(path, "raw", n = file.info(path)$size)
  if (length(raw) >= 3 && identical(as.integer(raw[1:3]), c(239L, 187L, 191L))) {
    raw <- raw[-seq_len(3)]
  }
  txt <- rawToChar(raw)
  Encoding(txt) <- "UTF-8"
  txt
}

fpa_ai_report_sealed <- function(text) {
  grepl("(?m)^sha256:[0-9a-f]{64}$", text, perl = TRUE)
}

fpa_ai_report_modes <- function(text) {
  hit <- unique(unlist(regmatches(text, gregexpr("(?m)^mode: (sans|avec)$", text, perl = TRUE))))
  unique(sub("^mode: ", "", hit))
}

fpa_ai_report_start <- function(config_path, title, closed = "", mode = "sans", provider = "", model = "") {
  dir <- fpa_ai_report_dir(config_path)
  if (!nzchar(dir)) stop("No project folder.", call. = FALSE)
  stamp <- format(Sys.time(), "%Y-%m-%d-%H%M%S")
  path <- file.path(dir, paste0(stamp, "-", fpa_ai_slug(title), ".md"))
  body <- paste(
    paste0("# ", title),
    "",
    paste("closed:", if (nzchar(closed)) closed else "n/a"),
    paste("modes:", fpa_ai_mode_norm(mode)),
    paste("provider:", provider),
    paste("model:", model),
    paste("started:", format(Sys.time(), "%Y-%m-%d %H:%M")),
    "",
    sep = "\n"
  )
  fpa_ai_write_utf8(path, body)
  settings <- fpa_ai_settings_read(config_path)
  settings$open_report <- path
  fpa_ai_settings_write(config_path, settings)
  path
}

fpa_ai_report_title <- function(text) {
  hit <- regexpr("(?m)^#\\s+\\S.*$", text %||% "", perl = TRUE)
  if (hit[[1]] < 0) return("")
  sub("^#\\s+", "", regmatches(text, hit)[[1]])
}

fpa_ai_report_answers <- function(text) {
  parts <- strsplit(text %||% "", "(?m)^## ", perl = TRUE)[[1]]
  if (length(parts) < 2L) return(character(0))
  vapply(parts[-1], function(part) {
    part <- sub("(?s)^.*?mode:[^\n]*\n+", "", part, perl = TRUE)
    part <- sub("(?s)\n+<details>.*$", "", part, perl = TRUE)
    trimws(part)
  }, "")
}

fpa_ai_report_contains <- function(path, answer) {
  ans <- trimws(answer %||% "")
  if (!nzchar(ans)) return(FALSE)
  stored <- fpa_ai_report_answers(fpa_ai_read_utf8(path))
  stored <- stored[nzchar(stored)]
  if (!length(stored)) return(FALSE)
  if (any(stored == ans)) return(TRUE)
  # A formatted note continues under later headings. The saved section is the start of that text.
  any(vapply(stored, function(s) startsWith(ans, s), logical(1)))
}

fpa_ai_report_append <- function(path, title, answer, sent, mode = "sans") {
  text <- fpa_ai_read_utf8(path)
  if (!nzchar(text)) stop("Report not found.", call. = FALSE)
  if (fpa_ai_report_sealed(text)) stop("Report is sealed.", call. = FALSE)
  if (any(fpa_ai_report_answers(text) == trimws(answer %||% ""))) return(invisible(NA_character_))
  mode <- fpa_ai_mode_norm(mode)
  section <- paste(
    "",
    paste0("## ", title),
    paste("time:", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste("mode:", mode),
    "",
    answer,
    "",
    "<details>",
    "<summary>What was sent</summary>",
    "",
    sent,
    "",
    "</details>",
    "",
    sep = "\n"
  )
  modes <- unique(c(fpa_ai_report_modes(text), mode))
  text <- sub("(?m)^modes:.*$", paste("modes:", paste(modes, collapse = ",")), text, perl = TRUE)
  fpa_ai_write_utf8(path, paste0(text, section))
  invisible(path)
}

fpa_ai_report_finalize_messages <- function(path, lang = "fr") {
  text <- fpa_ai_read_utf8(path)
  if (!nzchar(text) || fpa_ai_report_sealed(text)) stop("Report cannot be finalized.", call. = FALSE)
  recipe <- paste(
    "Rewrite this management note as one note.",
    "Keep every figure. Do not repeat a figure.",
    "Order: summary, gaps, landing, cash, close, decisions.",
    "Leave the original sections under a heading Appendix.",
    if (identical(fpa_ai_mode_norm(lang), "en") || identical(lang, "en")) "Write in English." else "Write in French.",
    if (!grepl("avec", text)) "The note has no euro amounts. Do not add any." else "",
    "\n\n", text
  )
  list(
    list(role = "system", content = "You edit an FP&A note. You do not invent figures."),
    list(role = "user", content = recipe)
  )
}

fpa_ai_report_seal <- function(path) {
  text <- fpa_ai_read_utf8(path)
  if (!nzchar(text)) stop("Report not found.", call. = FALSE)
  if (fpa_ai_report_sealed(text)) return(invisible(path))
  hash <- fpa_ai_sha256(text)
  if (!nzchar(hash)) stop("Hash failed.", call. = FALSE)
  tail <- paste(
    "",
    "---",
    paste("Scellé", format(Sys.time(), "%Y-%m-%d %H:%M")),
    paste0("sha256:", hash),
    "",
    sep = "\n"
  )
  fpa_ai_write_utf8(path, paste0(text, tail))
  invisible(path)
}

fpa_ai_report_list <- function(config_path) {
  dir <- fpa_ai_report_dir(config_path)
  if (!nzchar(dir) || !dir.exists(dir)) return(data.frame(path = character(), title = character(), sections = integer(), sealed = logical(), stringsAsFactors = FALSE))
  files <- list.files(dir, pattern = "\\.md$", full.names = TRUE)
  if (!length(files)) {
    return(data.frame(path = character(), title = character(), sections = integer(), sealed = logical(), stringsAsFactors = FALSE))
  }
  rows <- lapply(files, function(path) {
    text <- fpa_ai_read_utf8(path)
    title <- sub("^# ", "", strsplit(text, "\n", fixed = TRUE)[[1]][[1]])
    data.frame(
      path = path, title = title,
      sections = length(gregexpr("(?m)^## ", text, perl = TRUE)[[1]][1] %||% integer()) ,
      sealed = fpa_ai_report_sealed(text),
      stringsAsFactors = FALSE
    )
  })
  # section count: gregexpr returns -1 when none
  out <- do.call(rbind, rows)
  out$sections <- vapply(files, function(path) {
    text <- fpa_ai_read_utf8(path)
    hit <- gregexpr("(?m)^## ", text, perl = TRUE)[[1]]
    if (hit[[1]] < 0) 0L else length(hit)
  }, 0L)
  out[order(out$path), , drop = FALSE]
}
