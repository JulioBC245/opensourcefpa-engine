# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# The key stays on disk. DPAPI when Windows can seal it, plain JSON still reads.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-ai.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

box <- tempfile("fpa-key-")
dir.create(box)
path <- file.path(box, "ai-key.json")
options(fpa.ai.key_path = path)
secret <- "test-key-not-real-4821"

jsonlite::write_json(list(key = secret), path, auto_unbox = TRUE)
say(identical(fpa_ai_key_read(), secret), "a plain key still reads")

ok <- fpa_ai_key_write(secret)
raw <- paste(readLines(path, warn = FALSE), collapse = "")
say(isTRUE(ok) && file.exists(path), "a save writes a file")
sealed <- grepl("\"store\":\"dpapi\"", raw, fixed = TRUE) || grepl("\"store\": \"dpapi\"", raw, fixed = TRUE)
if (sealed) {
  say(!grepl(secret, raw, fixed = TRUE), "the sealed file does not hold the key in clear")
  say(identical(fpa_ai_key_read(), secret), "the sealed key reads back")
} else {
  say(identical(fpa_ai_key_read(), secret), "if sealing is unavailable the plain key still reads back")
}

fpa_ai_key_write("")
say(!file.exists(path), "an empty key removes the file")
say(identical(fpa_ai_key_read(), ""), "a missing file reads as no key")

options(fpa.ai.key_path = NULL)
cat("FAILS", fail, "\n")
quit(status = if (fail) 1 else 0)
