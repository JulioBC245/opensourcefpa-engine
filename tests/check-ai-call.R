# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Call errors: a closed port is offline, a 401 body is an auth error. The key is not printed.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-ai.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

key <- "sk-test-SECRET-call"
job <- fpa_ai_request_start(
  "http://127.0.0.1:9/v1/chat/completions",
  list(list(role = "user", content = "ping")),
  "gpt-4o-mini", key = key
)
hit <- NULL
for (i in 1:40) {
  hit <- fpa_ai_request_poll(job, timeout_sec = 20)
  if (!is.null(hit)) break
  Sys.sleep(0.25)
}
say(!is.null(hit) && identical(hit$kind, "offline"), paste("closed port is offline", if (is.null(hit)) "pending" else hit$kind))

auth <- fpa_ai_interpret(401, "{\"error\":{\"message\":\"bad\"}}", "openai")
say(identical(auth$kind, "auth"), "401 is an auth error")
say(!grepl("SECRET", paste(capture.output(print(auth)), collapse = " "), fixed = TRUE), "401 result has no key")

cat("FAILS ", fail, "\n", sep = "")
quit(status = if (fail) 1 else 0)
