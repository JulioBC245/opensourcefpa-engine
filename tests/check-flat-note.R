# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# A flat scenario says so. A moving result does not claim to be stuck.
# Run from the opensourcefpa-engine root: Rscript tests/check-flat-note.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

src <- readLines("app.R", encoding = "UTF-8", warn = FALSE)
start <- grep("^fpa_shiny_flat_note <- function", src)[[1]]
end <- grep("^fpa_shiny_flat_ui <- function", src)[[1]]
eval(parse(text = src[start:(end - 1L)]))

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) {
    pass <<- pass + 1L
    cat("PASS", label, "\n")
  } else {
    fail <<- fail + 1L
    cat("FAIL", label, "\n")
  }
}

frame <- function(rev, oi) data.frame(periode = "2026-10", REV = rev, OI = oi, stringsAsFactors = FALSE)
view <- function(lo_rev, lo_oi, hi_rev, hi_oi) {
  list(open = "2026-10", low = frame(lo_rev, lo_oi), high = frame(hi_rev, hi_oi))
}

both <- fpa_shiny_flat_note(view(1000, -50, 1000, -50))
result <- fpa_shiny_flat_note(view(1000, -50, 1100, -50))
revenue <- fpa_shiny_flat_note(view(1000, -11204, 1000, -11095))
moving <- fpa_shiny_flat_note(view(1000, -50, 1100, -40))

check("both stuck", grepl("ni le chiffre d'affaires ni le résultat", both))
check("result stuck names the result", grepl("résultat d'exploitation", result) && !grepl("chiffre d'affaires", result))
check("revenue stuck keeps the result range", grepl("chiffre d'affaires", revenue) && grepl("-11 204", revenue) && grepl("-11 095", revenue))
check("a real spread stays quiet", !nzchar(moving))

cat("PASS", pass, "FAIL", fail, "\n")
quit(status = if (fail) 1 else 0)
