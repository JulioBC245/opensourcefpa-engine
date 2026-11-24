# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Commentary stays out when it is off, and the fingerprint follows the figures.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-report.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

cache <- fpa_rep_ai_cache_put(list(), "syn_cards", "abc", "old commentary")
ids <- "syn_cards"
fps <- list(syn_cards = "abc")
say(identical(fpa_rep_ai_map_for(FALSE, cache, ids, fps), list()), "commentary off leaves the map empty")
say(identical(fpa_rep_ai_map_for(TRUE, cache, ids, fps)$syn_cards, "old commentary"), "commentary on can reuse a matching cache")
say(identical(fpa_rep_ai_map_for(TRUE, cache, ids, list(syn_cards = "other"))$syn_cards, ""), "a different fingerprint misses the cache")

a <- fpa_rep_fp_hash("syn_cards", TRUE, "2026-09", "2026-01|100|10", "sans", "m", "openai", "fr")
b <- fpa_rep_fp_hash("syn_cards", TRUE, "2026-09", "2026-01|100|10", "sans", "m", "openai", "fr")
c <- fpa_rep_fp_hash("syn_cards", TRUE, "2026-09", "2026-01|250|10", "sans", "m", "openai", "fr")
d <- fpa_rep_fp_hash("syn_cards", TRUE, "2026-09", "2026-01|100|10", "avec", "m", "openai", "en")
say(identical(a, b), "the same inputs keep the same fingerprint")
say(!identical(a, c), "a changed figure changes the fingerprint")
say(!identical(a, d), "mode and language change the fingerprint")

pnl <- data.frame(periode = c("2026-01", "2027-01"), REV = c(100, 900), NI = c(10, 90))
say(grepl("2026-01", fpa_rep_content_sig(pnl, "2026-01"), fixed = TRUE), "content signature uses the fiscal months")
say(!grepl("2027-01", fpa_rep_content_sig(pnl, "2026-01"), fixed = TRUE), "a later year stays out of the signature")

cat("FAILS", fail, "\n")
quit(status = if (fail) 1 else 0)
