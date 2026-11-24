# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Op/Fin optional contract: absent => empty; present => tags resolve.
source("R/fpa-paths.R", encoding = "UTF-8")
source("R/fpa-config.R", encoding = "UTF-8")
source("R/fpa-op-fin.R", encoding = "UTF-8")

pass <- 0L
check <- function(ok, msg) {
  if (!isTRUE(ok)) stop(msg, call. = FALSE)
  pass <<- pass + 1L
}

# Absent / empty
check(identical(fpa_line_nature(list(), "pnl"), character(0)), "empty config")
check(!fpa_line_nature_has(list(), "pnl"), "has false when empty")
check(identical(fpa_line_nature(list(line_nature = list()), "pnl"), character(0)), "empty block")
check(identical(fpa_line_nature(list(line_nature = list(pnl = list())), "bs"), character(0)), "wrong stmt")

cfg <- list(line_nature = list(
  pnl = list(REV = "op", FIN = "financing", TAX = "other", BOGUS = "nope"),
  bs = list(CASH = "fin", AR = "operating")
))
m <- fpa_line_nature(cfg, "pnl")
check(identical(unname(m[["REV"]]), "op"), "REV op")
check(identical(unname(m[["FIN"]]), "fin"), "FIN alias")
check(identical(unname(m[["TAX"]]), "other"), "TAX other")
check(!"BOGUS" %in% names(m), "bogus dropped")
check(fpa_line_nature_has(cfg, "pnl"), "has pnl")
check(fpa_nature_keep("REV", m, "all"), "keep all")
check(fpa_nature_keep("REV", m, "op_fin"), "keep op")
check(fpa_nature_keep("TAX", m, "op_fin") == FALSE, "drop other under filter")
check(identical(fpa_nature_norm("Exploitation"), "op"), "fr alias")

# Live hotel config if present
hotel <- "../fpa-local-test/fpa-config.json"
if (file.exists(hotel)) {
  h <- fpa_load_config(hotel)
  check(fpa_line_nature_has(h, "pnl"), "hotel pnl has nature")
  hm <- fpa_line_nature(h, "pnl")
  check(identical(unname(hm[["FIN"]]), "fin"), "hotel FIN")
  check(identical(unname(hm[["REV"]]), "op"), "hotel REV")
}

cat("PASS", pass, "\n")
