# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
#' Numeric check on the cash cascade: chained mode must land on the closing balance,
#' reset mode must restart every month at zero, and label tiers must thin out on long spans.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
env <- new.env(parent = globalenv())
assign("%||%", `%||%`, envir = env)
for (ex in parse(file.path(root, "app.R"))) {
  if (!is.call(ex)) next
  if (!identical(as.character(ex[[1]]), "<-")) next
  if (is.call(ex[[3]])) {
    head <- as.character(ex[[3]][[1]])[[1]]
    if (!head %in% c("function", "list")) next
  }
  try(eval(ex, envir = env), silent = TRUE)
}

cash <- utils::read.csv(
  "../fpa-local-test/work/cash-bridge-monthly.csv",
  stringsAsFactors = FALSE
)
build <- get("fpa_shiny_bridge_build", envir = env)
tiers <- get("fpa_shiny_bridge_tiers", envir = env)

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

b <- build(cash, "series", NULL, "6", "2026-07", "chain", "poste")
last <- b$bars[nrow(b$bars), ]
say(identical(as.character(last$label), "Clôture"), "chained series ends on a closing bar")
say(abs(last$end - 74881.59) < 1, paste("closing bar equals cash_close:", round(last$end)))
say(abs(b$bars$end[[1]] - 51391) < 2, paste("opening bar equals cash_open:", round(b$bars$end[[1]])))
steps <- b$bars[b$bars$kind == "step", ]
say(
  all(abs(steps$base[-1] - steps$end[-nrow(steps)]) < 1e-6),
  "every chained step starts where the previous one ended"
)

r <- build(cash, "series", NULL, "6", "2026-07", "reset", "poste")
firsts <- vapply(unique(r$bars$month), function(m) r$bars$base[r$bars$month == m][[1]], numeric(1))
say(all(abs(firsts) < 1e-9), "reset series restarts every month at zero")

m <- build(cash, "series", NULL, "all", NULL, "chain", "mois")
say(nrow(m$bars) == length(m$periods) + 2L, paste("monthly grain draws one bar per month plus caps:", nrow(m$bars)))
say(abs(m$bars$end[[nrow(m$bars)]] - 74881.59) < 1, "monthly chain ends on the closing balance")
full <- build(cash, "series", NULL, "all", NULL, "chain", "poste")
say(nrow(full$bars) > length(full$periods), paste("40 months by line item are drawn:", nrow(full$bars), "bars"))
tp <- tiers(full)
say(!tp$values && isTRUE(tp$zoom), "a long line-item span drops labels and offers zoom")
say(tp$month_every > 1L, paste("month labels thinned every", tp$month_every))

t12 <- tiers(build(cash, "series", NULL, "12", "2026-01", "chain", "mois"))
say(t12$values, "12 monthly bars keep their value labels")
t12p <- tiers(build(cash, "series", NULL, "12", "2026-01", "reset", "poste"))
say(!t12p$values && t12p$month_every == 1L, paste("12 months by line item drop value labels:", t12p$bars, "bars"))
t6p <- tiers(build(cash, "series", NULL, "6", "2026-07", "chain", "poste"))
say(!t6p$steps_on_axis, "6 months label the month, not each bar")

one <- build(cash, "one", "2026-11", NULL, NULL, "reset", "poste")
say(nrow(one$bars) == 3L, "single month keeps three step bars")
say(all(one$bars$base[[1]] == 0), "single month starts at zero")
say(tiers(one)$values && tiers(one)$upright, "single month keeps upright labels")

cat(if (fail) paste0("\n", fail, " CHECK(S) FAILED\n") else "\nALL CHECKS PASSED\n")
