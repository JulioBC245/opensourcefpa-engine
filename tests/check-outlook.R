# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# G3: 4-week cash outlook stays off the monthly model.
root <- "."
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(root)

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

as_of <- as.Date("2026-09-27")
weeks <- fpa_outlook_weeks(as_of, 4)
say(length(weeks) == 4L, "four weeks")
say(identical(weeks[[1]]$start, as_of) && identical(weeks[[4]]$end, as_of + 27L),
    paste("span", as.character(weeks[[1]]$start), as.character(weeks[[4]]$end)))

need <- fpa_outlook_roll(NA, list(), as_of)
say(identical(need$reason, "need_balance"), "missing balance")

only <- fpa_outlook_roll(10000, list(), as_of)
say(identical(only$reason, "balance_only") && identical(only$low, 10000), "balance alone")

same <- fpa_outlook_roll(1000, list(
  list(id = "a", kind = "in", label = "Client", amount = 500, date = "2026-09-27", uncertain = FALSE),
  list(id = "b", kind = "out", label = "Loyer", amount = 800, date = "2026-09-27", uncertain = FALSE)
), as_of)
say(abs(same$low - 200) < 1e-6 && identical(same$low_items, "Loyer"),
    paste("same-day payment first, low", same$low))
say(abs(same$weeks[[1]]$close - 700) < 1e-6, paste("week 1 close", same$weeks[[1]]$close))

items <- list(
  list(id = "c", kind = "in", label = "Clients", amount = 4000, date = "2026-10-02", uncertain = TRUE),
  list(id = "d", kind = "out", label = "Salaires", amount = 6000, date = "2026-10-06", uncertain = FALSE)
)
plain <- fpa_outlook_roll(10000, items, as_of, FALSE)
late <- fpa_outlook_roll(10000, items, as_of, TRUE)
say(abs(plain$low - 8000) < 1e-6 && abs(plain$weeks[[2]]$close - 8000) < 1e-6,
    paste("plain low", plain$low, "w2", plain$weeks[[2]]$close))
say(abs(late$low - 4000) < 1e-6 && identical(late$low_items, "Salaires"),
    paste("delayed uncertain receipt, low", late$low, "on", as.character(late$low_date)))
say(abs(late$weeks[[1]]$close - 10000) < 1e-6 && abs(late$weeks[[2]]$close - 8000) < 1e-6,
    paste("delay moves receipt to week 2", late$weeks[[1]]$close, late$weeks[[2]]$close))

edge <- fpa_outlook_roll(10000, list(
  list(id = "e", kind = "out", label = "Passe", amount = 10, date = "2026-09-26"),
  list(id = "f", kind = "in", label = "Tard", amount = 10, date = "2026-11-20")
), as_of)
say(edge$before == 1L && edge$beyond == 1L && identical(edge$reason, "balance_only"),
    paste("before/beyond", edge$before, edge$beyond, edge$reason))

dir.create(tmp <- tempfile("outlook"), showWarnings = FALSE)
cfg <- list(`.config_path` = file.path(tmp, "fpa-config.json"))
wrote <- fpa_outlook_write(cfg, list(
  as_of = "2026-09-27", opening = 10000,
  items = list(list(kind = "out", label = "Loyer", amount = 800, date = "2026-10-01"))
))
back <- fpa_outlook_read(cfg)
say(abs(back$opening - 10000) < 1e-6 && length(back$items) == 1L && identical(back$items[[1]]$label, "Loyer"),
    "roundtrip keeps the typed line")
say(!grepl("cash_close", paste(names(wrote), collapse = " ")), "outlook file is not the model cash")

draft <- fpa_journal_from_outlook(list(ok = FALSE), plain)
say(identical(draft$source_tab, "Trésorerie") && abs(draft$amount - 8000) < 1e-6 &&
    identical(draft$amount_label, fpa_txt("out_journal_amt")),
    paste("journal snapshot low", draft$amount))
bare <- fpa_journal_from_outlook(list(ok = FALSE), only)
say(abs(bare$amount - 10000) < 1e-6 && identical(bare$amount_label, fpa_txt("out_journal_amt_open")),
    "journal names a balance with no movement as the declared balance")
say(isTRUE(bare$snapshot$simple) && isTRUE(draft$snapshot$simple),
    "outlook records use the short journal form")
moved <- list(
  reason = "partial", as_of = as.Date("2026-10-20"), opening = 54000,
  low = 50500, low_date = as.Date("2026-10-20"), delay = FALSE, weeks = list(),
  opening_source = "modeled",
  lines = list(list(kind = "out", label = "TeamBuilding", amount = 3500, date = "2026-10-20"))
)
moved_draft <- fpa_journal_from_outlook(list(ok = FALSE), moved)
moved_bits <- fpa_outlook_journal_bits(moved_draft$snapshot)
say(any(grepl("Paiement", moved_bits)) && any(grepl("modélisée", moved_bits)) && !any(grepl("weeks", moved_bits)),
    "a movement names itself and the modeled start")
marks <- fpa_synth_declared_marks(list(
  list(source_metric = "outlook_4w", note = "facture", snapshot = list(as_of = "2026-10-15", opening = 50000)),
  list(source_metric = "outlook_4w", note = "lecture", snapshot = list(as_of = "2026-10-11", opening = 55000)),
  list(source_metric = "outlook_4w", note = "retard", snapshot = list(
    as_of = "2026-10-20", opening = 54000, opening_source = "modeled",
    lines = list(list(kind = "out", label = "TeamBuilding", amount = 3500, date = "2026-10-20"))
  )),
  list(source_metric = "other", note = "x", snapshot = list(as_of = "2026-10-01", opening = 1))
))
say(sum(marks$as_of == "2026-10-20") == 0L, "a modeled movement is not a declared balance")
moves <- fpa_synth_movement_marks(list(), list(
  opening = 54000,
  lines = list(list(kind = "out", label = "TeamBuilding", amount = 3500, date = "2026-10-20"))
))
say(nrow(moves) == 1L && abs(moves$amount - 50500) < 1 && identical(moves$kind, "out"),
    "a payment is its own point at the balance after the movement")
say(all(c("2026-10-15", "2026-10-11") %in% marks$as_of),
    "both declared dates are kept for the chart")

cat(if (fail) paste0("\n", fail, " CHECK(S) FAILED\n") else "\nALL CHECKS PASSED\n")
quit(save = "no", status = if (fail) 1L else 0L)
