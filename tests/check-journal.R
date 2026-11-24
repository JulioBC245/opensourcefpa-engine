# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
#' Journal: round-trip, frozen snapshot, favorites, sort/filter, amount.
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(root)

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

tmp <- tempfile("fpa-journal")
dir.create(tmp)
paths <- list(work = tmp)

say(length(fpa_journal_read(paths)) == 0L, "empty journal on first read")

pack <- list(ok = TRUE, config = list(company = list(name = "Test Co")), finance = list(period_status = NULL), paths = paths)
draft <- fpa_journal_draft(
  pack, type = "observation", title = "CA écart", note = "à suivre",
  periode = "2026-09", source_tab = "Anomalies", source_metric = "driver/REV",
  amount = 19980, amount_label = "Écart",
  snapshot = list(gap = 19980, sentence = "CA écart 19 980")
)
got <- fpa_journal_add(paths, draft)
say(isTRUE(got$ok) && nzchar(got$id), paste("add entry", got$id))
ents <- fpa_journal_read(paths)
say(length(ents) == 1L, "one entry after write")
say(identical(ents[[1]]$snapshot$gap, 19980) || identical(as.numeric(ents[[1]]$snapshot$gap), 19980), "snapshot frozen gap")
say(identical(ents[[1]]$title, "CA écart"), "title kept")
say(nzchar(ents[[1]]$created_at), paste("auto stamp", ents[[1]]$created_at))

# Mutate draft numbers after save — disk must stay frozen
draft$snapshot$gap <- 1
draft$amount <- 1
ents2 <- fpa_journal_read(paths)
say(abs(as.numeric(ents2[[1]]$amount) - 19980) < 0.5, "disk amount unchanged after mutatin in-memory draft")
say(abs(as.numeric(ents2[[1]]$snapshot$gap) - 19980) < 0.5, "disk snapshot unchanged")

fav <- fpa_journal_set_favorite(paths, ents2[[1]]$id, TRUE)
say(isTRUE(fav$ok) && isTRUE(fav$entries[[1]]$favorite), "favorite set")

scn <- fpa_journal_from_scenario(
  pack,
  list(
    at = "2026-09-26 12:00:00", moves = "Achats -10 %", cash_close = 59852,
    cash_delta = 53, oi = 33577, oi_delta = 53, base_cash = 59799, base_oi = 33524,
    floor = 0, under_months = character(), deltas = list()
  )
)
say(identical(scn$type, "explored_scenario"), "scenario type")
say(abs(scn$amount - 53) < 0.5, paste("scenario amount", scn$amount))
scn2 <- fpa_journal_from_scenario(
  pack,
  list(
    at = "2026-09-30 12:00:00", moves = "Chiffre d'affaires +5 %", cash_close = 38000,
    cash_delta = 900, oi = 34000, oi_delta = 785, oi_fy = 34000, oi_last = -3000,
    base_oi_fy = 33215, base_oi_last = -3462, ye_month = "2026-12",
    fy_months = sprintf("2026-%02d", 1:12), base_cash = 37099, base_oi = 33215,
    floor = 0, under_months = character(), deltas = list(),
    objectif = list(metric = "oi_fy", target = 34000, ye_month = "2026-12", value = 34000)
  )
)
say(identical(scn2$snapshot$metric, "oi_fy") && scn2$snapshot$oi_fy == 34000 && scn2$snapshot$oi_last == -3000 &&
      identical(scn2$snapshot$ye_month, "2026-12") && length(scn2$snapshot$fy_months) == 12L,
    "scenario snapshot freezes both income meanings, the month, and the metric")
fpa_journal_add(paths, scn)
fpa_journal_add(paths, fpa_journal_draft(pack, type = "decision", title = "Go Oct", amount = 1000))

all_e <- fpa_journal_read(paths)
say(length(all_e) == 3L, paste("three entries", length(all_e)))
favs <- fpa_journal_list(all_e, "favorite", "newest")
say(length(favs) == 1L && isTRUE(favs[[1]]$favorite), "filter favorites")
by_amt <- fpa_journal_list(all_e, "all", "amount_desc")
amts <- vapply(by_amt, function(e) abs(suppressWarnings(as.numeric(e$amount))), numeric(1))
say(all(diff(amts) <= 0), paste("amount_desc", paste(round(amts), collapse = ",")))
obs <- fpa_journal_list(all_e, "observation", "newest")
say(length(obs) == 1L, "filter observation")

bad <- fpa_journal_normalize(list(type = "nope", title = "x"))
say(identical(bad$type, "observation"), "invalid type coerced")

# Withdraw keeps history but drops the live bank diamond.
bank <- fpa_journal_draft(
  pack, type = "observation", title = "Banque", note = "mauvais solde",
  periode = "2026-10-15", source_tab = "Trésorerie", source_metric = "outlook_4w",
  amount = 50000, amount_label = "Solde déclaré",
  snapshot = list(as_of = "2026-10-15", opening = 50000, opening_source = "declared", simple = TRUE)
)
bank_got <- fpa_journal_add(paths, bank)
say(isTRUE(bank_got$ok), "add bank observation")
ents_b <- fpa_journal_read(paths)
marks_before <- fpa_synth_declared_marks(ents_b)
say(!is.null(marks_before) && any(abs(marks_before$amount - 50000) < 0.5), "declared mark before withdraw")
wd <- fpa_journal_withdraw(paths, bank_got$id)
say(isTRUE(wd$ok) && identical(wd$entry$state, "withdrawn"), "withdraw sets state")
say(nzchar(wd$entry$withdrawn_at %||% ""), "withdrawn_at stamped")
ents_w <- fpa_journal_read(paths)
say(any(vapply(ents_w, function(e) identical(e$id, bank_got$id), logical(1))), "entry still on disk")
marks_after <- fpa_synth_declared_marks(ents_w)
say(is.null(marks_after) || !any(abs(marks_after$amount - 50000) < 0.5), "no declared mark after withdraw")
say(is.null(fpa_synth_latest_declared(ents_w)) ||
      !identical(as.character(fpa_synth_latest_declared(ents_w)$as_of), "2026-10-15"),
    "latest declared skips withdrawn")
due_w <- fpa_journal_due_items(ents_w)
say(!any(vapply(due_w, function(d) identical(d$id, bank_got$id), logical(1))), "due items skip withdrawn")
rest <- fpa_journal_restore_keep(paths, bank_got$id)
say(isTRUE(rest$ok) && identical(rest$entry$state, "open"), "restore keep")
marks_rest <- fpa_synth_declared_marks(fpa_journal_read(paths))
say(!is.null(marks_rest) && any(abs(marks_rest$amount - 50000) < 0.5), "declared mark after restore")

unlink(tmp, recursive = TRUE)
cat(if (fail) paste0("\n", fail, " CHECK(S) FAILED\n") else "\nALL CHECKS PASSED\n")
quit(save = "no", status = if (fail) 1L else 0L)
