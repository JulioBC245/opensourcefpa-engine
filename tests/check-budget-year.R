# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Budget year: open exercise, N-1 moves with N, local rewrite, post status.
# Run from the opensourcefpa-engine root: Rscript tests/check-budget-year.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

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

oct <- as.Date("2026-10-12")
check("December close in October is 2026", fpa_budget_open_year(12L, oct) == 2026L)
check("March close in October is 2027", fpa_budget_open_year(3L, oct) == 2027L)

choices <- fpa_budget_year_choices(2026L, 2025L)
check("open year is the first choice", identical(choices[[1]], 2026L))
check("saved 2025 stays in the menu", identical(choices, c(2026L, 2025L, 2027L, 2024L)))

shifted <- fpa_budget_retarget(
  list(Poste = "libelle", "Budget N" = "periode_montant", "Budget N-1" = "periode_montant"),
  list("Budget N" = "2025", "Budget N-1" = "2024"),
  2026L
)
check("calendar N moves to 2026", identical(shifted[["Budget N"]], "2026"))
check("calendar N-1 moves to 2025", identical(shifted[["Budget N-1"]], "2025"))

relative <- fpa_budget_retarget(
  list(Poste = "libelle", "Budget N" = "periode_montant", "Budget N-1" = "periode_montant"),
  list("Budget N" = "N", "Budget N-1" = "N-1"),
  2026L
)
check("relative N becomes 2026", identical(relative[["Budget N"]], "2026"))
check("relative N-1 becomes 2025", identical(relative[["Budget N-1"]], "2025"))

month_stay <- fpa_budget_retarget(
  list(Janvier = "periode_montant"),
  list(Janvier = "Janvier 2025"),
  2026L
)
check("a named month is not shifted", identical(month_stay[["Janvier"]], "Janvier 2025"))

pnl <- list(
  source_id = "pnl", file_role = "pnl_actuals",
  column_map = list(A = "libelle"), column_period = list(), exercice_n = 2025L
)
bud <- list(
  source_id = "bud", file_role = "budget", grain = "year",
  column_map = list(Poste = "libelle", "Budget N" = "periode_montant", "Budget N-1" = "periode_montant"),
  column_period = list("Budget N" = "2025", "Budget N-1" = "2024"),
  exercice_n = 2025L
)
sources <- list(pnl, bud)
patched <- fpa_budget_patch_sources(sources, 2026L, 12L, "", oct)
check("patch finds the budget", isTRUE(patched$ok))
check("budget source id", identical(patched$source_id, "bud"))
check("pnl source is untouched", identical(patched$sources[[1]], sources[[1]]))
got <- patched$sources[[2]]
check("stored year is 2026", identical(as.integer(got$exercice_n), 2026L))
check("published N is 2026", identical(got$column_period[["Budget N"]], "2026"))
years <- vapply(got$periods, function(p) as.integer(p$year), 0L)
check("compiled periods include 2026 and 2025", all(c(2026L, 2025L) %in% years))
again <- fpa_budget_patch_sources(patched$sources, 2026L, 12L, "bud", oct)
check("the same year does not count as a change", isTRUE(again$ok) && isTRUE(again$unchanged))
missing <- fpa_budget_patch_sources(list(list(file_role = "pnl_actuals")), 2026L, 12L)
check("no budget source", !isTRUE(missing$ok) && identical(missing$reason, "missing"))
empty <- fpa_budget_patch_sources(
  list(list(file_role = "budget", source_id = "bud", column_map = list(), column_period = list())),
  2026L, 12L
)
check("empty budget is not written", !isTRUE(empty$ok) && identical(empty$reason, "empty"))

hotel_shape <- list(
  company = list(fiscal_year_end_month = 12L),
  sources = list(list(
    source_id = "src-budget", source_name = "Budget", file_role = "budget",
    time_frame = "plan", grain = "year", exercice_n = 2025L,
    column_map = list("Montant (€)" = "periode_montant", "Poste comptable" = "libelle"),
    column_period = list("Montant (€)" = "2025"),
    periods = list(list(
      header = "Montant (€)", user_label = "2025", kind = "calendar",
      year = 2025L, month = 12L, period_end = "2025-12-31", grain = "year", offset = 0L
    ))
  ))
)
need <- fpa_budget_needs_year(hotel_shape$sources[[1]], 12L, oct)
check("2025 budget asks for a year", !is.null(need) && identical(need$declared, 2025L) && identical(need$open, 2026L))
check("menu offers the open year first", identical(need$choices[[1]], 2026L))
moved <- fpa_budget_patch_sources(hotel_shape$sources, 2026L, 12L, "src-budget", oct)
bar <- fpa_chain_coverage(list(company = hotel_shape$company, sources = moved$sources), today = oct)
check("the bar ends in 2026-12", identical(format(bar$end[[1]], "%Y-%m"), "2026-12"))
check("2026 is no longer marked late", identical(as.character(bar$state[[1]]), "à jour"))
check("the menu hides once 2026 is covered", is.null(fpa_budget_needs_year(moved$sources[[1]], 12L, oct)))

box <- file.path(tempdir(), paste0("fpa-budget-year-", as.integer(runif(1, 1, 1e9))))
dir.create(box, recursive = TRUE, showWarnings = FALSE)
secret <- "NOTAREALKEY000000000000000000000000000"
cfg_path <- file.path(box, "fpa-config.json")
disk <- hotel_shape
disk$config_version <- "1.0.0"
disk$engine_min_version <- "0.1.0"
disk$project <- list(id = "proj-test", key = secret, site = "http://127.0.0.1:9", sync_revision = 8L)
jsonlite::write_json(disk, cfg_path, auto_unbox = TRUE, pretty = TRUE, null = "null")
out <- capture.output(wrote <- fpa_budget_write_year(cfg_path, 2026L, "src-budget", oct))
check("write prints nothing", !any(grepl(secret, out, fixed = TRUE)) && !any(grepl("key", out, fixed = TRUE)))
back <- jsonlite::fromJSON(cfg_path, simplifyVector = FALSE)
check("file year is 2026", identical(as.integer(back$sources[[1]]$exercice_n), 2026L))
check("file column is 2026", identical(as.character(back$sources[[1]]$column_period[["Montant (€)"]]), "2026"))
check("file period end is 2026-12-31", identical(as.character(back$sources[[1]]$periods[[1]]$period_end), "2026-12-31"))
check("the key stays in the file", identical(as.character(back$project$key), secret))
check("returned value has no key", !grepl(secret, paste(capture.output(str(wrote)), collapse = "\n"), fixed = TRUE))
same <- capture.output(again_w <- fpa_budget_write_year(cfg_path, 2026L, "src-budget", oct))
check("second write is unchanged", isTRUE(again_w$unchanged) && !any(grepl(secret, same, fixed = TRUE)))

dir.create(file.path(box, "work"), showWarnings = FALSE)
jsonlite::write_json(list(mode = "local"), file.path(box, "work", "sync-mode.json"), auto_unbox = TRUE)
post_out <- capture.output(posted <- fpa_sync_post_budget_year(box, cfg_path, 2026L, "src-budget"))
check("local mode does not post", identical(posted$status, "local"))
check("post prints nothing", !any(grepl(secret, post_out, fixed = TRUE)))

check("200 keeps the revision", {
  got <- fpa_budget_post_read(200L, list(sync_revision = 9L, unchanged = FALSE, year = 2026L))
  isTRUE(got$ok) && identical(got$revision, 9L)
})
check("409 is the empty account copy", identical(fpa_budget_post_read(409L, list(error = "empty"))$status, "empty"))
check("any other code is a failed post", identical(fpa_budget_post_read(500L, NULL)$status, "fail"))
check("no response is a failed post", identical(fpa_budget_post_read(0L, NULL)$status, "fail"))
src <- paste(readLines("R/fpa-sync.R", encoding = "UTF-8", warn = FALSE), collapse = "\n")
check(
  "the post quotes the content type",
  grepl("shQuote(\"Content-Type: application/json\")", src, fixed = TRUE)
)

cat(sprintf("PASS %d FAIL %d\n", pass, fail))
if (fail > 0L) quit(status = 1)
