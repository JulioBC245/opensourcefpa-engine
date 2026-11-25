# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Drivers tab: copy keys, glossary tooltips, row caps, no literal "%%" on screen.
# Run from the opensourcefpa-engine root: Rscript tests/check-driver-page.R

`%||%` <- function(a, b) if (is.null(a)) b else a
root <- if (file.exists("app.R")) "." else ".."
source(file.path(root, "tests", "helpers.R"), encoding = "UTF-8")
suppressPackageStartupMessages(library(shiny))
source(file.path(root, "R", "fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-driver-map.R"), encoding = "UTF-8")

exprs <- parse(file.path(root, "app.R"), encoding = "UTF-8")
for (e in exprs) {
  if (!is.call(e) || !identical(as.character(e[[1]]), "<-") || !is.name(e[[2]])) next
  rhs <- e[[3]]
  if (is.call(rhs) && (identical(rhs[[1]], as.name("function")) || identical(rhs[[1]], as.name("new.env")))) {
    eval(e, globalenv())
  }
}

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}
html <- function(x) as.character(htmltools::renderTags(x)$html)
count <- function(pattern, s) lengths(regmatches(s, gregexpr(pattern, s, fixed = TRUE)))

app_src <- paste(readLines(file.path(root, "app.R"), encoding = "UTF-8", warn = FALSE), collapse = "\n")
used <- unique(regmatches(app_src, gregexpr("fpa_txt\\(\"(drv_|readings_)[a-z0-9_]+\"", app_src))[[1]])
used <- sub("^fpa_txt\\(\"", "", sub("\"$", "", used))
missing <- setdiff(used, names(fpa_copy))
check(sprintf("every drv_/readings_ key used by app.R exists (%d keys)", length(used)), !length(missing))
if (length(missing)) cat("   missing:", paste(missing, collapse = ", "), "\n")

drv_keys <- grep("^(drv_|readings_)", names(fpa_copy), value = TRUE)
lit <- vapply(drv_keys, function(k) {
  fr <- fpa_copy[[k]][["fr"]]
  grepl("%%", fr, fixed = TRUE) && !grepl("%s", fr, fixed = TRUE)
}, logical(1))
check("no argument-free drv_ string carries a literal %%", !any(lit))
if (any(lit)) cat("   literal %%:", paste(drv_keys[lit], collapse = ", "), "\n")
check("'Aucune série ne passe 5 %%' is gone", !grepl("%%", fpa_txt("drv_bottom_none_pass"), fixed = TRUE))

gl_ok <- all(vapply(fpa_drv_glossary_keys, function(k) nzchar(fpa_txt(paste0("drv_term_", k))) && nzchar(fpa_txt(paste0("drv_tip_", k))), logical(1)))
check(sprintf("glossary: %d terms, each with a label and a tooltip", length(fpa_drv_glossary_keys)), gl_ok)
tips_end <- vapply(fpa_drv_glossary_keys, function(k) grepl("[.?]$", fpa_txt(paste0("drv_tip_", k))), logical(1))
check("glossary tooltips are full sentences ending with a full stop", all(tips_end))

term <- html(fpa_drv_term("rank", "Rang"))
check("a term renders a focusable span with data-tip", grepl("class=\"fpa-term\"", term) && grepl("tabindex=\"0\"", term) && grepl("data-tip=\"Le moteur", term))
gloss <- html(fpa_drv_glossary())
check("glossary fold lists every term", count("<dt>", gloss) == length(fpa_drv_glossary_keys))

mk <- function(n) lapply(seq_len(n), function(i) shiny::tags$tr(shiny::tags$td(i)))
c1 <- fpa_shiny_cap_rows(mk(1), 8L)
c8 <- fpa_shiny_cap_rows(mk(8), 8L)
c30 <- fpa_shiny_cap_rows(mk(30), 8L)
check("cap at 1 row: nothing hidden, no button", c1$n_hidden == 0L && is.null(c1$toggle) && length(c1$rows) == 1L)
check("cap at 8 rows: nothing hidden, no button", c8$n_hidden == 0L && is.null(c8$toggle))
h30 <- html(shiny::tagList(c30$rows))
check("cap at 30 rows: all 30 kept, 22 hidden", length(c30$rows) == 30L && c30$n_hidden == 22L && count("fpa-cap-hidden", h30) == 22L)
check("cap at 30 rows: button says 22 more", grepl("Afficher les 22 autres lignes", html(c30$toggle), fixed = TRUE))

opt <- fpa_optional_data_dir()
if (!is.null(opt) && file.exists(file.path(opt, "work", "drivers", "driver-grades.csv"))) {
  grades <- utils::read.csv(file.path(opt, "work", "drivers", "driver-grades.csv"), stringsAsFactors = FALSE)
  check(sprintf("hotel grades load (%d series)", nrow(grades)), nrow(grades) >= 1L)
  oi <- html(fpa_shiny_goal_table(grades, "", "oi"))
  ln <- html(fpa_shiny_goal_table(grades, "", "line"))
  check("result table: no literal %% on screen", !grepl("%%", oi, fixed = TRUE) && !grepl("%%", ln, fixed = TRUE))
  check("result table: method sits in a closed fold", grepl("<details class=\"fpa-drv-fold-lite\">", oi, fixed = TRUE))
  check("result table: headers carry tooltips (r, partial r, rank, lag, months)", count("class=\"fpa-term\"", oi) >= 6L)
  n_oi <- sum(as.character(grades$bottom_line) == "OI")
  check(sprintf("result table: one row per OI series (%d)", n_oi), count("<tr", oi) == n_oi + 1L)
  tests <- html(fpa_shiny_tests_table(grades, "", list()))
  check("tests tab: no raw 'Confirmé' header for saved views", grepl("Avis enregistré", tests, fixed = TRUE))
} else {
  fpa_notify_skip_optional_data("hotel driver page checks")
}

lead <- fpa_shiny_lag_label(-3L, -0.57, -3, 5, TRUE, TRUE)
check("lag label is accented and dash-free", grepl("corrélation", lead) && grepl("recommandation du moteur", lead) && !grepl("\u2014", lead))
check("no unaccented 'decalage' left in shift offers", !grepl("Aucun decalage|meme mois|Verdict a confirmer|Decalage a confirmer|a teste les", app_src))

hero <- html(fpa_shiny_driver_hero(NULL, NULL, 0L, 0L))
check("hero renders with no series (empty load)", grepl("Drivers : ce qui fait bouger vos chiffres", hero, fixed = TRUE))

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
