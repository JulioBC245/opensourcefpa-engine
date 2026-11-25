# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Drivers "Votre avis" table: one proof, three tests, one overall reading per series.
# Run from the opensourcefpa-engine root: Rscript tests/check-driver-work-table.R

`%||%` <- function(a, b) if (is.null(a)) b else a
root <- if (file.exists("app.R")) "." else ".."
source(file.path(root, "tests", "helpers.R"), encoding = "UTF-8")
suppressPackageStartupMessages(library(shiny))
source(file.path(root, "R", "fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-driver-map.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-driver-grade.R"), encoding = "UTF-8")

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

opt <- fpa_skip_if_no_optional_data("driver work table checks")
work <- file.path(opt, "work", "drivers")
grades <- utils::read.csv(file.path(work, "driver-grades.csv"), stringsAsFactors = FALSE)
formulas <- utils::read.csv(file.path(work, "formula-checks.csv"), stringsAsFactors = FALSE)
grades <- fpa_driver_state(grades, formulas, NULL)
check(sprintf("hotel grades and formulas load (%d series, %d formulas)", nrow(grades), nrow(formulas)),
      nrow(grades) == 8L && nrow(formulas) >= 1L)

row_of <- function(name) grades[grades$series_name == name, , drop = FALSE]
expect <- list(
  capacite          = list(st = c("pass", "na", "na", "na"),             kind = "proved", np = 0L, run = 0L),
  taux_utilisation  = list(st = c("pass", "expected", "pass", "pass"),   kind = "proved", np = 2L, run = 2L),
  tarif             = list(st = c("pass", "expected", "pass", "pass"),   kind = "proved", np = 2L, run = 2L),
  nuitees_vendues   = list(st = c("pass", "expected", "na", "pass"),     kind = "proved", np = 1L, run = 1L),
  effectif          = list(st = c("fail", "fail", "pass", "pass"),       kind = "decide", np = 2L, run = 3L),
  cout_moyen        = list(st = c("fail", "fail", "pass", "fail"),       kind = "decide", np = 1L, run = 3L),
  temperature_c     = list(st = c("na", "fail", "fail", "fail"),         kind = "none",   np = 0L, run = 3L),
  precipitations_mm = list(st = c("na", "fail", "fail", "fail"),         kind = "none",   np = 0L, run = 3L)
)
for (nm in names(expect)) {
  g <- row_of(nm)
  p <- fpa_drv_pillars(g, formulas)
  b <- fpa_drv_bilan(g, p)
  got <- c(p$formula$state, p$line$state, p$indep$state, p$result$state)
  ex <- expect[[nm]]
  check(sprintf("%s: proof and tests read %s", nm, paste(ex$st, collapse = "/")), identical(got, ex$st))
  check(sprintf("%s: overall %s, %d passed of %d measured", nm, ex$kind, ex$np, ex$run),
        identical(b$kind, ex$kind) && b$np == ex$np && b$run == ex$run)
}

eff <- fpa_drv_pillars(row_of("effectif"), formulas)
check("effectif: line cross explains the reverse direction", grepl("la ligne bouge avant la série", eff$line$tip, fixed = TRUE))
check("effectif: formula tip names the payroll formula and its 26,6 % gap",
      grepl("masse salariale = effectif", eff$formula$tip, fixed = TRUE) && grepl("26,6 %", eff$formula$tip, fixed = TRUE))
tar <- fpa_drv_pillars(row_of("tarif"), formulas)
check("tarif: formula tip says no gap on 37 months", grepl("37 mois clos", tar$formula$tip, fixed = TRUE) && grepl("sans écart", tar$formula$tip, fixed = TRUE))
check("tarif: result figure matches the engine r (0,69)", identical(tar$result$num, "r 0,69"))
tmp <- fpa_drv_pillars(row_of("temperature_c"), formulas)
check("température: result fail says chance does as well (rank 95,8 %)", grepl("95,8 %", tmp$result$tip, fixed = TRUE) && grepl("hasard fait aussi bien", tmp$result$tip, fixed = TRUE))

all_tips <- unlist(lapply(seq_len(nrow(grades)), function(i) {
  p <- fpa_drv_pillars(grades[i, , drop = FALSE], formulas)
  c(vapply(p, function(x) x$tip, ""), fpa_drv_bilan(grades[i, , drop = FALSE], p)$tip)
}))
check("every tooltip is a full sentence with no literal %% or %s",
      all(nzchar(all_tips)) && !any(grepl("%%|%s", all_tips)) && all(grepl("[.]$", all_tips)))

full <- html(fpa_shiny_driver_work(grades, list(), "src-ops_other__effectif", formulas))
check("table: two header rows with the proof band and the tests band",
      grepl("Preuve comptable", full, fixed = TRUE) && grepl("Trois tests sur l&#39;historique|Trois tests sur l'historique", full))
check("table: 8 body rows, one marked as open", count("class=\"fpa-w-check\"", full) == 9L && count("class=\"is-on\"", full) == 1L)
check("table: 32 icons with a tooltip (4 per series)", count("class=\"fpa-pil is-", full) == 32L)
check("table: no raw engine words on screen", !grepl("refused_reverse|part_whole|Reject|Optional|statistically", full))
check("table: legend shows the four shapes", count("fpa-w-leg\"", full) == 4L)
check("unsaved formula gaps stay À trancher", count("fpa-bil is-decide", full) == 2L)

called <- list(
  list(series_id = as.character(row_of("effectif")$series_id[[1]]), verdict = "Keep"),
  list(series_id = as.character(row_of("cout_moyen")$series_id[[1]]), verdict = "Reject")
)
settled <- html(fpa_shiny_driver_work(fpa_shiny_apply_confirmations(grades, called), called, "", formulas))
check("a saved call replaces À trancher with the verdict",
      count("fpa-bil is-decide", settled) == 0L &&
        grepl("fpa-bil is-called", settled, fixed = TRUE) &&
        grepl(">Garder<", settled, fixed = TRUE) &&
        grepl("fpa-bil is-parked", settled, fixed = TRUE) &&
        grepl(">Écarter<", settled, fixed = TRUE))
check("saved-call tooltip names the verdict and says it is recorded",
      grepl("Vous avez tranché : Garder.", settled, fixed = TRUE) &&
        grepl("cet avis est enregistré.", settled, fixed = TRUE))

lead_open <- html(fpa_shiny_driver_lead(grades, formulas))
check("before any call, the failed formula is still an open choice",
      grepl("Il vous reste", lead_open, fixed = TRUE) && grepl("s'écarte des comptes", lead_open, fixed = TRUE))
lead_pay <- html(fpa_shiny_driver_lead(fpa_shiny_apply_confirmations(grades, called), formulas))
check("calls on both payroll series drop that formula from the open list",
      !grepl("s'écarte des comptes", lead_pay, fixed = TRUE) && grepl("2 séries", lead_pay, fixed = TRUE))
every <- lapply(seq_len(nrow(grades)), function(i) {
  list(series_id = as.character(grades$series_id[[i]]), verdict = "Keep")
})
lead_all <- html(fpa_shiny_driver_lead(fpa_shiny_apply_confirmations(grades, every), formulas))
check("when every series has a call, nothing is left to decide",
      grepl("Il ne reste rien à trancher", lead_all, fixed = TRUE) &&
        !grepl("Il vous reste", lead_all, fixed = TRUE))

one <- html(fpa_shiny_driver_work(grades[1, , drop = FALSE], list(), "", formulas))
check("table: renders with a single series", count("class=\"fpa-pil is-", one) == 4L)
empty <- html(fpa_shiny_driver_work(grades[0, , drop = FALSE], list(), "", formulas))
check("table: empty load shows a sentence, not an error", grepl("Pas encore de séries", empty, fixed = TRUE))
nof <- html(fpa_shiny_driver_work(fpa_driver_state(utils::read.csv(file.path(work, "driver-grades.csv"), stringsAsFactors = FALSE), NULL, NULL), list(), "", NULL))
check("table: renders with no declared formula (every proof reads not measured)", count("fpa-pil is-na", nof) >= 8L)

cat(sprintf("\n%d passed, %d failed\n", pass, fail))
if (fail > 0L) quit(status = 1L)
