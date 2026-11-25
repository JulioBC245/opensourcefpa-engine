# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Clôture tab: steps, version gap, register, lock drift and budget must tie to the work files.
# Run from the opensourcefpa-engine root: Rscript tests/check-close-page.R
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()
suppressPackageStartupMessages(library(shiny))

exprs <- parse("app.R", encoding = "UTF-8")
for (e in exprs) {
  if (!is.call(e) || !identical(as.character(e[[1]]), "<-") || !is.name(e[[2]])) next
  nm <- as.character(e[[2]])
  rhs <- e[[3]]
  is_fun <- is.call(rhs) && identical(rhs[[1]], as.name("function"))
  if (is_fun || grepl("^fpa_var_|^\\.fpa_series_meta$", nm)) eval(e, globalenv())
}
FPA_HAS_PLOTLY <- requireNamespace("plotly", quietly = TRUE)

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}
html <- function(x) as.character(htmltools::renderTags(x)$html)
visible <- function(s) gsub("<[^>]+>", " ", s)
count <- function(pattern, s) lengths(regmatches(s, gregexpr(pattern, s, fixed = TRUE)))
near <- function(a, b, tol = 1) is.finite(a) && is.finite(b) && abs(a - b) <= tol
raw_ids <- "\\b(REV|OTHER_OP_INC|COGS|PAYROLL|OPEX_OTHER|D_A|FIN|EXCEP|TAX|line_id|periode|[0-9]{8}-[0-9]{6}|20[0-9]{2}-[0-9]{2}(?!-))\\b"
no_raw <- function(s) !grepl(raw_ids, visible(s), perl = TRUE)

hotel <- fpa_optional_data_config()
if (is.null(hotel)) fpa_skip_if_no_optional_data("close page checks")
p <- fpa_shiny_load(hotel)
check("hotel pack loads", isTRUE(p$ok) && nrow(p$finance$pnl) == 12L)
st <- fpa_close_state(p)
lock_file <- utils::read.csv(file.path(p$paths$work, "standing-lock.csv"), stringsAsFactors = FALSE)

# ---- State --------------------------------------------------------------
check("closed month is the last actual month (2026-09)", identical(st$closed, "2026-09") && length(st$act) == 9L)
check(sprintf("lock months read from the file (%d)", length(st$lock_months)), length(st$lock_months) == length(unique(lock_file$periode)))
check("September is closed but not locked", identical(st$unlocked, "2026-09"))
check("current forecast matches a saved version", !is.null(st$saved) && identical(st$saved$md5[[1]], st$cur_md5))
check("current year totals tie to the P&L", near(st$current$rev, sum(p$finance$pnl$REV)) && near(st$current$oi, sum(p$finance$pnl$OI)))

# ---- Versions and default pair -----------------------------------------
v <- st$versions
g <- fpa_close_groups(v)
n_distinct_runs <- sum(!v$same_as_prev)
check(sprintf("%d versions, %d runs of identical files", nrow(v), length(g)), length(g) == n_distinct_runs && sum(vapply(g, function(x) x$n, integer(1))) == nrow(v))
check("groups are newest first", identical(g[[1]]$id, utils::tail(v$id, 1)))
pair <- fpa_close_default_pair(v, st$cur_md5)
md5_of <- function(id) if (identical(id, "current")) st$cur_md5 else v$md5[v$id == id][[1]]
check("default pair compares two different files", md5_of(pair[["a"]]) != md5_of(pair[["b"]]))
check("default arrival is the version of the calculation on screen", identical(md5_of(pair[["b"]]), st$cur_md5))
check("default start is the last different version before it", {
  ib <- match(pair[["b"]], v$id); ia <- match(pair[["a"]], v$id)
  ia < ib && all(v$md5[(ia + 1L):ib] == v$md5[[ib]])
})
ch <- fpa_close_pick_choices(st)
check("picker offers one entry per run, no current row when saved", length(ch) == length(g) && !("current" %in% ch))
check("picker labels carry dates, not ids", no_raw(paste(names(ch), collapse = " ")))

# ---- Compare: tie-outs --------------------------------------------------
a <- fpa_close_read_pnl(p$paths, pair[["a"]])
b <- fpa_close_read_pnl(p$paths, pair[["b"]])
cmp <- fpa_close_compare(a, b, st$months)
op <- fpa_close_op_lines()
check("compare runs on the 12 months of the calculation", length(cmp$months) == 12L)
check(sprintf("operating lines sum to the OI gap (%s)", fpa_var_eur_s(cmp$oi_b - cmp$oi_a)), near(sum(cmp$impact[op, ]), cmp$oi_b - cmp$oi_a, 0.01))
check("operating lines sum to the OI gap month by month", all(abs(colSums(cmp$impact[op, , drop = FALSE]) - cmp$oi) < 0.01))
check("all lines sum to the net income gap", near(sum(cmp$impact), cmp$ni_b - cmp$ni_a, 0.01))
check("OI totals tie to the version files", near(cmp$oi_a, sum(a$OI[a$periode %in% st$months])) && near(cmp$oi_b, sum(b$OI)))
check("a higher cost counts negative", {
  l <- "PAYROLL"; m <- cmp$months[[12]]
  raw <- b[[l]][b$periode == m] - a[[l]][a$periode == m]
  near(cmp$impact[l, m], -raw, 0.01)
})
check(sprintf("moved months are the ones with a gap (%d)", length(cmp$moved)), all(colSums(abs(cmp$impact[, cmp$moved, drop = FALSE]) >= 0.5) > 0) &&
  all(colSums(abs(cmp$impact[, setdiff(cmp$months, cmp$moved), drop = FALSE]) >= 0.5) == 0))

# ---- Bridge ------------------------------------------------------------
bf <- fpa_close_bridge_frame(cmp, "A", "B")
check("bridge starts at the start OI and ends at the end OI", near(bf$end[[1]], cmp$oi_a, 0.01) && near(bf$end[[nrow(bf)]], cmp$oi_b, 0.01))
check("bridge totals start at zero", all(bf$start[bf$kind == "total"] == 0))
check("bridge chain lands on the end OI", near(bf$end[[nrow(bf) - 1L]], cmp$oi_b, 0.01))
check("bridge steps carry business labels", no_raw(paste(bf$label, collapse = " ")))
if (FPA_HAS_PLOTLY) {
  fig <- plotly::plotly_build(fpa_var_bridge_fig(bf))
  shp <- fig$x$layout$shapes
  check("bridge connectors are horizontal", length(shp) == nrow(bf) - 1L && all(vapply(shp, function(s) s$y0 == s$y1, logical(1))))
  check("bridge uses no plotly total measure", !any(grepl("measure", names(fig$x$data[[1]]))))
}

# ---- Compare table -------------------------------------------------------
th <- html(fpa_close_cmp_table(cmp, st$act, st$year))
check("compare table shows business labels only", no_raw(th))
check("compare table has one column per moved month plus total", count("<th class=\"is-", th) == length(cmp$moved))
check("compare table OI total ties", grepl(fpa_var_eur_s(cmp$oi_b - cmp$oi_a), visible(th), fixed = TRUE))
check("negative cells are flagged and use a real minus", grepl("is-neg", th) && grepl("\u2212", th))
check("hidden months are named", grepl("Mois sans écart masqués", th))
check("compare fact names both versions and the OI move", {
  f <- fpa_close_cmp_fact(cmp, fpa_close_ver_ref(v, pair[["a"]]), fpa_close_ver_ref(v, pair[["b"]]), st$year)
  grepl("la version du", f) && grepl(fpa_var_eur(cmp$oi_a), f, fixed = TRUE) && grepl(fpa_var_eur(cmp$oi_b), f, fixed = TRUE) && no_raw(f)
})
csv <- fpa_close_cmp_csv(cmp)
check("export rows tie to the table (OI row total)", near(csv$Total[csv$Ligne == fpa_shiny_line_name("OI")], cmp$oi_b - cmp$oi_a, 0.01))

# ---- Register ------------------------------------------------------------
rh <- html(fpa_close_register_ui(st))
check("register: one row per run of identical files", count("<tr", rh) == length(g) + 1L)
check("register names identical copies", any(vapply(g, function(x) x$n > 1L, logical(1))) == grepl("copies identiques", rh))
check("register flags a version that covers part of the year", any(v$n_months < 12L) == grepl("mois sur 12", rh))
check("register marks the calculation on screen", grepl("calcul affiché", rh))
check("register shows no raw ids", no_raw(rh))
check("register uses the singular for one copy", !grepl("+1 copies", visible(rh), fixed = TRUE) && !any(grepl("+1 copies", names(ch), fixed = TRUE)))
check("register latest revenue ties to the P&L", grepl(fpa_var_eur(st$current$rev), visible(rh), fixed = TRUE))
check("register fact counts versions and different ones", grepl(sprintf("%d versions enregistrées, dont %d différentes", nrow(v), length(g)), fpa_close_reg_fact(st), fixed = TRUE))

# ---- Drift -----------------------------------------------------------------
src <- fpa_close_source_pnl(p$config, p$paths)
check("source P&L equals the engine on the unlocked month", near(src$REV[src$periode == "2026-09"], p$finance$pnl$REV[p$finance$pnl$periode == "2026-09"], 0.01) &&
  near(src$OI[src$periode == "2026-09"], p$finance$pnl$OI[p$finance$pnl$periode == "2026-09"], 0.01))
drift <- fpa_close_drift(lock_file, src)
dm <- fpa_close_drift_months(st, drift)
check(sprintf("drift months inside the calculation (%d)", nrow(dm)), all(dm$periode %in% st$months))
check("drift: locked revenue ties to the P&L", near(sum(dm$rev_lock), sum(p$finance$pnl$REV[p$finance$pnl$periode %in% dm$periode]), 0.01))
check("drift: file revenue ties to the source", near(sum(dm$rev_file), sum(src$REV[src$periode %in% dm$periode]), 0.01))
check("drift: file OI ties to the source", near(sum(dm$oi_file), sum(src$OI[src$periode %in% dm$periode]), 0.01))
check("drift: OI gap is file minus lock", near(sum(dm$oi_gap), sum(dm$oi_file) - sum(dm$oi_lock), 0.01))
lh <- html(fpa_close_lock_ui(st, drift))
check("lock card warns when the file differs", (nrow(dm) > 0L) == grepl("ne correspondent plus au fichier", lh))
check("lock card offers the lock button for September", grepl("Verrouiller septembre 2026", lh))
check("lock card says older months have no effect", grepl("hors du calcul actuel", lh) == any(!st$lock_months %in% st$months))
check("lock card shows no raw ids", no_raw(lh))
check("lock grid shows the months of the calculation only", grepl("mois du calcul", lh) && !grepl("sept. 25", lh, fixed = TRUE) && grepl("août 26", lh, fixed = TRUE))
check("lock fact states count, span and open month", {
  f <- fpa_close_lock_fact(st, drift)
  grepl(sprintf("%d mois verrouillés", length(st$lock_months)), f) && grepl("Septembre 2026 pas encore verrouillé", f)
})

# ---- Lock merge and accept on a temp copy --------------------------------
tmp <- file.path(tempdir(), "close-check")
dir.create(file.path(tmp, "work"), recursive = TRUE, showWarnings = FALSE)
tp <- list(work = file.path(tmp, "work"), reports = file.path(tmp, "reports"))
old <- data.frame(periode = c("2026-01", "2026-01"), line_id = c("REV", "PAYROLL"), montant = c(100, 50))
new <- data.frame(periode = c("2026-01", "2026-01", "2026-02"), line_id = c("REV", "PAYROLL", "REV"), montant = c(999, 999, 200))
mg <- fpa_close_lock_merge(old, new)
check("lock merge keeps locked amounts", mg$montant[mg$periode == "2026-01" & mg$line_id == "REV"] == 100)
check("lock merge adds the new month", nrow(mg) == 3L && any(mg$periode == "2026-02"))
utils::write.csv(mg, file.path(tp$work, "standing-lock.csv"), row.names = FALSE)
n <- fpa_close_accept_drift(tp, data.frame(periode = "2026-01", line_id = "REV", locked = 100, source = 120, delta = 20))
back <- utils::read.csv(file.path(tp$work, "standing-lock.csv"))
check("accept drift changes only the drifted row", n == 1L && back$montant[back$periode == "2026-01" & back$line_id == "REV"] == 120 &&
  back$montant[back$line_id == "PAYROLL"] == 50 && back$montant[back$periode == "2026-02"] == 200)

# ---- Budget --------------------------------------------------------------
bud <- st$budget
check("budget is frozen and unchanged", isTRUE(bud$frozen) && isTRUE(bud$intact))
oi_bud <- fpa_close_budget_oi(bud$table, "frozen")
raw_b <- utils::read.csv(file.path(p$paths$work, "vintages", "budget-frozen.csv"), stringsAsFactors = FALSE)
oi_raw <- sum(vapply(op, function(l) fpa_pnl_ni_sign(l) * sum(raw_b$montant[raw_b$functional_label == l]), numeric(1)))
check(sprintf("budget OI ties to the frozen file (%s)", fpa_var_eur(oi_bud)), near(oi_bud, oi_raw, 0.01))
bh <- html(fpa_close_budget_ui(bud))
check("budget table drops the gap column when intact", !grepl(">Écart<", bh) && grepl("identiques", bh))
check("budget table shows no section or subtotal rows", !grepl("SECTION|SUBTOTAL", bh) && no_raw(bh))
check("budget OI total sits after the operating lines, before financial lines", {
  vb <- visible(bh)
  regexpr("Dotations", vb) < regexpr("Résultat d'exploitation", vb) && regexpr("Résultat d'exploitation", vb) < regexpr("Résultat financier", vb)
})
moved <- bud
moved$intact <- FALSE
moved$table$current[moved$table$line_id == "REV"] <- moved$table$current[moved$table$line_id == "REV"] + 1000
moved$table$delta <- moved$table$current - moved$table$frozen
check("changed budget shows the gap and the OI move", grepl(">Écart<", html(fpa_close_budget_ui(moved))) && grepl("+1 000", fpa_close_budget_fact(moved), fixed = TRUE))

# ---- Steps ---------------------------------------------------------------
steps <- fpa_close_steps(st, drift)
states <- vapply(steps, function(s) s$state, character(1))
check("steps cover data, checks, result, lock, save, budget", identical(names(steps), c("data", "checks", "result", "lock", "save", "budget")))
check("result step is for reading only", identical(states[["result"]], "info"))
check("lock step asks for a decision when the file differs", identical(states[["lock"]], "now") &&
  identical(steps$lock$label, if (nrow(dm)) fpa_txt("close_s_lock_drift_lab") else fpa_txt("close_s_lock_one", "septembre 2026")))
check("save and budget steps are done on this calculation", identical(states[["save"]], "done") && identical(states[["budget"]], "done"))
sh <- html(fpa_close_steps_ui(st, steps))
check("steps show a shape and a word for each state", count("fpa-close-mark", sh) == length(steps) && grepl("aria-label=\"Fait\"", sh) && grepl("aria-label=\"À faire\"", sh))
check("result step ties to the September OI", grepl(fpa_var_eur(p$finance$pnl$OI[p$finance$pnl$periode == "2026-09"]), sh, fixed = TRUE))
check("steps show no raw ids", no_raw(sh))
f <- fpa_close_steps_fact(st, steps)
gate <- sum(states != "info")
check(sprintf("fact counts gated steps (%d)", gate), grepl(sprintf("sur %d faites", gate), f) && grepl("septembre 2026", f))

st2 <- st
st2$saved <- NULL
st2$budget$frozen <- FALSE
st2$unlocked <- character(0)
s2 <- fpa_close_steps(st2, drift[0, , drop = FALSE])
check("unsaved forecast asks to save with a label input", identical(s2$save$state, "now") && grepl("close_label", html(s2$save$action)) && grepl("Clôture septembre 2026", html(s2$save$action)))
check("unfrozen budget asks to freeze", identical(s2$budget$state, "now") && grepl("close_freeze_btn", html(s2$budget$action)))
check("all months locked and no drift: lock step done", identical(s2$lock$state, "done"))
check("unsaved state adds the current row to the register", grepl("Non enregistrée", html(fpa_close_register_ui(st2))))
check("unsaved state offers the current forecast in the picker", "current" %in% fpa_close_pick_choices(st2))

# ---- Extremes ------------------------------------------------------------
same <- fpa_close_compare(b, b, st$months)
check("identical versions: no moved month, plain sentence, no table", !length(same$moved) && is.null(fpa_close_cmp_table(same)) &&
  grepl("aucun montant ne change", fpa_close_cmp_fact(same, "x", "y", "2026")))
part <- fpa_close_compare(fpa_close_read_pnl(p$paths, v$id[[1]]), b, st$months)
pf <- fpa_close_cmp_fact(part, "x", "y", st$year, length(st$months))
check(sprintf("partial overlap (%d months) is named, never called the year", length(part$months)),
  length(part$months) < 12L && grepl(sprintf("%d mois communs", length(part$months)), pf) && !grepl("sur 2026", pf))
one <- fpa_close_compare(a, b, "2026-12")
check("one month compare keeps a line by month matrix", is.matrix(one$impact) && ncol(one$impact) == 1L && near(sum(one$impact[op, ]), one$oi, 0.01))
long_a <- fpa_close_read_pnl(p$paths, v$id[[1]])
long_b <- fpa_close_read_pnl(p$paths, v$id[[2]])
lc <- fpa_close_compare(long_a, long_b)
check(sprintf("long span compare (%d months) ties", length(lc$months)), length(lc$months) >= 24L && near(sum(lc$impact[op, ]), lc$oi_b - lc$oi_a, 0.01))
check("long span table headers carry the year", grepl(" 2[0-9]\\s*<small>", html(fpa_close_cmp_table(lc, st$act, "x"))))
empty_st <- st
empty_st$versions <- data.frame()
empty_st$saved <- NULL
check("no version: register and picker say so", grepl("Aucune version", html(fpa_close_register_ui(empty_st))) &&
  grepl("deux versions différentes", html(fpa_close_picks_ui(empty_st, "", ""))))
nolock <- st
nolock$lock <- data.frame()
nolock$lock_months <- character(0)
check("no lock: fact says so", grepl("Aucun mois verrouillé", fpa_close_lock_fact(nolock, NULL)))
check("drift with no source is empty", nrow(fpa_close_drift(lock_file, NULL)) == 0L)
check("no budget frozen: fact says so", grepl("pas encore figé", fpa_close_budget_fact(list(frozen = FALSE))))

# ---- Copy ------------------------------------------------------------------
src_app <- paste(readLines("app.R", encoding = "UTF-8"), collapse = "\n")
keys <- unique(regmatches(src_app, gregexpr("fpa_txt\\(\"close_[a-z0-9_]+\"", src_app))[[1]])
keys <- sub("^fpa_txt\\(\"", "", sub("\"$", "", keys))
miss <- keys[!vapply(keys, function(k) !is.null(fpa_copy[[k]]) && nzchar(fpa_copy[[k]][["fr"]]) && nzchar(fpa_copy[[k]][["en"]]), logical(1))]
check(sprintf("all %d close copy keys have fr and en", length(keys)), !length(miss))
if (length(miss)) cat("missing:", miss, "\n")

cat(sprintf("\n%d pass, %d fail\n", pass, fail))
if (fail > 0L) quit(status = 1L)
