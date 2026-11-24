# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Écarts tab: heat map, line story, bridge, causes and table must tie to the variance file.
# Run from fpa-local: Rscript tests/check-variance-page.R
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()
suppressPackageStartupMessages(library(shiny))

exprs <- parse("app.R", encoding = "UTF-8")
for (e in exprs) {
  if (!is.call(e) || !identical(as.character(e[[1]]), "<-") || !is.name(e[[2]])) next
  nm <- as.character(e[[2]])
  rhs <- e[[3]]
  is_fun <- is.call(rhs) && identical(rhs[[1]], as.name("function"))
  if (is_fun || grepl("^fpa_var_", nm)) eval(e, globalenv())
}

pass <- 0L
fail <- 0L
check <- function(label, ok) {
  if (isTRUE(ok)) { pass <<- pass + 1L; cat("PASS", label, "\n") } else { fail <<- fail + 1L; cat("FAIL", label, "\n") }
}
html <- function(x) as.character(htmltools::renderTags(x)$html)
visible <- function(s) gsub("<[^>]+>", " ", s)
count <- function(pattern, s) lengths(regmatches(s, gregexpr(pattern, s, fixed = TRUE)))
near <- function(a, b, tol = 1) is.finite(a) && is.finite(b) && abs(a - b) <= tol
raw_ids <- "\\b(REV|OTHER_OP_INC|COGS|PAYROLL|OPEX_OTHER|D_A|FIN|EXCEP|TAX|unexplained|variance_pct|headcount|line_id)\\b"

hotel <- "../fpa-local-test/fpa-config.json"
cfg <- fpa_load_config(hotel)
paths <- fpa_paths(cfg)
v <- utils::read.csv(file.path(paths$work, "variance-monthly.csv"), stringsAsFactors = FALSE)
st <- utils::read.csv(file.path(paths$work, "period-status.csv"), stringsAsFactors = FALSE)
v <- fpa_variance_tag_status(v, st)
vol <- fpa_read_volumes(cfg, paths)$data
check(sprintf("hotel variance loads (%d rows, %d lines)", nrow(v), length(unique(v$line_id))), nrow(v) >= 12L && "REV" %in% v$line_id)

rev <- v[v$line_id == "REV", , drop = FALSE]
rev <- rev[order(rev$periode), , drop = FALSE]
rev <- fpa_var_split(rev, vol)
check("revenue splits into phasing and unreconciled", fpa_var_has_split(rev))
u <- rev$unexplained
u[!is.finite(u)] <- 0
check("phasing + unreconciled = rest to explain, every month", all(abs(rev$phase + rev$unlinked - u) < 0.01))
ls <- fpa_var_line_sum(rev)
check(sprintf("revenue year variance %s", fpa_var_eur_s(ls$year)), near(ls$year, 18738))
check(sprintf("revenue closed + rest = year (%s + %s)", fpa_var_eur_s(ls$closed), fpa_var_eur_s(ls$rest)), near(ls$closed + ls$rest, ls$year, 0.01))
parts <- fpa_var_parts(rev)
pv <- function(id) { h <- Filter(function(p) identical(p$id, id), parts); if (length(h)) h[[1]]$v else 0 }
check(sprintf("price %s, volume %s, mix %s", fpa_var_eur_s(pv("price")), fpa_var_eur_s(pv("volume")), fpa_var_eur_s(pv("mix"))),
      near(pv("price"), 2883) && near(pv("volume"), 2392) && near(pv("mix"), 185))
check(sprintf("phasing %s, unreconciled %s", fpa_var_eur_s(pv("phase")), fpa_var_eur_s(pv("unlinked"))),
      near(pv("phase"), 19098) && near(pv("unlinked"), -5820))
check("causes add back to the year variance", near(sum(vapply(parts, function(p) p$v, numeric(1))), ls$year, 0.01))

bf <- fpa_var_bridge_frame(rev)
check(sprintf("bridge starts at the year budget %s", fpa_var_eur(bf$raw[[1]])), near(bf$raw[[1]], 115500) && bf$kind[[1]] == "total")
check(sprintf("bridge lands on actual + forecast %s", fpa_var_eur(bf$raw[[nrow(bf)]])), near(bf$raw[[nrow(bf)]], 134238) && near(bf$raw[[nrow(bf)]], ls$actual, 0.01))
check("bridge chain is continuous (each step starts where the last ended)",
      all(abs(bf$start[3:(nrow(bf) - 1)] - bf$end[2:(nrow(bf) - 2)]) < 0.01) && near(bf$start[[2]], bf$end[[1]], 0.01) && near(bf$end[[nrow(bf) - 1]], bf$end[[nrow(bf)]], 0.01))
check("bridge totals stand on zero", all(bf$start[bf$kind == "total"] == 0))

story <- fpa_var_story(rev, vol)
s_all <- paste(story, collapse = " ")
check("story names the flat budget (9 625 a month)", grepl("budget de cette ligne est plat", s_all) && grepl("9 625", s_all, fixed = TRUE))
check("story names phasing with 115 500 and 134 598", grepl("phasage", s_all) && grepl("115 500", s_all, fixed = TRUE) && grepl("134 598", s_all, fixed = TRUE))
check("story names September 9 300 against 15 120", grepl("septembre 2026", s_all) && grepl("9 300", s_all, fixed = TRUE) && grepl("15 120", s_all, fixed = TRUE))

pay <- v[v$line_id == "PAYROLL", , drop = FALSE]
pay <- fpa_var_split(pay[order(pay$periode), , drop = FALSE], vol)
check("payroll has no phasing split", !fpa_var_has_split(pay))
pb <- fpa_var_bridge_frame(pay)
ps <- fpa_var_line_sum(pay)
check("payroll bridge: budget, closed months, forecast, landing", nrow(pb) %in% c(3L, 4L) && near(pb$raw[[nrow(pb)]], ps$actual, 0.01))
check("payroll story says nothing splits it", any(grepl("Aucun mécanisme", fpa_var_story(pay))))

h <- html(fpa_var_heat_ui(v, "REV", "2026-09"))
present <- unique(v$line_id)
zero_lines <- present[vapply(present, function(l) all(abs(fpa_var_num(v$variance[v$line_id == l])) < 0.5), logical(1))]
n_rows <- length(setdiff(present, zero_lines))
check(sprintf("heat map: %d line rows + 1 operating row", n_rows), count('class="fpa-vh-row', h) == n_rows + 1L && count("is-oi", h) == 1L)
check("heat map: selected line and month highlighted once", count("fpa-vh-row is-on", h) == 1L && count("is-sel", h) >= 1L)
check("heat map: 3 forecast month headers dashed", count('<th class="is-fc"', h) == 3L || count('class="is-fc', h) >= 3L)
check("heat map: no raw ids on screen", !grepl(raw_ids, visible(h)))
check("heat map: September revenue cell reads in k", grepl("\u2212", visible(h)) && grepl("\u00a0k", visible(h)))
oi_sum <- sum(vapply(which(v$line_id %in% fpa_var_oi_lines), function(i) fpa_var_sg(v$line_id[[i]], v$variance[[i]]), numeric(1)), na.rm = TRUE)
check(sprintf("heat map: operating year total %s shown", fpa_var_eur_s(oi_sum)), grepl(fpa_var_eur_s(oi_sum), h, fixed = TRUE))
hero <- html(fpa_var_hero_ui(v))
hm <- fpa_var_hero_math(v)
slice <- function(s, a, b) sub(b, "", sub(a, "", s))
closed_bit <- slice(hero, '.*id="fpa-var-math-closed"', 'id="fpa-var-math-rest".*')
rest_bit <- slice(hero, '.*id="fpa-var-math-rest"', 'id="fpa-var-math-year".*')
year_bit <- slice(hero, '.*id="fpa-var-math-year"', "")
check("hero: the same operating year total", grepl(fpa_var_eur_s(oi_sum), hero, fixed = TRUE))
check("hero: no raw ids on screen", !grepl(raw_ids, visible(hero)))
check("hero: closed, then the rest, equals the year", count("fpa-var-join", hero) == 2L && grepl("puis", hero, fixed = TRUE) && grepl("égale", hero, fixed = TRUE) && count("fpa-var-arrow", hero) == 2L)
check("hero: three cards open a calculation", count("<button", hero) == 3L && count("id=\"fpa-var-math-", hero) == 3L && count("window.fpaVarTile(this)", hero) == 3L)
check(sprintf("closed lines sum to the card %s", fpa_var_eur_s(hm$closed)),
      sum(hm$closed_lines$shown) == hm$closed_lines$total && hm$closed_lines$total == round(hm$closed) && grepl(fpa_var_eur_s(hm$closed), closed_bit, fixed = TRUE))
check(sprintf("closed months sum to the card %s", fpa_var_eur_s(hm$closed)),
      sum(hm$closed_months$shown) == round(hm$closed) && grepl(fpa_var_eur_s(hm$closed), closed_bit, fixed = TRUE))
check(sprintf("rest lines and months sum to the card %s", fpa_var_eur_s(hm$rest)),
      sum(hm$rest_lines$shown) == round(hm$rest) && sum(hm$rest_months$shown) == round(hm$rest) && grepl(fpa_var_eur_s(hm$rest), rest_bit, fixed = TRUE))
check(sprintf("year periods and lines sum to the card %s", fpa_var_eur_s(hm$year)),
      sum(hm$year_periods$shown) == round(hm$year) && sum(hm$year_lines$shown) == round(hm$year) && grepl(fpa_var_eur_s(hm$year), year_bit, fixed = TRUE))

tb <- html(fpa_var_table_ui(rev, "2026-09"))
jan <- rev[rev$periode == "2026-01", , drop = FALSE]
qp_jan <- fpa_var_num(jan$budget[[1]]) + fpa_var_num(jan$phase[[1]])
check("table: 12 month rows + total", count("<tr", tb) == 1L + nrow(rev) + 1L)
check(sprintf("table: January quantity x budgeted rate %s, same series as the chart", fpa_var_eur(qp_jan)),
      grepl("Quantité", tb, fixed = TRUE) && grepl(fpa_var_eur(qp_jan), tb, fixed = TRUE) && grepl(fpa_var_eur(jan$actual[[1]]), tb, fixed = TRUE) && grepl(fpa_var_eur(jan$budget[[1]]), tb, fixed = TRUE))
pay_tb <- html(fpa_var_table_ui(pay))
check("payroll table has no quantity x rate column", !grepl("Quantité", pay_tb, fixed = TRUE))
h_tip <- html(fpa_var_heat_ui(v, "REV", "2026-01", vol))
check("heat tip names actual, budget, quantity x rate and the variance",
      grepl("data-tip-html", h_tip, fixed = TRUE) && grepl("Réalisé", h_tip, fixed = TRUE) && grepl("Budget", h_tip, fixed = TRUE) && grepl("Quantité", h_tip, fixed = TRUE) && grepl(fpa_var_eur(qp_jan), h_tip, fixed = TRUE) && grepl(fpa_var_eur_s(fpa_var_sg("REV", jan$variance[[1]])), h_tip, fixed = TRUE))
check("table: September highlighted", count("is-sel", tb) == 1L)
check("table: phasing and unreconciled columns, rest column replaced", grepl("Phasage", tb) && grepl("Non relié", tb) && !grepl("Reste à expliquer", tb))
check("table: hidden empty columns named", grepl("Colonnes masquées", tb) && grepl("Taux", tb))
check("table: total gap equals year variance", grepl(fpa_var_eur_s(ls$year), tb, fixed = TRUE))
check("table: no raw ids or long decimals", !grepl(raw_ids, visible(tb)) && !grepl("[0-9]\\.[0-9]{2,}", visible(tb)))
pvf <- fpa_var_pv_frame(rev)
pvr <- pvf$rows
check(sprintf("price or volume: %d closed months split, %d forecast months named", sum(!pvr$fc), length(pvf$idle)),
      sum(!pvr$fc) == 9L && length(pvf$idle) == 3L)
check(sprintf("price %s + volume %s + mix %s = %s", fpa_var_eur_s(sum(pvr$price)), fpa_var_eur_s(sum(pvr$volume)), fpa_var_eur_s(sum(pvr$mix)), fpa_var_eur_s(sum(pvr$price + pvr$volume + pvr$mix))),
      near(sum(pvr$price), pv("price"), 0.01) && near(sum(pvr$volume), pv("volume"), 0.01) && near(sum(pvr$mix), pv("mix"), 0.01) && round(sum(pvr$price + pvr$volume + pvr$mix)) == 5460)
check("each month: price + volume + mix = nights sold x rate - budget nights x budget rate",
      all(abs(pvr$price + pvr$volume + pvr$mix - (pvr$q1 * pvr$p1 - pvr$q0 * pvr$p0)) < 0.01))
apr <- pvr[pvr$periode == "2026-04", , drop = FALSE]
check("April: price 88 x (93,99 - 97) = -265, volume 97 x (70 - 88) = -1 746",
      round(apr$price) == -265 && round(apr$volume) == -1746 &&
        fpa_var_pv_formula("price", apr$q0, apr$p0, apr$q1, apr$p1) == "88 \u00d7 \u22123,01" &&
        fpa_var_pv_formula("volume", apr$q0, apr$p0, apr$q1, apr$p1) == "97 \u00d7 \u221218")
check("price or volume chart is absent when the line does not split", is.null(fpa_var_pv_fig(pay, "")))
if (requireNamespace("plotly", quietly = TRUE)) {
  w <- fpa_var_pv_fig(rev, "2026-04")
  pf <- plotly::plotly_build(w)
  bars <- Filter(function(d) identical(d$type, "bar"), pf$x$data)
  ai <- match("2026-04", pvr$periode)
  tip <- w$jsHooks$render[[1]]$data$rows
  check("price or volume chart: three series, nine months, sums tie to +2 392, +2 883 and +185",
        length(bars) == 3L && length(bars[[1]]$y) == 9L &&
          near(sum(bars[[1]]$y), sum(pvr$volume), 0.01) &&
          near(sum(bars[[2]]$y), sum(pvr$price), 0.01) &&
          near(sum(bars[[3]]$y), sum(pvr$mix), 0.01) &&
          near(sum(bars[[1]]$y) + sum(bars[[2]]$y) + sum(bars[[3]]$y), 5460, 0.5))
  check("April bars are volume -1 746, price -265, mix +54",
        near(bars[[1]]$y[[ai]], -1746, 1) && near(bars[[2]]$y[[ai]], -265, 1) && near(bars[[3]]$y[[ai]], 54, 1))
  cum_line <- Filter(function(d) identical(d$mode, "lines+markers"), pf$x$data)
  cum_exp <- list(cumsum(pvr$volume), cumsum(pvr$price), cumsum(pvr$mix))
  cum_y <- unlist(cum_exp)
  zero_share <- function(rng) abs(min(0, rng[[1]])) / (abs(min(0, rng[[1]])) + max(0, rng[[2]]))
  yb <- pf$x$layout$yaxis$range
  y2 <- pf$x$layout$yaxis2$range
  check("three cumulative lines, one per effect, end at +2 392, +2 883 and +185",
        length(cum_line) == 3L &&
          all(vapply(seq_along(cum_exp), function(k) length(cum_line[[k]]$y) == nrow(pvr) && all(abs(cum_line[[k]]$y - cum_exp[[k]]) < 0.01), logical(1))) &&
          near(cum_line[[1]]$y[[nrow(pvr)]], 2392, 0.5) && near(cum_line[[2]]$y[[nrow(pvr)]], 2883, 0.5) && near(cum_line[[3]]$y[[nrow(pvr)]], 185, 0.5) &&
          near(sum(vapply(cum_line, function(d) d$y[[nrow(pvr)]], numeric(1))), 5460, 0.5) &&
          identical(cum_line[[1]]$name, "Cumul volume") && identical(cum_line[[2]]$name, "Cumul prix") && identical(cum_line[[3]]$name, "Cumul mix") &&
          identical(cum_line[[1]]$line$color, "#1f4e5f") && identical(cum_line[[2]]$line$color, "#c47b2b") && identical(cum_line[[3]]$line$color, "#3b6fb6") &&
          all(vapply(cum_line, function(d) identical(d$marker$color, "#ffffff") && identical(d$marker$line$color, d$line$color) && !identical(d$marker$color, d$marker$line$color), logical(1))) &&
          grepl("Cumul volume", tip[[ai]], fixed = TRUE) && grepl("Cumul prix", tip[[ai]], fixed = TRUE) && grepl("Cumul mix", tip[[ai]], fixed = TRUE) &&
          !grepl("Cumul des trois", tip[[ai]], fixed = TRUE) &&
          identical(pf$x$layout$yaxis2$overlaying, "y") && identical(pf$x$layout$yaxis2$side, "right"))
  check("the two axes cross zero at the same height",
        near(zero_share(yb), zero_share(y2), 0.001) && yb[[1]] < 0 && yb[[2]] > 0 && y2[[1]] < 0 && y2[[2]] > 0 &&
          min(unlist(lapply(bars, function(d) d$y))) >= yb[[1]] && max(unlist(lapply(bars, function(d) d$y))) <= yb[[2]] &&
          min(cum_y) >= y2[[1]] && max(cum_y) <= y2[[2]])
  check("price or volume chart: plain legend, mix is blue, no hatch, hover writes April's calculation",
        identical(bars[[1]]$name, "Volume") && identical(bars[[2]]$name, "Prix") && identical(bars[[3]]$name, "Mix") &&
          identical(bars[[1]]$marker$color, "#1f4e5f") && identical(bars[[3]]$marker$color, "#3b6fb6") &&
          !any(vapply(bars, function(d) grepl("[\u25CF\u25A0\u25C6]", d$name), logical(1))) &&
          !any(vapply(bars, function(d) !is.null(d$marker$pattern), logical(1))) &&
          !grepl("\u25CF", fpa_txt("var_pv_chart_cap"), fixed = TRUE) &&
          grepl("88 \u00d7 \u22123,01", tip[[ai]], fixed = TRUE) && grepl("97 \u00d7 \u221218", tip[[ai]], fixed = TRUE))
  check("price or volume chart: nine months drop bar labels",
        all(vapply(bars, function(d) is.null(d$text) || !any(nzchar(as.character(d$text))), logical(1))))
  one_w <- fpa_var_pv_fig(rev[rev$periode == "2026-04", , drop = FALSE], "")
  one_b <- plotly::plotly_build(one_w)
  one_bars <- Filter(function(d) identical(d$type, "bar"), one_b$x$data)
  check("one month: three bars, each labelled",
        length(one_bars) == 3L && length(one_bars[[1]]$x) == 1L &&
          all(vapply(one_bars, function(d) any(nzchar(as.character(d$text))), logical(1))))
  longp <- do.call(rbind, lapply(2024:2026, function(y) {
    x <- rev
    x$periode <- sprintf("%d-%s", y, substr(rev$periode, 6, 7))
    x
  }))
  long_n <- nrow(fpa_var_pv_frame(longp)$rows)
  long_b <- plotly::plotly_build(fpa_var_pv_fig(longp, ""))
  long_bars <- Filter(function(d) identical(d$type, "bar"), long_b$x$data)
  check("long span: every kept month is a bar, labels dropped, ticks thinned",
        long_n > 24L && length(long_bars[[1]]$x) == long_n &&
          all(vapply(long_bars, function(d) is.null(d$text) || !any(nzchar(as.character(d$text))), logical(1))) &&
          isTRUE(long_b$x$layout$xaxis$tickangle == -45))
}
pvh <- html(fpa_var_pv_ui(rev, "2026-04"))
check("price or volume: April row shows 88 to 70 nights and 97 to 93,99 EUR, highlighted",
      grepl("88 \u2192 70", pvh, fixed = TRUE) && grepl("97,00 \u2192 93,99", pvh, fixed = TRUE) && count("is-sel", pvh) == 1L)
check("price or volume: rate under budget January to April, nights under budget January and April",
      grepl("Tarif sous le budget : janvier, février, mars, avril. ", pvh, fixed = TRUE) && grepl("Nuitées sous le budget : janvier, avril. ", pvh, fixed = TRUE) && !grepl("..", visible(pvh), fixed = TRUE))
check("price or volume: total rate is a two-decimal average on both sides", grepl("\\d{3},\\d{2} \u2192 \\d{3},\\d{2}", visible(pvh)))
check("price or volume: total row ties to +5 460, no raw ids", grepl("+5 460", pvh, fixed = TRUE) && !grepl(raw_ids, visible(pvh)))
check("price or volume: up and down glyphs are shapes with words", grepl("\u25BC", pvh, fixed = TRUE) && grepl("\u25B2", pvh, fixed = TRUE) && grepl("sous le budget", pvh, fixed = TRUE))
check("price or volume: a source link opens the file popup", grepl("D'où viennent ces chiffres", pvh, fixed = TRUE) && grepl("var_pv_src", pvh, fixed = TRUE))
src <- html(fpa_var_pv_src_body(list(list(path = "input/volumes-mensuel-2026.csv", role = "Nuitées et tarifs"))))
check("source popup names the volumes file and the four columns, and does not send the reader to the drivers map",
      grepl("volumes-mensuel-2026.csv", src, fixed = TRUE) && grepl("nuitées du budget", src, fixed = TRUE) && grepl("tarif du budget", src, fixed = TRUE) && !grepl("Carte", src, fixed = TRUE) && !grepl("Drivers", src, fixed = TRUE))
empty_src <- html(fpa_var_pv_src_body(list()))
check("source popup says so when no volumes file is linked", grepl("Aucun fichier de volumes", empty_src, fixed = TRUE))
check("price or volume: hidden on payroll", is.null(fpa_var_pv_ui(pay)))
check("price or volume: one month still renders", nzchar(html(fpa_var_pv_ui(rev[rev$periode == "2026-04", , drop = FALSE]))))
gfig <- fpa_var_main_fig(rev, "gap", "")
grows <- unlist(gfig$jsHooks$render[[1]]$data$rows)
check("chart hover writes the calculation under price and volume", length(grows) == nrow(rev) && grepl("88 \u00d7 \u22123,01", grows[[4]], fixed = TRUE) && grepl("97 \u00d7 \u221218", grows[[4]], fixed = TRUE))
facts <- fpa_var_card_facts(v, rev)
check("folded hero states the operating year", grepl(fpa_var_eur_s(oi_sum), facts$hero, fixed = TRUE))
check("folded heat states operating year, the open line and its year variance",
      grepl(fpa_var_eur_s(oi_sum), facts$heat, fixed = TRUE) && grepl(fpa_shiny_line_name("REV"), facts$heat, fixed = TRUE) && grepl(fpa_var_eur_s(ls$year), facts$heat, fixed = TRUE))
check("folded price or volume states +2 883, +2 392, +185 and +5 460",
      grepl("+2 883", facts$pv, fixed = TRUE) && grepl("+2 392", facts$pv, fixed = TRUE) && grepl("+185", facts$pv, fixed = TRUE) && grepl("+5 460", facts$pv, fixed = TRUE))
check("folded bridge states budget, landing and the year variance",
      grepl("115 500", facts$bridge, fixed = TRUE) && grepl(fpa_var_eur_s(ls$year), facts$bridge, fixed = TRUE))
check("folded causes name phasing first", grepl("Phasage", facts$parts, fixed = TRUE) && grepl(fpa_var_eur_s(pv("phase")), facts$parts, fixed = TRUE))
check("folded month table states actual, budget and the year variance",
      grepl(fpa_var_eur(ls$actual), facts$months, fixed = TRUE) && grepl(fpa_var_eur(ls$budget), facts$months, fixed = TRUE) && grepl(fpa_var_eur_s(ls$year), facts$months, fixed = TRUE))
pay_facts <- fpa_var_card_facts(v, pay)
check("folded price or volume says payroll does not split", grepl("ne se découpe pas", pay_facts$pv, fixed = TRUE))
line_card <- html(fpa_var_fold("line", "Ligne ouverte", "fait", shiny::tags$p("corps"), open = TRUE))
other_card <- html(fpa_var_fold("pv", "Prix ou volume", "fait", shiny::tags$p("corps")))
check("only the named card carries the open attribute", grepl("<details[^>]*\\bopen\\b", line_card) && !grepl("<details[^>]*\\bopen\\b", other_card))

pp <- html(fpa_var_parts_ui(rev))
check("causes list: largest first is phasing", regexpr("Phasage du budget", pp) < regexpr("Prix", pp))

if (requireNamespace("plotly", quietly = TRUE)) {
  f1 <- plotly::plotly_build(fpa_var_main_fig(rev, "gap", "2026-09"))
  check("main chart (gap) builds with one trace per cause + labels + cumul", length(f1$x$data) >= length(parts) + 1L)
  f2 <- plotly::plotly_build(fpa_var_main_fig(rev, "level", ""))
  check("main chart (level) has actual, budget and budget q x p", length(f2$x$data) == 3L)
  f3 <- plotly::plotly_build(fpa_var_bridge_fig(bf))
  shp <- f3$x$layout$shapes
  check("bridge connectors are horizontal", length(shp) == nrow(bf) - 1L && all(vapply(shp, function(s) isTRUE(all.equal(s$y0, s$y1)), logical(1))))
  check("bridge uses no plotly waterfall totals", !any(vapply(f3$x$data, function(d) identical(d$type, "waterfall"), logical(1))))
}

# Extremes
one <- rev[rev$periode == "2026-01", , drop = FALSE]
check("single month: heat, story, table and bridge still render",
      nzchar(html(fpa_var_heat_ui(v[v$periode == "2026-01", , drop = FALSE], "REV", ""))) &&
        length(fpa_var_story(one)) >= 1L && nzchar(html(fpa_var_table_ui(one))) && !is.null(fpa_var_bridge_frame(one)))
zv <- v
zv$variance[zv$line_id == "TAX"] <- 0
hz <- html(fpa_var_heat_ui(zv, "REV", ""))
check("all-zero line is hidden and named", grepl("masquées : ", hz) && grepl("Impôt", hz) && !grepl(">Impôt</th>", hz))
check("empty variance: hero says so, heat is empty", grepl("Pas encore d'écarts", html(fpa_var_hero_ui(v[0, , drop = FALSE]))) && is.null(fpa_var_heat_ui(v[0, , drop = FALSE])))
long <- do.call(rbind, lapply(0:2, function(k) { x <- v; x$periode <- sprintf("%d-%s", 2024L + k, substr(v$periode, 6, 7)); x }))
hl <- html(fpa_var_heat_ui(long, "REV", ""))
check("36 months: cell text dropped, hover note shown, years in headers", grepl("is-dense", hl) && grepl("Survolez", hl) && grepl("janv. 25", hl))
if (requireNamespace("plotly", quietly = TRUE)) {
  lr <- fpa_var_split(long[long$line_id == "REV", , drop = FALSE][order(long$periode[long$line_id == "REV"]), , drop = FALSE], NULL)
  fl <- plotly::plotly_build(fpa_var_main_fig(lr, "gap", ""))
  check("36 months: chart keeps every bar, drops bar labels", length(fl$x$data[[1]]$x) == 36L && !any(vapply(fl$x$data, function(d) identical(d$mode, "text"), logical(1))))
}

cat(sprintf("\n%d pass, %d fail\n", pass, fail))
if (fail > 0L) quit(status = 1L)
