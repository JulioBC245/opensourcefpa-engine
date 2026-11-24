# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Break-even surface: the tooltip names both shocks, the income, and how far volume can fall.
`%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
options(fpa.lang = "en")
source(file.path(root, "R", "fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-chart-expert.R"), encoding = "UTF-8")

fail <- 0L
say <- function(ok, msg) {
  if (!ok) fail <<- fail + 1L
  cat(if (ok) "ok   " else "FAIL ", msg, "\n", sep = "")
}

home <- "../fpa-hotel-alpes/work"
pnl <- utils::read.csv(file.path(home, "pnl-monthly.csv"), stringsAsFactors = FALSE)
bs <- utils::read.csv(file.path(home, "bs-monthly.csv"), stringsAsFactors = FALSE)
status <- utils::read.csv(file.path(home, "period-status.csv"), stringsAsFactors = FALSE)
scenarios <- utils::read.csv(file.path(home, "scenarios-monthly.csv"), stringsAsFactors = FALSE)

fit <- fpa_chart_fit(pnl, status)
say(isTRUE(fit$ok) && identical(fit$n, 9L), "nine closed months set the cost line")
say(is.finite(fit$F) && is.finite(fit$v) && fit$v >= 0 && fit$v < 1, "variable cost is a share of revenue")
base_oi <- fpa_chart_oi(fit, 0, 0)
say(abs(base_oi - fit$oi0) < 1e-6, "the centre is the fitted operating income")
say(fpa_chart_cushion(fit, 0, 0) > 0, "the closed-month centre is above break-even")

tip <- fpa_chart_be_tip(fit, 0.01, 0.02)
say(grepl("Price", tip, fixed = TRUE) && grepl("Volume", tip, fixed = TRUE), "tooltip names price and volume")
say(grepl("Operating income", tip, fixed = TRUE), "tooltip names operating income")
say(grepl("margin", tip, fixed = TRUE), "tooltip names the margin")
say(grepl("before losses", tip, fixed = TRUE) || grepl("to break even", tip, fixed = TRUE) || grepl("does not cover", tip, fixed = TRUE), "tooltip gives the reading")
say(grepl("<br>", tip, fixed = TRUE) && grepl("EUR", tip, fixed = TRUE), "tooltip is more than one line")
say(!grepl("% pts", tip, fixed = TRUE), "the distance is in points, not a percent sign")

low <- fpa_chart_be_tip(fit, -1.05, 0)
say(grepl("does not cover", low, fixed = TRUE), "a price under the variable cost says no volume recovers it")

options(fpa.lang = "fr")
fr <- fpa_chart_be_tip(fit, 0.01, 0.02)
say(grepl("Résultat d'exploitation", fr, fixed = TRUE) && grepl("avant les pertes", fr, fixed = TRUE), "French tooltip carries the same reading")
options(fpa.lang = "en")

sub <- fpa_chart_expert_sub(pnl, bs, status, scenarios, "be")
say(grepl("9 closed months", sub, fixed = TRUE), "the caption counts the closed months")
say(grepl(fpa_chart_num(fit$F), sub, fixed = TRUE), "the caption uses the fitted fixed cost")

if (requireNamespace("plotly", quietly = TRUE)) {
  fig <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "be", FALSE))
  types <- vapply(fig$x$data, function(tr) as.character(tr$type %||% ""), "")
  say(any(types == "surface") && any(types == "scatter3d"), "break-even is a Plotly surface plus a 3D point")
  surface <- fig$x$data[[which(types == "surface")[2]]]
  say(isTRUE(as.numeric(surface$opacity) < 0.55), "the operating-income pane is translucent")
  cross <- Filter(function(tr) identical(as.character(tr$name), "be-zero-cross"), fig$x$data)
  on_income <- function(tr) {
    xs <- as.numeric(unlist(tr$x))
    ys <- as.numeric(unlist(tr$y))
    zs <- as.numeric(unlist(tr$z))
    if (!grepl("f7f1df", paste(unlist(tr$line$color), collapse = ""), ignore.case = TRUE)) return(FALSE)
    if (all(abs(xs) < 1e-8) && diff(range(ys)) > 0) {
      expect <- vapply(ys, function(q) fpa_chart_oi(fit, 0, q), numeric(1))
    } else if (all(abs(ys) < 1e-8) && diff(range(xs)) > 0) {
      expect <- vapply(xs, function(px) fpa_chart_oi(fit, px, 0), numeric(1))
    } else {
      return(FALSE)
    }
    length(zs) > 2 && max(abs(zs - expect)) < 1 && any(abs(zs) > 1)
  }
  say(length(cross) == 2L && all(vapply(cross, on_income, logical(1))), "the cross follows the dotted income surface at 0% price and 0% volume")
  point_text <- unlist(lapply(fig$x$data, function(tr) if (identical(tr$type, "scatter3d")) tr$text else NULL))
  say(any(grepl("Operating income", point_text, fixed = TRUE)) ||
        (length(surface$text) > 20 && any(grepl("Operating income", unlist(surface$text), fixed = TRUE))),
      "the surface points carry the long tooltip")
  dup <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "dupont", FALSE))
  ter <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "terrain", FALSE))
  cube <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "cube", FALSE))
  sky <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "sky", FALSE))
  cloud <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "cloud", FALSE))
  say(!is.null(dup) && !is.null(ter) && !is.null(cube) && !is.null(sky) && !is.null(cloud), "the other five charts build")
  sky_types <- vapply(sky$x$data, function(tr) as.character(tr$type %||% ""), "")
  say(any(sky_types == "mesh3d"), "the skyline is a Plotly mesh")
  cash_faces <- paste(unlist(sky$x$data[[1]]$facecolor), collapse = " ")
  say(grepl("8fd4e2|143, 212, 226|143,212,226", cash_faces, ignore.case = TRUE), "cash bars use the light cyan")
  attendu <- cloud$x$data[[which(vapply(cloud$x$data, function(tr) identical(as.character(tr$name), "Expected"), logical(1)))[[1]]]]
  attendu_line <- paste(unlist(attendu$line$color), collapse = " ")
  say(grepl("#ffffff|255, 255, 255|255,255,255", attendu_line, ignore.case = TRUE), "the expected path is a white line")
  cloud_text <- unlist(lapply(cloud$x$data, function(tr) tr$text))
  say(any(grepl("Against expected", cloud_text, fixed = TRUE)), "scenario points compare with expected")
  cloud_y <- as.character(unlist(cloud$x$layout$scene$yaxis$ticktext))
  say(
    length(cloud_y) >= 2 && length(cloud_y) <= 6 && all(grepl("%", cloud_y)) &&
      any(grepl("[0-9][.][0-9] %", cloud_y)) && !any(grepl("^[+-]?[0-9]+[.][0-9]+$", cloud_y)),
    "net margin ticks are percentages to 0.1"
  )
  dup_x <- as.character(unlist(dup$x$layout$scene$xaxis$ticktext))
  say(length(dup_x) >= 2 && all(grepl("%", dup_x)), "DuPont net margin ticks are percentages")
  modes <- vapply(cloud$x$data, function(tr) as.character(if (is.null(tr$mode)) "" else tr$mode), "")
  nms <- vapply(cloud$x$data, function(tr) as.character(if (is.null(tr$name)) "" else tr$name), "")
  line_names <- nms[modes == "lines+markers"]
  say(length(line_names) > 0 && identical(line_names[[length(line_names)]], "Expected"), "expected is drawn on top of the other paths")
  text_labs <- unlist(lapply(seq_along(cloud$x$data), function(i) if (identical(modes[[i]], "text")) cloud$x$data[[i]]$text else NULL))
  say(any(text_labs == "Expected"), "the expected path carries its name")
  say(any(text_labs %in% c("Jan", "Feb", "Mar")), "month labels sit on the expected path")
  path <- NULL
  for (tr in cube$x$data) {
    if (identical(as.character(tr$type), "scatter3d") && identical(as.character(tr$mode), "lines+markers")) path <- tr
  }
  xr <- as.numeric(cube$x$layout$scene$xaxis$range)
  xs <- as.numeric(unlist(path$x))
  say(
    !is.null(path) && length(xr) == 2 && diff(xr) < diff(range(xs)) * 2.2 + 1,
    "the cash-cycle axes wrap the path"
  )
  sky_cash <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "sky", FALSE, sky = "CASH"))
  sky_cash_types <- vapply(sky_cash$x$data, function(tr) as.character(tr$type %||% ""), "")
  sky_cash_groups <- vapply(sky_cash$x$data, function(tr) as.character(if (is.null(tr$legendgroup)) "" else tr$legendgroup), "")
  say(sum(sky_cash_types == "mesh3d" & sky_cash_groups == "CASH") == 1, "one skyline series can be drawn alone")
  full_z <- as.numeric(sky$x$layout$scene$zaxis$range)
  cash_z <- as.numeric(sky_cash$x$layout$scene$zaxis$range)
  say(length(cash_z) == 2 && cash_z[[2]] < full_z[[2]] * 0.75, "hiding the tall lines rescales the skyline")
  xt <- as.character(unlist(fig$x$layout$scene$xaxis$ticktext))
  yt <- as.character(unlist(fig$x$layout$scene$yaxis$ticktext))
  say(length(xt) >= 3 && all(grepl("%", xt)) && all(grepl("%", yt)), "price and volume ticks are percentages")
  faces <- paste(unlist(lapply(sky$x$data, function(tr) tr$facecolor)), collapse = " ")
  say(grepl("d5e6ea", faces, ignore.case = TRUE), "a translucent wall divides assets from funding")
  act <- fpa_chart_actual(pnl, status)
  per <- as.character(pnl$periode)[act & fpa_chart_col(pnl, "REV") > 0]
  marks <- data.frame(
    periode = per,
    nuitees_vendues = seq(40, by = 18, length.out = length(per)),
    tarif = seq(55, by = 11, length.out = length(per)),
    stringsAsFactors = FALSE
  )
  pts <- fpa_chart_be_points(pnl, status, marks)
  say(!is.null(pts) && length(pts$per) == length(per), "every closed month with a price and a volume is a point")
  marked <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "be", FALSE, marks = marks))
  mark_names <- vapply(marked$x$data, function(tr) as.character(if (is.null(tr$name)) "" else tr$name), "")
  say(!any(c("Highest volumes", "Lowest volumes", "Highest prices", "Lowest prices") %in% mark_names), "the extreme pairs are not legend switches")
  month_trace <- NULL
  for (tr in marked$x$data) {
    if (identical(as.character(tr$mode), "markers+text") && length(tr$x) == length(pts$per)) month_trace <- tr
  }
  say(!is.null(month_trace) && isTRUE(month_trace$showlegend == FALSE), "month dots sit on the surface without a legend switch")
  say(identical(as.character(unlist(month_trace$text)), fpa_chart_mon(pts$per)), "each dot is labelled with its month")
  one <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "be", FALSE, marks = marks, be = pts$per[[1]]))
  one_n <- sum(vapply(one$x$data, function(tr) {
    if (!identical(as.character(tr$mode), "markers+text")) return(0)
    length(tr$x)
  }, numeric(1)))
  say(one_n == 1, "a chosen month is the only dot")
  none <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "be", FALSE, marks = marks, be = character()))
  none_n <- sum(vapply(none$x$data, function(tr) {
    if (!identical(as.character(tr$mode), "markers+text")) return(0)
    length(tr$x)
  }, numeric(1)))
  say(none_n == 0, "clearing the months removes the dots")
  say(tolower(as.character(unlist(month_trace$marker$color))[[1]]) == "#3ec4ff", "closed months share one bright blue dot")
  fc_per <- c("2026-10", "2026-11", "2026-12")
  marks_fc <- rbind(marks, data.frame(
    periode = fc_per, nuitees_vendues = c(82, 46, 39), tarif = c(98, 84, 90),
    stringsAsFactors = FALSE
  ))
  pts_fc <- fpa_chart_be_points(pnl, status, marks_fc)
  fc_at <- match(fc_per, pts_fc$per)
  say(all(!is.na(fc_at)) && all(pts_fc$forecast[fc_at]), "October to December are forecast points")
  say(abs(pts_fc$n0 - pts$n0) < 1e-9 && abs(pts_fc$r0 - pts$r0) < 1e-9, "forecast months stay off the closed-month average")
  say(fpa_chart_fit(pnl, status)$n == length(per), "the surface still rests on the closed months")
  drawn <- plotly::plotly_build(fpa_chart_expert_plot(pnl, bs, status, scenarios, "be", FALSE, marks = marks_fc))
  dot_cols <- unlist(lapply(drawn$x$data, function(tr) {
    if (!identical(as.character(tr$mode), "markers+text")) return(character())
    tolower(as.character(tr$marker$color[[1]]))
  }))
  say(all(c("#3ec4ff", "#ffffff") %in% dot_cols), "forecast dots are white and closed dots are bright blue")
  be_named <- fpa_chart_math("be", pnl, bs, status, scenarios, pts_fc$per[[1]], marks_fc)
  say(
    !any(grepl("forecast|prévision", be_named$labels, ignore.case = TRUE)) &&
      isTRUE(be_named$forecast[[match("2026-10", be_named$periods)]]),
    "a forecast month keeps a short name and a forecast flag"
  )
  say(identical(fpa_screen("chart_st_hist"), "history") && identical(fpa_screen("chart_st_fc"), "forecast"), "history and forecast are the two row captions")
  be_math <- fpa_chart_math("be", pnl, bs, status, scenarios, pts$per[[1]], marks)
  say(length(be_math$gloss) == length(be_math$eqs) && length(be_math$gloss) == 3, "each break-even equation has a reading line")
  say(grepl(fpa_chart_num(pts$rate[[1]]), be_math$gloss[[1]], fixed = TRUE) && grepl("month's rate", be_math$gloss[[1]], fixed = TRUE), "the price line names the month's rate")
  say(grepl("average nights", be_math$gloss[[2]], fixed = TRUE) && grepl("fixed cost", be_math$gloss[[3]], fixed = TRUE), "volume and income name their numbers")
  dup_math <- fpa_chart_math("dupont", pnl, bs, status, scenarios, NULL)
  say(length(dup_math$gloss) == 3 && grepl("net income", dup_math$gloss[[1]], fixed = TRUE), "each DuPont equation names its numbers")
  say(length(dup_math$defs) == 3 && length(dup_math$eqs) == 3, "DuPont shows three axis definitions and three equations")
  say(all(grepl("=", dup_math$eqs, fixed = TRUE)), "each DuPont equation ends at the chart number")
  cube_math <- fpa_chart_math("cube", pnl, bs, status, scenarios, dup_math$periods[[1]])
  say(length(cube_math$eqs) == 3 && all(grepl("days", cube_math$eqs, fixed = TRUE)), "the cash-cycle month has three day equations")
  sky_math <- fpa_chart_math("sky", pnl, bs, status, scenarios, NULL)
  say(any(grepl("Assets =", sky_math$eqs, fixed = TRUE)) && any(grepl("Liabilities and equity =", sky_math$eqs, fixed = TRUE)), "the skyline month adds the two sides")
  cloud_math <- fpa_chart_math("cloud", pnl, bs, status, scenarios, NULL)
  cloud_groups <- fpa_chart_eq_groups("cloud", cloud_math$eqs)
  say(length(cloud_groups) == 3 && all(lengths(cloud_groups) == 2), "each scenario keeps margin and cash together")
  say(all(grepl("Expected", cloud_groups[[1]], fixed = TRUE)), "the first pair is the expected case")
  dup_groups <- fpa_chart_eq_groups("dupont", dup_math$eqs)
  say(length(dup_groups) == 1 && length(dup_groups[[1]]) == 3, "DuPont keeps its three equations in one block")
  jan <- c(
    "Prix = 79 / 112 \u2212 1 = -29,7 %",
    "Volume = 35 / 109 \u2212 1 = -68,0 %",
    "Resultat = 10 833 \u00d7 0,70 \u00d7 0,32 \u2212 7 913 = -5 475 EUR"
  )
  mai <- c(
    "Prix = 108 / 112 \u2212 1 = -3,7 %",
    "Volume = 125 / 109 \u2212 1 = +14,3 %",
    "Resultat = 10 833 \u00d7 0,96 \u00d7 1,14 \u2212 7 913 = 4 015 EUR"
  )
  marked <- fpa_chart_eq_marks(jan, mai)
  hot_text <- function(pieces) paste(vapply(pieces, function(p) if (isTRUE(p$hot)) p$text else "", ""), collapse = " ")
  hot_style <- function(pieces, bit) {
    hit <- Filter(function(p) isTRUE(p$hot) && grepl(bit, p$text, fixed = TRUE), pieces)
    if (!length(hit)) "" else hit[[1]]$style
  }
  say(grepl("35", hot_text(marked$left[[2]]), fixed = TRUE) && grepl("125", hot_text(marked$right[[2]]), fixed = TRUE), "the volume driver is marked in both months")
  say(!grepl("112", hot_text(marked$left[[1]]), fixed = TRUE), "the shared denominator stays plain")
  say(!grepl("475", hot_text(marked$left[[3]]), fixed = TRUE) && !grepl("015", hot_text(marked$right[[3]]), fixed = TRUE), "the equation result stays plain")
  en_may <- "Price = 108 / 112 \u2212 1 = -3.7 %"
  en_sep <- "Price = 120 / 112 \u2212 1 = +7.2 %"
  en_tok <- fpa_chart_num_toks(en_may)
  say(length(en_tok) == 4 && en_tok[[4]]$text == "-3.7 %" && abs(en_tok[[4]]$value + 3.7) < 1e-9, "a dot percent is one number")
  en_hot <- hot_text(fpa_chart_eq_marks(en_may, en_sep)$left[[1]])
  say(!grepl("-3", en_hot, fixed = TRUE) && grepl("108", en_hot, fixed = TRUE), "the price gap is not clipped, and the rate keeps the highlight")
  oi_en <- c(
    "Income = 10 833 \u00d7 0.96 \u00d7 1.14 \u2212 7 913 = 4 015 EUR",
    "Income = 10 833 \u00d7 1.07 \u00d7 1.15 \u2212 7 913 = 5 466 EUR"
  )
  oi_tok <- fpa_chart_num_toks(oi_en[[1]])
  say(any(vapply(oi_tok, function(t) identical(t$text, "0.96"), logical(1))), "a dot factor is one number")
  oi_hot <- hot_text(fpa_chart_eq_marks(oi_en[[1]], oi_en[[2]])$left[[1]])
  say(grepl("0.96", oi_hot, fixed = TRUE) && !grepl(" 0 ", paste0(" ", oi_hot, " "), fixed = TRUE), "the factor highlight covers 0.96")
  dup_l <- c(
    "Marge nette = 1 240 / 11 000 = 11,3 %",
    "Rotation des actifs = 11 000 / 137 555 = 0,08",
    "Rentabilite = 1 240 / 81 240 = 1,5 %"
  )
  dup_r <- c(
    "Marge nette = -1 999 / 4 882 = -40,9 %",
    "Rotation des actifs = 4 882 / 151 757 = 0,03",
    "Rentabilite = -1 999 / 96 751 = -2,1 %"
  )
  dup_m <- fpa_chart_eq_marks(dup_l, dup_r)
  say(grepl("1 240", hot_text(dup_m$left[[1]]), fixed = TRUE) && grepl("-1 999", hot_text(dup_m$right[[1]]), fixed = TRUE), "net income is a driver in both months")
  say(identical(hot_style(dup_m$left[[1]], "1 240"), hot_style(dup_m$right[[1]], "-1 999")) && nzchar(hot_style(dup_m$left[[1]], "1 240")), "a driver keeps one gold across the two months")
  say(!grepl("11,3", hot_text(dup_m$left[[1]]), fixed = TRUE) && !grepl("40,9", hot_text(dup_m$right[[1]]), fixed = TRUE), "the margin result stays plain")
  say(!grepl("137", hot_text(dup_m$left[[2]]), fixed = TRUE), "the third driver stays plain")
} else {
  say(FALSE, "plotly is installed")
}

cat("FAILS ", fail, "\n", sep = "")
quit(status = if (fail) 1L else 0L)
