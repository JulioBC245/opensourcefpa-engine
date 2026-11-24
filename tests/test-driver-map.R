# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Driver map: measured cells stay measured, gaps stay gaps, no euro is invented.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

g <- data.frame(
  series_id = c("a", "b", "c"),
  series_name = c("beta", "alpha", "gamma"),
  score = c(0.2, 0.2, NA),
  independence_r = c(-0.4, 0.9, 0.5),
  independence_status = c("measured", "not_available", "measured"),
  impact_eur = c(0, -100, 50),
  impact_status = c("engine", "not_available", "not_available"),
  target_line = c("REV", "REV", "PAYROLL"),
  causal_lag = c(0, NA, -2),
  lag_status = c("suggested", "not_distinguishable", "refused_reverse_causality"),
  evidence_r = c(0.2, NA, 0.5),
  flag = c("", "identite", "sens inverse"),
  stringsAsFactors = FALSE
)

score <- fpa_driver_map_heat(g, "score")
# tie on 0.2 breaks by name: alpha before beta. gamma has no score and comes last.
stopifnot(identical(score$series_id, c("b", "a", "c")))
stopifnot(identical(score$series_name, c("alpha", "beta", "gamma")))
stopifnot(identical(score$state, c("value", "value", "missing")))
stopifnot(is.na(score$value[[3]]))

partial <- fpa_driver_map_heat(g, "partial")
# b is not_available even though a number sits in the column. It must not rank.
stopifnot(identical(partial$series_id, c("c", "a", "b")))
stopifnot(identical(partial$state[[3]], "missing"))
stopifnot(is.na(partial$value[[3]]))
stopifnot(partial$value[[1]] == 0.5)

euro <- fpa_driver_map_heat(g, "euro")
stopifnot(identical(euro$series_id[[1]], "a"))
stopifnot(euro$value[[1]] == 0)
stopifnot(identical(euro$state, c("value", "missing", "missing")))
stopifnot(all(is.na(euro$value[euro$state == "missing"])))

stopifnot(identical(fpa_driver_map_flags(g), c("identite", "sens inverse")))
blank <- g
blank$flag <- c(NA, NA, NA)
stopifnot(length(fpa_driver_map_flags(blank)) == 0L)
blank$flag <- c("", "", "")
stopifnot(length(fpa_driver_map_flags(blank)) == 0L)
stopifnot(length(fpa_driver_map_heat(g[0, , drop = FALSE], "score")$series_id) == 0L)

links <- fpa_driver_map_layout(fpa_driver_map_links(g))
stopifnot(length(unique(links$y_left)) == 3L)
stopifnot(sum(links$euro_state == "value") == 1L)
stopifnot(links$euro[links$series_id == "a"] == 0)
stopifnot(all(is.na(links$euro[links$euro_state == "missing"])))
# PAYROLL's right node sits on gamma, REV's sits between alpha and beta.
stopifnot(links$y_right[links$series_id == "c"] == links$y_left[links$series_id == "c"])

cand <- data.frame(
  series_id = c("nights", "price"),
  series_name = c("nuitees_vendues", "tarif"),
  stringsAsFactors = FALSE
)
vol <- fpa_driver_map_volume(cand, data.frame(series_id = "nights", series_name = "Nuitées", stringsAsFactors = FALSE))
stopifnot(identical(vol$id, "nights"))
stopifnot(identical(vol$name, "Nuitées"))
stopifnot(is.na(fpa_driver_map_volume(cand[0, , drop = FALSE])$id))

stopifnot(identical(fpa_driver_map_edge_tag(0.991, NA, "missing"), "r +0,99"))
stopifnot(identical(fpa_driver_map_edge_tag(-0.576, -4158.0592, "value"), "r -0,58 · -4 158 EUR"))
stopifnot(identical(fpa_driver_map_edge_tag(NA, NA, "missing"), ""))
stopifnot(identical(fpa_driver_map_edge_tag(NA, 0, "value"), "0 EUR"))
line_euro <- fpa_driver_map_line_euro(fpa_driver_map_links(g))
stopifnot(line_euro[["REV"]] == 0)
stopifnot(is.na(line_euro[["PAYROLL"]]))

ink <- fpa_driver_map_score_ink(0.9)
stopifnot(identical(ink$fg, "#ffffff"))
stopifnot(identical(fpa_driver_map_score_ink(NA)$bg, "#f4f7f8"))
stopifnot(identical(fpa_driver_map_signed_ink(-10, 10)$fg, "#ffffff"))

r_best <- fpa_driver_map_r_ink(0.98)
r_worst <- fpa_driver_map_r_ink(-0.33)
r_miss <- fpa_driver_map_r_ink(NA)
stopifnot(identical(r_best$fg, "#ffffff"))
stopifnot(identical(fpa_driver_map_r_ink(-0.98)$bg, r_best$bg))
stopifnot(identical(r_worst$fg, "#8a969c"))
stopifnot(identical(r_miss$bg, "#f4f7f8"))
# |r| near 1 is greener than |r| near 0.3; weak stays near gray
stopifnot(r_best$bg != r_worst$bg)
stopifnot(grepl("^#[cdef]", r_worst$bg))
stopifnot(grepl("^#[12]", r_best$bg))

hotel_g <- "../fpa-local-test/work/drivers/driver-grades.csv"
hotel_c <- "../fpa-local-test/work/drivers/driver-candidates.csv"
if (file.exists(hotel_g) && file.exists(hotel_c)) {
  hg <- utils::read.csv(hotel_g, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  hc <- utils::read.csv(hotel_c, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  hv <- fpa_driver_map_volume(hc, hg)
  stopifnot(identical(hv$id, "src-ops_other__nuitees_vendues"))
  he <- fpa_driver_map_heat(hg, "euro")
  engine <- as.character(hg$impact_status) == "engine" & is.finite(suppressWarnings(as.numeric(hg$impact_eur)))
  stopifnot(sum(he$state == "value") == sum(engine))
  stopifnot(identical(fpa_driver_map_cell_label("euro", "value", 4229.4), "4 229 EUR"))
  gap_ids <- c("src-ops_other__capacite", "src-5__temperature_c", "src-5__precipitations_mm")
  stopifnot(all(he$state[he$series_id %in% gap_ids] == "missing"))
  stopifnot(identical(fpa_driver_map_cell_label("euro", "missing", NA), "Pas chiffré"))
  stopifnot(identical(fpa_driver_map_cell_label("partial", "missing", 0.5), "Non mesuré"))
  stopifnot(identical(fpa_driver_map_score_note("prior"), "prior"))
  stopifnot(identical(fpa_driver_map_score_note("mix"), ""))
  stopifnot(all(is.na(he$value[he$state == "missing"])))
  hs <- fpa_driver_map_heat(hg, "score")
  stopifnot(all(hs$tier[abs(hs$value - 0.85) < 1e-9] == "mechanism"))
  stopifnot(all(hs$score_kind[abs(hs$value - 0.85) < 1e-9] == "prior"))
  stopifnot(all(hs$score_kind[hs$series_id %in% c("src-5__temperature_c", "src-5__precipitations_mm")] == "held"))
  stopifnot(hs$score_kind[hs$series_id == "src-ops_other__effectif"] == "held")
  stopifnot(hs$score_kind[hs$series_id == "src-ops_other__tarif"] == "mix")
  hp <- fpa_driver_map_heat(hg, "partial")
  stopifnot(hp$series_id[[1]] == "src-ops_other__tarif")
  stopifnot(identical(fpa_driver_map_cell_label("partial", "value", hp$value[[1]]), "+0.99"))
  hlinks <- fpa_driver_map_links(hg)
  hline <- fpa_driver_map_line_euro(hlinks)
  stopifnot(all(is.na(hline)))
  cm_edge <- hlinks[hlinks$series_id == "src-ops_other__cout_moyen", , drop = FALSE]
  stopifnot(!grepl("EUR", fpa_driver_map_edge_tag(cm_edge$r, cm_edge$euro, cm_edge$euro_state), fixed = TRUE))
  cap <- hlinks[hlinks$series_id == "src-ops_other__capacite", , drop = FALSE]
  cap_tag <- fpa_driver_map_edge_tag(cap$r, cap$euro, cap$euro_state)
  stopifnot(identical(cap_tag, ""))
  stopifnot(hp$state[hp$series_id == "src-ops_other__nuitees_vendues"] == "missing")
  stopifnot(hp$state[hp$series_id == "src-ops_other__capacite"] == "missing")
}

message("PASS driver-map")
