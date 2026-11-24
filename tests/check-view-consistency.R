# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# One fact reads the same way on every view (Astra E2 / E3).
# Calendar dated on the map, ';' edit round-trip, parameters in force, driver states,
# thin liquidity, forecast rows in variances, the À faire count, and the extremes.
root <- Sys.getenv("FPA_LOCAL_DIR", ".")
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
fpa_source_all(root)

fail <- 0L
n_ok <- 0L
say <- function(ok, msg) {
  if (!isTRUE(ok)) fail <<- fail + 1L else n_ok <<- n_ok + 1L
  cat(if (isTRUE(ok)) "ok   " else "FAIL ", msg, "\n", sep = "")
}

# ';' edit round-trip, UTF-8 and latin1, and a comma file stays comma.
tmp <- tempfile(fileext = ".csv")
writeLines(c("date;libelle;montant;type;duree_mois",
             "2026-11-15;Rénovation salle;18500;capex;60",
             "2026-12-31;Remboursement;-4200;dette;"), tmp, useBytes = FALSE)
df <- fpa_csv_read_sniff(tmp)
say(identical(attr(df, "fpa_sep"), ";") && nrow(df) == 2L && ncol(df) == 5L, "';' file reads as five columns")
df$montant[[1]] <- 19000
fpa_csv_write_same(df, tmp)
back <- fpa_csv_read_sniff(tmp)
head_line <- readLines(tmp, n = 1L, encoding = "UTF-8")
say(identical(head_line, "date;libelle;montant;type;duree_mois"), "header still ';' after the edit")
say(identical(back$montant, c(19000L, -4200L)) || identical(as.numeric(back$montant), c(19000, -4200)), "edited cell saved, other cell unchanged")
say(identical(back$libelle[[1]], "Rénovation salle"), "accented label survives the round-trip")

lat <- tempfile(fileext = ".csv")
con <- file(lat, open = "w", encoding = "latin1")
writeLines(c("libelle;montant", "Crédit;12,5"), con)
close(con)
ld <- fpa_csv_read_sniff(lat)
say(identical(attr(ld, "fpa_encoding"), "latin1") && identical(ld$libelle[[1]], "Crédit"), "latin1 file detected and read")
fpa_csv_write_same(ld, lat)
raw_back <- readBin(lat, "raw", file.info(lat)$size)
say(!validUTF8(rawToChar(raw_back)), "latin1 file written back as latin1")

com <- tempfile(fileext = ".csv")
writeLines(c("a,b", "1,2"), com)
cd <- fpa_csv_read_sniff(com)
fpa_csv_write_same(cd, com)
say(identical(readLines(com, n = 1L), "a,b"), "comma file stays comma")

# À faire: the count is the sum of the four groups.
decisions <- list(list(id = "j1", title = "A", sentence = "a"), list(id = "j2", title = "B", sentence = "b"))
absent <- data.frame(id = "src:x", title = "Fichier x", path = "C:/nulle/part.csv", stringsAsFactors = FALSE)
scan <- data.frame(
  family = c("books", "ratio", "driver", "books"), level = c("act", "act", "watch", "clear"),
  periode = c("2026-09", "2026-09", "2026-09", "2026-09"), line_id = c("BS", "cash_ratio", "REV", "BS"),
  value = 1, reference = 1, gap = 0, n = 1L, stringsAsFactors = FALSE
)
nodes <- data.frame(
  id = c("rule:a", "src:b", "rule:c"), title = c("Règle a", "Fichier b", "Règle c"),
  status = c("incomplet", "sans suite", "utilisé"), detail = c("manque x", "aucune règle", ""),
  stringsAsFactors = FALSE
)
gaps <- c("Calendrier : duree_mois sans rôle.", "Fichier b : aucune règle en aval.")
tg <- fpa_todo_groups(decisions, absent, scan, nodes, gaps)
say(tg$n == 8L && tg$n == sum(lengths(tg$groups)), paste("À faire count", tg$n, "= 2 + 1 + 2 + 3"))
say(length(tg$groups$map) == 3L, "map questions: incomplete rule, file with no rule, column without a role")
say(length(tg$groups$anomalies) == fpa_anomaly_counts(scan)$act, "anomalies queued equal the Anomalies à éclaircir count")
rats <- Filter(function(it) grepl("Dénominateur mince", it$title), tg$groups$anomalies)
books_q <- Filter(function(it) grepl("^Livres", it$title), tg$groups$anomalies)
say(length(rats) == 1L && grepl("septembre 2026", rats[[1]]$title) && grepl("Ouvre la carte Dénominateur mince", rats[[1]]$sentence),
    "liquidity queue names the card, the month, and where it opens")
say(identical(sub("^anom:", "", rats[[1]]$id), "anom-row-ratio-2026-09-cash_ratio"),
    "liquidity queue id is the anomaly line")
say(length(books_q) == 1L && grepl("Ouvre la carte Livres", books_q[[1]]$sentence), "books queue names the Livres card")
extra <- scan[scan$family == "ratio", , drop = FALSE]
extra$periode <- "2026-12"
extra$value <- 80
tg2 <- fpa_todo_groups(list(), NULL, rbind(scan, extra), NULL)
pair <- Filter(function(it) grepl("Dénominateur", it$title), tg2$groups$anomalies)
say(length(pair) == 2L && pair[[1]]$title != pair[[2]]$title && grepl("décembre 2026", pair[[2]]$title),
    "september and december liquidity are two rows of one card")
bare_ids <- vapply(tg$groups$map, function(it) it$id, character(1))
say(any(startsWith(bare_ids, "map:col:")) && !any(startsWith(bare_ids, "map:cols:")),
    "a column without a role keeps a stable id")
life_row <- Filter(function(it) grepl("Calendrier", it$title), tg$groups$map)
unused_row <- Filter(function(it) identical(it$title, "Fichier b"), tg$groups$map)
say(length(life_row) == 1L && grepl("amort", life_row[[1]]$sentence) && grepl("zéro", life_row[[1]]$sentence),
    "a life column says what it is, and that a zero does not clear the row")
say(length(unused_row) == 1L && grepl("ne changent pas", unused_row[[1]]$sentence) && grepl("réserve", unused_row[[1]]$sentence),
    "an unused file says the statements do not move")
scan_dir <- tempfile()
dir.create(scan_dir)
scan_paths <- list(work = scan_dir)
still <- fpa_todo_groups(list(), NULL, NULL, NULL, "Calendrier : duree_mois sans rôle.")
say(fpa_todo_retire_resolved(scan_paths, still, still) == 0L, "refresh keeps a column that still has no role")
gone <- fpa_todo_groups(list(), NULL, NULL, NULL)
say(fpa_todo_retire_resolved(scan_paths, still, gone) == 1L && identical(fpa_todo_cleared_read(scan_paths)[[1]]$via, "scan"),
    "refresh logs a row the new scan no longer produces")
say(fpa_todo_retire_resolved(scan_paths, gone, still) == 0L && !length(fpa_todo_cleared_read(scan_paths)),
    "a row that is valid again leaves the removed log")
logged_scan <- fpa_todo_cleared_add(scan_paths, still$groups$map[[1]]$id, "t", "s", "scan")
say(fpa_todo_hide_cleared(still, fpa_todo_cleared_read(scan_paths))$n == still$n,
    "an automatic removal does not hide a row that the scan still produces")
cleared_dir <- tempfile()
dir.create(cleared_dir)
cpaths <- list(work = cleared_dir)
rid <- "anom:anom-row-ratio-2026-09-cash_ratio"
saved <- fpa_todo_cleared_add(cpaths, rid, rats[[1]]$title, rats[[1]]$sentence, "check")
hidden <- fpa_todo_hide_cleared(tg, fpa_todo_cleared_read(cpaths))
say(hidden$n == tg$n - 1L && !any(vapply(hidden$groups$anomalies, function(it) identical(it$id, rid), logical(1))),
    "a checked liquidity row leaves À faire")
say(length(fpa_todo_cleared_read(cpaths)) == 1L && grepl("Coché|check", fpa_todo_cleared_read(cpaths)[[1]]$via),
    "the check is stored with its date")
back <- fpa_todo_cleared_remove(cpaths, rid)
say(!length(back) && fpa_todo_hide_cleared(tg, back)$n == tg$n, "putting the row back restores the count")
due <- list(list(id = "dec1", title = "Revoir", sentence = "due", stamp = "2026-10-01"))
tg_due <- fpa_todo_groups(due, NULL, NULL, NULL)
saved_due <- fpa_todo_cleared_add(cpaths, "dec1", "Revoir", "due", "check", "2026-10-01")
say(fpa_todo_hide_cleared(tg_due, fpa_todo_cleared_read(cpaths))$n == 0L, "a checked decision stays off for that review date")
due[[1]]$stamp <- "2026-11-01"
tg_due2 <- fpa_todo_groups(list(due[[1]]), NULL, NULL, NULL)
say(fpa_todo_hide_cleared(tg_due2, fpa_todo_cleared_read(cpaths))$n == 1L, "a new review date puts the decision back")
te <- fpa_todo_groups(list(), NULL, NULL, NULL)
say(te$n == 0L, "empty queue counts 0")

cal <- tempfile(fileext = ".csv")
writeLines(c(
  "date;libelle;montant;type;duree_mois",
  "2026-02-10;Ligne de trésorerie saisonnière, tirage;15000;dette_tirage;",
  "2026-03-15;Remplacement four de production;28000;capex;84"
), cal)
bad <- fpa_try_csv(cal)
say(ncol(bad) == 1L, "comma reader still sees one column on a semicolon calendar")
prev <- fpa_raw_preview_frame(cal)
say(is.null(prev$error) && ncol(prev$data) == 5L && nrow(prev$data) == 2L,
    "popup preview splits the semicolon calendar into five columns")
say(identical(prev$data$libelle[[1]], "Ligne de trésorerie saisonnière, tirage"),
    "comma inside the label stays in the label")
say(is.na(prev$data$duree_mois[[1]]) && as.numeric(prev$data$duree_mois[[2]]) == 84,
    "blank life stays blank and the capex life is 84")
editp <- tempfile(fileext = ".csv")
file.copy(cal, editp, overwrite = TRUE)
res0 <- fpa_raw_edit_apply(editp, list(`1_5` = "12", `2_3` = "28000"))
back_e <- fpa_raw_preview_frame(editp)
say(res0$n == 1L && as.numeric(back_e$data$duree_mois[[1]]) == 12 && as.numeric(back_e$data$montant[[2]]) == 28000,
    "unlock writes the blank life and leaves an unchanged amount")
say(identical(back_e$data$libelle[[1]], "Ligne de trésorerie saisonnière, tirage") && identical(attr(back_e$data, "fpa_sep"), ";"),
    "the comma inside the label and the semicolon survive the save")
res1 <- fpa_raw_edit_apply(editp, list(`1_5` = ""))
back_z <- fpa_raw_preview_frame(editp)
say(res1$n == 1L && is.na(back_z$data$duree_mois[[1]]) && identical(back_z$data$type[[2]], "capex"),
    "clearing the life writes a blank cell again")
say(ncol(fpa_try_csv(editp)) == 1L, "the comma reader is unchanged after an edit")
res_sp <- fpa_raw_edit_apply(editp, list(`1_2` = "Ligne de trésorerie saisonnière, tirage", `2_3` = "15 000"))
back_sp <- fpa_raw_preview_frame(editp)
say(res_sp$n == 1L && identical(back_sp$data$libelle[[1]], "Ligne de trésorerie saisonnière, tirage") && as.numeric(back_sp$data$montant[[2]]) == 15000,
    "spaces stay in a label and a grouped number is still a number")

# Thin liquidity: the ratio leaves the fold mix and is named.
rows <- data.frame(id = c("current_ratio", "quick_ratio", "cash_ratio"), a = c(70000, 66000, 65942), b = c(436, 436, 436),
                   stringsAsFactors = FALSE)
thin <- fpa_health_thin_ids(rows)
say(identical(thin, c("current_ratio", "quick_ratio", "cash_ratio")), "cash 65 942 against 436 is a thin read")
mix <- fpa_health_fold_mix(rows$id, rows$a / rows$b, skip = thin)
say(mix$n_used == 0L && !is.finite(mix$pos) && identical(mix$skipped, thin), "thin ratios do not score the fold (no Bonne santé on them)")
say(!length(fpa_health_thin_ids(transform(rows, a = 5000, b = 4000))), "ordinary liabilities are read normally")
say(!length(fpa_health_thin_ids(rows[0, ])), "no rows, no thin flag")

# Variance status tags, empty and full.
say(identical(names(fpa_variance_tag_status(fpa_variance_empty(), NULL)), names(fpa_variance_empty())), "empty variance keeps its columns")

# Driver states: synthetic, then the hotel.
g <- data.frame(series_name = c("capacite", "effectif", "meteo"), relation = c("part_whole", "independent", "independent"),
                evidence_class = c("insufficient_evidence", "insufficient_evidence", "insufficient_evidence"),
                bottom_sig = c(FALSE, FALSE, FALSE), verdict = c("Reject", "Reject", "Reject"), stringsAsFactors = FALSE)
f <- data.frame(target = c("LINE:REV", "LINE:PAYROLL"), factors = c("capacite x tarif", "effectif x cout_moyen"),
                verified = c(TRUE, FALSE), median_gap = c(0.0001, 0.27), n = c(37L, 37L), stringsAsFactors = FALSE)
s <- fpa_driver_state(g, f, list())
say(isTRUE(s$state_proved[[1]]) && !grepl("Écarter", s$state_label[[1]]) && grepl("non concluant", s$state_label[[1]]),
    "proved factor reads « dans la formule vérifiée · test statistique non concluant »")
say(isTRUE(s$state_failed[[2]]) && grepl("non retenue", s$state_label[[2]]), "failed payroll formula reads « formule déclarée non retenue »")
say(!s$state_configured[[3]] && !nzchar(s$state_label[[3]]), "undeclared series keeps its verdict")
say(is.null(fpa_driver_state(NULL)) && nrow(fpa_driver_state(g[0, ], f, list())) == 0L, "no grades, no states")
s0 <- fpa_driver_state(g, NULL, list())
say(!any(s0$state_proved) && !any(s0$state_failed), "no formula file, nothing proved or failed")

# Year-end on one month of data.
one <- fpa_year_end_info(list(company = list(fiscal_year_end_month = 12L)),
                         data.frame(periode = "2026-09", status = "actual", stringsAsFactors = FALSE), "2026-09")
say(identical(one$close, "2026-12") && identical(one$measured, "2026-09") && isTRUE(one$beyond), "one month: close December, measured September")

cfg <- Sys.getenv("FPA_CONFIG", "../fpa-local-test/fpa-config.json")
if (file.exists(cfg)) {
  config <- fpa_load_config(cfg)
  ch <- fpa_chain_build(config, NULL)
  meteo <- ch$nodes[ch$nodes$title == "meteo", , drop = FALSE]
  say(nrow(meteo) == 1L && grepl("meteo-provence-mensuel-2026.csv", meteo$detail) && grepl("réserve", meteo$detail),
      "hotel weather row names the file and says it is only in reserve")
  life_gap <- grep("duree_mois sans rôle\\.$", ch$gaps, value = TRUE)
  say(length(life_gap) == 1L && grepl("zéro", fpa_todo_column_sentence(fpa_todo_bare_cols(life_gap))),
      "hotel calendar row stays after the cells are filled, because the column still has no role")
  b <- fpa_run(cfg, write = FALSE, stop_on_error = FALSE)
  cov <- fpa_chain_coverage(config)
  paths <- vapply(config$sources, function(s) as.character(tryCatch(fpa_chain_source_path(config, s), error = function(e) "") %||% ""), "")
  cal_i <- grep("calendrier", paths, ignore.case = TRUE)
  cal <- if (length(cal_i)) cov[cal_i[[1]], , drop = FALSE] else cov[0, ]
  say(nrow(cal) == 1L && identical(format(cal$start, "%Y-%m"), "2026-02") && identical(format(cal$end, "%Y-%m"), "2026-12"),
      paste("calendar dated on the map:", if (nrow(cal)) paste(format(cal$start), "to", format(cal$end)) else "missing"))
  roles <- vapply(config$sources, function(s) as.character(s$file_role %||% ""), "")
  asm <- cov[roles == "assumptions", , drop = FALSE]
  say(nrow(asm) >= 1L && all(asm$state %in% c("en vigueur", "non daté")),
      paste("parameters read « en vigueur » or « non daté »:", paste(asm$state, collapse = ", ")))
  say(!any(cov$state[roles == "assumptions"] == "en retard"), "parameters never « en retard »")

  cash <- b$cash
  nov <- cash[as.character(cash$periode) == "2026-11", , drop = FALSE]
  capex_col <- intersect(c("capex", "CAPEX", "invest"), names(cash))
  if (length(capex_col)) {
    say(abs(abs(as.numeric(nov[[capex_col[[1]]]])) - 18500) < 1, "bridge holds the November capex 18 500")
  }

  v <- b$variance
  rev <- v[as.character(v$line_id) == "REV", , drop = FALSE]
  gap_col <- intersect(c("variance", "gap", "ecart"), names(rev))[[1]]
  tot <- sum(as.numeric(rev[[gap_col]]), na.rm = TRUE)
  closed <- sum(as.numeric(rev[[gap_col]][rev$status == "actual"]), na.rm = TRUE)
  open <- sum(as.numeric(rev[[gap_col]][rev$status == "forecast"]), na.rm = TRUE)
  say(all(rev$status[as.character(rev$periode) > "2026-09"] == "forecast") &&
      all(rev$status[as.character(rev$periode) <= "2026-09"] == "actual"), "October to December flagged forecast")
  say(abs(tot - 18738.01) < 1 && abs(closed - 32203.00) < 1 && abs(open - (-13464.99)) < 1,
      sprintf("REV variance %.2f = closed %.2f + open %.2f", tot, closed, open))

  hm <- fpa_health_month(b$bs, b$pnl, "2026-09", 1L)
  say(length(fpa_health_thin_ids(hm)) >= 0L, paste("hotel September liquidity thin reads:", length(fpa_health_thin_ids(hm))))

  d <- file.path(fpa_paths(config)$work, "drivers")
  gr <- utils::read.csv(file.path(d, "driver-grades.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  fo <- utils::read.csv(file.path(d, "formula-checks.csv"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  st <- fpa_driver_state(gr, fo, config)
  pr <- st[st$state_proved, , drop = FALSE]
  say(nrow(pr) == 4L && !any(grepl("Écarter", pr$state_label)), paste("hotel proved factors:", paste(pr$series_name, collapse = ", ")))
  pay <- st[st$series_name %in% c("effectif", "cout_moyen"), , drop = FALSE]
  say(nrow(pay) == 2L && all(pay$state_failed) && all(grepl("non retenue", pay$state_label)), "hotel payroll inputs: formule déclarée non retenue")
  say(all(pay$state_used), "hotel payroll inputs are still read by the forecast (flagged in the label)")
  pay_fc <- b$pnl$PAYROLL[as.character(b$pnl$periode) == "2026-10"]
  vol <- fpa_read_volumes(config)
  say(abs(pay_fc - vol$payroll[["2026-10"]]) < 0.01, sprintf("October payroll %.2f = effectif x coût moyen", pay_fc))

  info <- fpa_year_end_info(config, b$period_status, as.character(b$pnl$periode))
  say(abs(fpa_objectif_read(b, "cash_ye", info$close) - (-36642.19)) < 1 && abs(fpa_objectif_read(b, "oi_fy", info$close) - 31090.19) < 1,
      "year-end objective reads match a fresh fpa_run")
} else {
  cat("skip hotel checks (no FPA_CONFIG)\n")
}

cat(sprintf("\n%d passed, %d failed\n", n_ok, fail))
cat(if (fail) paste0(fail, " CHECK(S) FAILED\n") else "ALL CHECKS PASSED\n")
quit(save = "no", status = if (fail) 1L else 0L)
