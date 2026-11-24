# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Split driver pair: history Jan-Jun, recent file May-Jul.
# The joined file keeps Jan-Apr from history and May-Jul from the recent file.
# Run from fpa-local: Rscript tests/check-split-pair.R

root <- if (file.exists("R/fpa-drivers.R")) "." else ".."
source(file.path(root, "R", "fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-config.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-paths.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-lines.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-read.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-validate.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-drivers.R"), encoding = "UTF-8")
source(file.path(root, "R", "fpa-profile.R"), encoding = "UTF-8")

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

tmp <- tempfile("split-pair-")
dir.create(tmp)
work <- file.path(tmp, "work")
dir.create(work)
paths <- list(work = work, root = tmp, input = file.path(tmp, "input"))

months <- function(from, to) {
  format(seq(as.Date(paste0(from, "-01")), as.Date(paste0(to, "-01")), by = "month"), "%Y-%m")
}
hist_months <- months("2026-01", "2026-06")
roll_months <- months("2026-05", "2026-07")

hist <- data.frame(
  periode = hist_months,
  nuitees = 1:6,
  origine = paste0("h", 1:6),
  stringsAsFactors = FALSE
)
roll <- data.frame(
  periode = roll_months,
  nuitees = c(50, 60, 70),
  frais = c(5, 6, 7),
  stringsAsFactors = FALSE
)
only <- data.frame(periode = hist_months, nuitees = 11:16, stringsAsFactors = FALSE)
live <- data.frame(periode = roll_months, nuitees = c(8, 9, 10), stringsAsFactors = FALSE)
jour_h <- data.frame(jour = c("2026-01-01", "2026-01-02"), visiteurs = c(1, 2), stringsAsFactors = FALSE)
jour_r <- data.frame(jour = c("2026-01-02", "2026-01-03"), visiteurs = c(20, 30), stringsAsFactors = FALSE)

hist_csv <- file.path(tmp, "history.csv")
roll_csv <- file.path(tmp, "rolling.csv")
only_csv <- file.path(tmp, "history-only.csv")
live_csv <- file.path(tmp, "living.csv")
jour_h_csv <- file.path(tmp, "jour-h.csv")
jour_r_csv <- file.path(tmp, "jour-r.csv")
utils::write.csv(hist, hist_csv, row.names = FALSE)
utils::write.csv(roll, roll_csv, row.names = FALSE)
utils::write.csv(only, only_csv, row.names = FALSE)
utils::write.csv(live, live_csv, row.names = FALSE)
utils::write.csv(jour_h, jour_h_csv, row.names = FALSE)
utils::write.csv(jour_r, jour_r_csv, row.names = FALSE)

config <- list(
  sources = list(
    list(source_id = "src-h", source_name = "Passe", file_role = "ops_other",
         feed_shape = "split", pair_id = "pair-nuit", pair_role = "history"),
    list(source_id = "src-r", source_name = "Suivi", file_role = "ops_other",
         feed_shape = "split", pair_id = "pair-nuit", pair_role = "rolling"),
    list(source_id = "src-only", source_name = "Seul", file_role = "ops_other",
         feed_shape = "history_only"),
    list(source_id = "src-live", source_name = "Vivant", file_role = "ops_other",
         feed_shape = "living"),
    list(source_id = "src-jh", source_name = "Jours passes", file_role = "ops_other",
         feed_shape = "split", pair_id = "pair-jour", pair_role = "history"),
    list(source_id = "src-jr", source_name = "Jours suivis", file_role = "ops_other",
         feed_shape = "split", pair_id = "pair-jour", pair_role = "rolling")
  ),
  extra_files = list(
    list(source_id = "src-h", path = hist_csv),
    list(source_id = "src-r", path = roll_csv),
    list(source_id = "src-only", path = only_csv),
    list(source_id = "src-live", path = live_csv),
    list(source_id = "src-jh", path = jour_h_csv),
    list(source_id = "src-jr", path = jour_r_csv)
  )
)

groups <- fpa_driver_groups(config)
by_key <- stats::setNames(groups, vapply(groups, function(g) g$key, character(1)))
split <- fpa_read_driver_group(config, by_key[["pair-nuit"]], paths)
only_g <- fpa_read_driver_group(config, by_key[["src-only"]], paths)
live_g <- fpa_read_driver_group(config, by_key[["src-live"]], paths)
jour_g <- fpa_read_driver_group(config, by_key[["pair-jour"]], paths)

joined_path <- file.path(work, "driver-pair-nuit.csv")
check("the joined csv is written", file.exists(joined_path) && identical(split$files, joined_path))

disk <- utils::read.csv(joined_path, stringsAsFactors = FALSE, na.strings = c("", "NA"))
check("seven months, Jan through Jul", identical(disk$periode, months("2026-01", "2026-07")))
check("Jan-Apr nuitees come from history", identical(as.numeric(disk$nuitees[1:4]), as.numeric(1:4)))
check("May-Jul nuitees come from the recent file", identical(as.numeric(disk$nuitees[5:7]), c(50, 60, 70)))
check("May is not the history value", !identical(disk$nuitees[[5]], 5))
check("a history-only column is kept", identical(disk$origine[1:4], paste0("h", 1:4)))
check("the history-only column is empty on recent months", all(is.na(disk$origine[5:7])))
check("a recent-only column is kept", identical(as.numeric(disk$frais[5:7]), c(5, 6, 7)))
check("the recent-only column is empty on history months", all(is.na(disk$frais[1:4])))
check("the csv has no engine clock column", !".clock" %in% names(disk))

same_periods <- identical(split$correlation$periode, split$rolling$periode)
same_values <- identical(split$correlation$nuitees, split$rolling$nuitees)
check("correlation and rolling read the same joined months", same_periods && same_values)
check("both frames keep the recent May value", identical(as.numeric(split$rolling$nuitees[[5]]), 50))

check("history_only is not joined", identical(only_g$files, only_csv) && is.null(only_g$rolling))
check("history_only keeps its six months", identical(as.numeric(only_g$correlation$nuitees), as.numeric(11:16)))
check("a living file stays one file", identical(live_g$files, live_csv))
check("a living file is not written as a pair csv", !file.exists(file.path(work, "driver-src-live.csv")))
check("living correlation and rolling are that one file",
      identical(as.numeric(live_g$correlation$nuitees), c(8, 9, 10)) &&
        identical(as.numeric(live_g$rolling$nuitees), c(8, 9, 10)))

check("a jour column is a period", identical(jour_g$correlation$jour, c("2026-01-01", "2026-01-02", "2026-01-03")))
check("the repeated day keeps the recent value", identical(as.numeric(jour_g$rolling$visiteurs), c(1, 20, 30)))

check("periode is found", identical(fpa_driver_period_col(data.frame(periode = "2026-01")), "periode"))
check("an accented periode is found", identical(fpa_driver_period_col(data.frame(période = "2026-01", check.names = FALSE)), "période"))
check("date is found", identical(fpa_driver_period_col(data.frame(date = "2026-01-01")), "date"))
check("day is found", identical(fpa_driver_period_col(data.frame(day = "2026-01-01")), "day"))

prof <- fpa_profile_drivers(config, paths)
sid <- fpa_profile_series_id("src-r", "nuitees")
series <- prof$series[[sid]]
check("correlation profiles the rolling source once", !is.null(series) && is.null(prof$series[[fpa_profile_series_id("src-h", "nuitees")]]))
if (!is.null(series)) {
  got_months <- format(series$dates, "%Y-%m")
  check("the profiled series runs Jan through Jul", identical(got_months, months("2026-01", "2026-07")))
  check("the profiled May value is the recent file", identical(as.numeric(series$values[[5]]), 50))
} else {
  check("the profiled series runs Jan through Jul", FALSE)
  check("the profiled May value is the recent file", FALSE)
}

bare <- data.frame(nuitees = 1:3, note = c("a", "b", "c"), stringsAsFactors = FALSE)
bare_h <- file.path(tmp, "bare-h.csv")
bare_r <- file.path(tmp, "bare-r.csv")
utils::write.csv(bare, bare_h, row.names = FALSE)
utils::write.csv(bare, bare_r, row.names = FALSE)
bare_cfg <- list(
  sources = list(
    list(source_id = "b-h", file_role = "ops_other", feed_shape = "split",
         pair_id = "pair-bare", pair_role = "history"),
    list(source_id = "b-r", file_role = "ops_other", feed_shape = "split",
         pair_id = "pair-bare", pair_role = "rolling")
  ),
  extra_files = list(
    list(source_id = "b-h", path = bare_h),
    list(source_id = "b-r", path = bare_r)
  )
)
notes <- fpa_driver_pair_findings(bare_cfg, paths)
check("a pair with no period column is reported", any(vapply(notes, function(n) identical(n$code, "driver_pair_no_period"), logical(1))))
check("that sentence is the French copy", any(vapply(notes, function(n) identical(n$message, fpa_txt("drv_split_no_period")), logical(1))))
check("no joined file is written without a period", !file.exists(file.path(work, "driver-pair-bare.csv")))

cat("\n", pass, "passed,", fail, "failed\n", sep = " ")
if (fail > 0L) quit(status = 1L)
