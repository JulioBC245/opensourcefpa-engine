# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Living file: a month already in the file is kept. Only a missing open month is filled.
# Run from fpa-local: Rscript tests/check-living-clock.R

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

df <- data.frame(
  periode = c("2025-09", "2026-01", "2026-06", "2026-07", "2026-08", "2026-10"),
  nuitees = c(19, 10, 60, 70, 80, NA),
  stringsAsFactors = FALSE
)
open <- c("2026-07", "2026-08", "2026-09", "2026-10")
clocked <- fpa_living_clock(df, "2026-06", open)

hist <- clocked$history
fc <- clocked$forecast
check("the clock runs", isTRUE(clocked$ok))
check("closed months are Jan and Jun 2026 plus Sep 2025",
      identical(hist$periode, c("2025-09", "2026-01", "2026-06")))
check("July stays the typed 70", identical(as.numeric(fc$nuitees[fc$periode == "2026-07"]), 70))
check("August stays the typed 80", identical(as.numeric(fc$nuitees[fc$periode == "2026-08"]), 80))
check("July and August are marked kept",
      identical(fc$`.origin`[fc$periode %in% c("2026-07", "2026-08")], c("kept", "kept")))
check("September was missing and is filled from Sep 2025",
      identical(as.numeric(fc$nuitees[fc$periode == "2026-09"]), 19))
check("September is marked filled", identical(fc$`.origin`[fc$periode == "2026-09"], "filled"))
check("a blank October is not kept", !"kept" %in% fc$`.origin`[fc$periode == "2026-10"])
check("October has no prior year, so it is absent", !"2026-10" %in% fc$periode)
check("the filled list is September only", identical(clocked$filled, "2026-09"))
check("correlation history does not include the typed future",
      !any(hist$periode %in% c("2026-07", "2026-08", "2026-09")))

vol <- c("2025-09" = 19, "2026-06" = 60, "2026-07" = 70)
filled_vol <- fpa_living_fill_vector(vol, "2026-06", open)
check("a present open month in the volume vector stays 70", identical(filled_vol[["2026-07"]], 70))
check("a missing open month takes last year", identical(filled_vol[["2026-09"]], 19))
check("a missing month with no last year is not invented", !"2026-10" %in% names(filled_vol))

tmp <- tempfile("living-")
dir.create(tmp)
work <- file.path(tmp, "work")
dir.create(work)
paths <- list(work = work)
live_csv <- file.path(tmp, "living.csv")
only_csv <- file.path(tmp, "only.csv")
utils::write.csv(df, live_csv, row.names = FALSE)
utils::write.csv(data.frame(periode = c("2026-01", "2026-02"), nuitees = c(1, 2)), only_csv, row.names = FALSE)
config <- list(
  sources = list(
    list(source_id = "src-live", source_name = "Suivi", file_role = "ops_other", feed_shape = "living"),
    list(source_id = "src-only", source_name = "Passe", file_role = "ops_other", feed_shape = "history_only")
  ),
  extra_files = list(
    list(source_id = "src-live", path = live_csv),
    list(source_id = "src-only", path = only_csv)
  )
)
read <- fpa_read_drivers(config, paths)
clocked_groups <- fpa_drivers_apply_clock(read, "2026-06", open)
live <- clocked_groups$groups[[1]]
only <- clocked_groups$groups[[2]]
check("the living forecast keeps July", identical(as.numeric(live$rolling$nuitees[live$rolling$periode == "2026-07"]), 70))
check("the living correlation stops at the last closed month",
      identical(live$correlation$periode, c("2025-09", "2026-01", "2026-06")))
check("the subscriber file is not rewritten", identical(read$groups[[1]]$files, live_csv))
check("history_only is not filled", is.null(only$filled) && identical(as.numeric(only$correlation$nuitees), c(1, 2)))

prof <- fpa_profile_drivers(config, paths, closed_through = "2026-06")
sid <- fpa_profile_series_id("src-live", "nuitees")
series <- prof$series[[sid]]
got <- if (is.null(series)) character(0) else format(series$dates, "%Y-%m")
check("the measure uses closed months only", identical(got, c("2025-09", "2026-01", "2026-06")))

check("the filled sentence names the month",
      identical(fpa_txt("drv_living_filled", "2026-09"),
                "Ces mois n'étaient pas dans le fichier. Le même mois de l'année précédente a été repris : 2026-09."))

cat("\n", pass, "passed,", fail, "failed\n", sep = " ")
if (fail > 0L) quit(status = 1L)
