# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Rolling path past fiscal year-end: 18 months, seasonal copy, cascade past month 12.
# Run from fpa-local: Rscript tests/check-rolling-horizon.R

source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

pass <- 0L
fail <- 0L
say <- function(ok, msg) {
  if (isTRUE(ok)) pass <<- pass + 1L else fail <<- fail + 1L
  cat(if (isTRUE(ok)) "PASS " else "FAIL ", msg, "\n", sep = "")
}

say(identical(fpa_horizon_months(list(horizon_months = 18), c("2026-09")), 18L), "numeric horizon is 18 months")
extra <- fpa_extend_periods(c("2026-09"), 18L)
say(identical(extra[[length(extra)]], "2028-03"), "18 months after Sep 2026 is Mar 2028")
say(identical(fpa_extend_periods(c("2026-09"), 12L)[[13]], "2027-09"), "12 months after Sep 2026 ends Sep 2027")
say(identical(fpa_extend_periods(c("2026-09"), 24L)[[25]], "2028-09"), "24 months after Sep 2026 ends Sep 2028")

df <- data.frame(
  periode = c("2025-10", "2025-11", "2025-12", "2026-01", "2026-02", "2026-03",
              "2026-04", "2026-05", "2026-06", "2026-07", "2026-08", "2026-09"),
  nuitees = c(10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21),
  stringsAsFactors = FALSE
)
open <- format(seq(as.Date("2026-10-01"), by = "month", length.out = 18), "%Y-%m")
clocked <- fpa_living_clock(df, "2026-09", open)
say(isTRUE(clocked$ok), "clock runs on an 18-month open list")
say(identical(as.numeric(clocked$forecast$nuitees[clocked$forecast$periode == "2026-10"]), 10),
    "Oct 2026 copies closed Oct 2025")
say(identical(as.numeric(clocked$forecast$nuitees[clocked$forecast$periode == "2027-09"]), 21),
    "Sep 2027 copies closed Sep 2026")
say(identical(as.numeric(clocked$forecast$nuitees[clocked$forecast$periode == "2027-10"]), 10),
    "Oct 2027 copies the filled Oct 2026")
say(identical(as.numeric(clocked$forecast$nuitees[clocked$forecast$periode == "2028-03"]), 15),
    "Mar 2028 copies the filled Mar 2027")
say(length(clocked$filled) == 18L, "all 18 open months are filled")

vec <- setNames(as.numeric(df$nuitees), df$periode)
filled_vec <- fpa_living_fill_vector(vec, "2026-09", open)
say(identical(filled_vec[["2027-10"]], 10), "vector fill cascades past month 12")

tmp <- tempfile("hz-")
dir.create(tmp)
cfg_path <- file.path(tmp, "fpa-config.json")
writeLines("{}", cfg_path)
config <- list(`.config_path` = cfg_path)
say(identical(fpa_horizon_months_read(config), 18L), "default rolling horizon is 18")
say(identical(fpa_horizon_months_write(config, 24L), 24L), "write accepts 24")
say(identical(fpa_horizon_months_read(config), 24L), "read back is 24")
say(identical(fpa_horizon_months_write(config, 7L), 18L), "invalid value falls back to 18")

cat("PASS ", pass, " FAIL ", fail, "\n", sep = "")
if (fail > 0L) quit(status = 1)
