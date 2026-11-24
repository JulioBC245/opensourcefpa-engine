# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Freeze, vintage compare, summary read back, standing lock.
source("tests/helpers.R", encoding = "UTF-8")
root <- fpa_test_bootstrap()

tmp <- tempfile("fpa-freeze-")
dir.create(file.path(tmp, "work"), recursive = TRUE)
dir.create(file.path(tmp, "reports"), recursive = TRUE)
paths <- list(work = file.path(tmp, "work"), reports = file.path(tmp, "reports"))
budget <- data.frame(periode = "2024-01", functional_label = "REV", montant = 10)
utils::write.csv(budget, file.path(paths$work, "budget-monthly.csv"), row.names = FALSE)
manifest <- fpa_freeze_budget(paths)
stopifnot(file.exists(file.path(paths$work, "vintages", "budget-frozen.csv")))
stopifnot(identical(manifest$md5, fpa_md5_file(file.path(paths$work, "vintages", "budget-frozen.csv"))))

pnl <- data.frame(periode = c("2024-01", "2024-02"), REV = c(10, 12), OI = c(4, 5), NI = c(3, 4))
utils::write.csv(pnl, file.path(paths$work, "pnl-monthly.csv"), row.names = FALSE)
v1 <- fpa_save_vintage(paths, "one")
pnl$OI <- c(4, 8)
utils::write.csv(pnl, file.path(paths$work, "pnl-monthly.csv"), row.names = FALSE)
Sys.sleep(1.1)
v2 <- fpa_save_vintage(paths, "two")
cmp <- fpa_compare_vintages(paths, v1$id, v2$id)
stopifnot(abs(cmp$delta_oi[[2]] - 3) < 1e-9)
summary <- fpa_export_summary(paths, v2$id)
stopifnot(file.exists(file.path(paths$reports, paste0("summary-", v2$id, ".csv"))))
stopifnot(abs(summary$oi - 12) < 1e-9)

locked <- data.frame(periode = "2024-01", line_id = "REV", montant = 99)
utils::write.csv(locked, file.path(paths$work, "standing-lock.csv"), row.names = FALSE)
frame <- data.frame(periode = c("2024-01", "2024-02"), REV = c(10, 12), COGS = c(1, 1),
                    OTHER_OP_INC = 0, PAYROLL = 0, OPEX_OTHER = 0, D_A = 0, FIN = 0, EXCEP = 0, TAX = 0)
held <- fpa_apply_standing_lock(frame, paths)
stopifnot(abs(held$REV[[1]] - 99) < 1e-9)
stopifnot(abs(held$REV[[2]] - 12) < 1e-9)

message("PASS freeze")
