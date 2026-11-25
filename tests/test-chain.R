# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# Configuration map: links follow enabled mechanisms and straight file reads.
source("tests/helpers.R", encoding = "UTF-8")
fpa_test_bootstrap()

stopifnot(fpa_chain_has_keys(c("charge.fixe.loyer", "dso"), c("charge.fixe", "dso")))
stopifnot(!fpa_chain_has_keys(c("dso"), "taux.impot"))
stopifnot(identical(fpa_chain_frame_label("history"), "Historique"))
stopifnot(identical(fpa_chain_frame_label("plan"), "Plan"))

bare <- list(
  mechanisms = list(effective_tax_rate = TRUE, capacity_x_util_x_rate = FALSE),
  assumption_keys = list(list(id = "taux.impot", enabled = TRUE)),
  sources = list()
)
bare_map <- fpa_chain_build(bare, evidence = NULL)
stopifnot(any(bare_map$nodes$id == "rule:effective_tax_rate"))
stopifnot(!any(bare_map$nodes$id == "rule:capacity_x_util_x_rate"))
stopifnot(!bare_map$read)
stopifnot(any(bare_map$nodes$status == "incomplet"))

ev <- data.frame(
  line_id = c("REV", "TAX"),
  evidence_label = c("imported_actual", "mechanical_rule"),
  stringsAsFactors = FALSE
)
stopifnot(identical(fpa_chain_used_lines(ev), "TAX"))
stopifnot(isTRUE(fpa_chain_has_import(ev)))

opt <- fpa_optional_data_dir()
if (!is.null(opt) && file.exists(file.path(opt, "fpa-config.json"))) {
  hotel <- file.path(opt, "fpa-config.json")
  long <- file.path(opt, "work", "pnl-monthly-long.csv")
  cfg <- fpa_load_config(hotel)
  evh <- if (file.exists(long)) utils::read.csv(long, stringsAsFactors = FALSE, fileEncoding = "UTF-8") else NULL
  ch <- fpa_chain_build(cfg, evh)
  meteo <- ch$nodes[ch$nodes$title == "meteo", , drop = FALSE]
  stopifnot(nrow(meteo) == 1L)
  stopifnot(identical(meteo$status, "sans suite"))
  stopifnot(nrow(fpa_chain_absent_sources(cfg)) == 0L)
  watched <- fpa_config_source_paths(cfg)
  stopifnot(any(grepl("meteo-provence-mensuel-2026.csv", watched, fixed = TRUE)))
  stopifnot(any(grepl("pnl-mensuel.xlsx", watched, fixed = TRUE)))
  stopifnot(any(grepl("volumes-mensuel-2026.csv", watched, fixed = TRUE)))
  ann <- ch$nodes[ch$nodes$id == "rule:read:annual", , drop = FALSE]
  stopifnot(nrow(ann) == 1L)
  stopifnot(grepl("douze mois", ann$detail))
  cap <- ch$nodes[ch$nodes$id == "rule:capacity_x_util_x_rate", , drop = FALSE]
  stopifnot(nrow(cap) == 1L)
  stopifnot(cap$status %in% c("relié", "utilisé", "partagé"))
  touch <- fpa_chain_touch(ch, "rule:capacity_x_util_x_rate")
  stopifnot("out:pnl" %in% touch)
  stopifnot(any(grepl("^src:", touch)))
  # Weather must not be pulled onto the capacity path.
  stopifnot(!any(touch == meteo$id))
  pnl <- ch$nodes[ch$nodes$id == "out:pnl", , drop = FALSE]
  cash <- ch$nodes[ch$nodes$id == "out:cash", , drop = FALSE]
  stopifnot(identical(pnl$status, "écrit"))
  stopifnot(identical(cash$status, "non vérifié"))
} else {
  fpa_notify_skip_optional_data("hotel chain checks")
}

stopifnot(identical(fpa_chain_status_bits(data.frame(status = c("présent", "présent", "sans suite"))), "2 présent · 1 sans suite"))

miss <- list(
  sources = list(list(
    source_id = "gone", source_name = "Nouveau csv", file_role = "ops_other", time_frame = "rolling"
  )),
  paths = list(ops_other = "C:/FPA-Local/input/does-not-exist.csv")
)
miss_map <- fpa_chain_build(miss, evidence = NULL)
gone <- miss_map$nodes[miss_map$nodes$id == "src:gone", , drop = FALSE]
stopifnot(nrow(gone) == 1L)
stopifnot(identical(gone$status, "absent"))
stopifnot(identical(gone$path, "C:/FPA-Local/input/does-not-exist.csv"))
abs_miss <- fpa_chain_absent_sources(miss)
stopifnot(nrow(abs_miss) == 1L)
stopifnot(identical(abs_miss$title, "Nouveau csv"))
stopifnot(identical(abs_miss$path, "C:/FPA-Local/input/does-not-exist.csv"))
stopifnot(any(grepl("introuvable", miss_map$gaps)))
blank <- list(sources = list(list(
  source_id = "nop", source_name = "Sans chemin", file_role = "ops_other"
)))
blank_map <- fpa_chain_build(blank, evidence = NULL)
nop <- blank_map$nodes[blank_map$nodes$id == "src:nop", , drop = FALSE]
stopifnot(identical(nop$status, "absent"))
stopifnot(!nzchar(nop$path))

cov_cfg <- list(sources = list(
  list(
    source_id = "old", source_name = "Suivi ancien", file_role = "ops_other", time_frame = "rolling",
    periods = list(list(period_end = "2024-06-01"))
  ),
  list(
    source_id = "year", source_name = "Annuel", file_role = "pnl_actuals", time_frame = "history",
    periods = list(list(period_end = "2025-12-31", grain = "year"))
  )
))
cov <- fpa_chain_coverage(cov_cfg, today = as.Date("2026-09-22"))
stopifnot(identical(cov$state[cov$id == "src:old"], "en retard"))
stopifnot(identical(cov$state[cov$id == "src:year"], "à jour"))
stopifnot(identical(as.character(cov$start[cov$id == "src:year"]), "2025-01-01"))
stopifnot(identical(as.character(cov$end[cov$id == "src:year"]), "2025-12-31"))

snap_cfg <- list(sources = list(
  list(
    source_id = "bs", source_name = "Bilan d'ouverture", file_role = "opening_bs",
    periods = list()
  ),
  list(
    source_id = "m", source_name = "Mensuel", file_role = "pnl_monthly",
    periods = list(list(period_end = "2026-01-31", grain = "month"))
  )
))
snap <- fpa_chain_coverage(snap_cfg, today = as.Date("2026-09-23"))
stopifnot(identical(snap$state[snap$id == "src:bs"], "instantané"))
stopifnot(identical(as.character(snap$start[snap$id == "src:bs"]), "2026-01-01"))
stopifnot(cov$months_back[cov$id == "src:old"] > 20)

# Wide monthly book: declared through December, but Oct-Dec are blank zeros.
wide_path <- file.path(tempdir(), "fpa-mensuel-wide.csv")
utils::write.csv(
  data.frame(
    Poste = c("REV", "COGS"),
    Janvier = c(10, 1), Fevrier = c(10, 1), Mars = c(10, 1), Avril = c(10, 1),
    Mai = c(10, 1), Juin = c(10, 1), Juillet = c(10, 1), Aout = c(10, 1),
    Septembre = c(10, 1), Octobre = c(0, 0), Novembre = c(0, 0), Decembre = c(0, 0),
    stringsAsFactors = FALSE
  ),
  wide_path, row.names = FALSE
)
stopifnot(identical(fpa_chain_month_col_index("Septembre (EUR)"), 9L))
wide_dates <- fpa_chain_wide_month_dates(wide_path, 2026L)
stopifnot(identical(format(min(wide_dates), "%Y-%m"), "2026-01"))
stopifnot(identical(format(max(wide_dates), "%Y-%m"), "2026-09"))
wide_cfg <- list(
  paths = list(pnl_monthly = wide_path),
  sources = list(list(
    source_id = "mensuel", source_name = "Compte de resultat mensuel",
    file_role = "pnl_monthly", time_frame = "rolling",
    periods = lapply(1:12, function(m) {
      list(period_end = format(fpa_chain_month_end(2026L, m), "%Y-%m-%d"), grain = "month")
    })
  ))
)
wide_cov <- fpa_chain_coverage(wide_cfg, today = as.Date("2026-09-28"))
mensuel <- wide_cov[wide_cov$id == "src:mensuel", , drop = FALSE]
stopifnot(nrow(mensuel) == 1L)
stopifnot(identical(format(mensuel$end, "%Y-%m"), "2026-09"))
stopifnot(isTRUE(mensuel$ahead_pct < 0.05))
stopifnot(identical(mensuel$state, "à jour"))

rulers <- fpa_chain_rulers(cov, 12L, as.Date("2026-09-22"))
stopifnot(identical(rulers$label[rulers$kind == "civil"], c("2024", "2025", "2026")))
stopifnot(identical(rulers$label[rulers$kind == "fiscal"], c("2024", "2025", "2026")))
fy <- fpa_chain_rulers(cov, 3L, as.Date("2026-09-22"))
stopifnot(identical(fy$full[fy$kind == "fiscal"], c("2024-2025", "2025-2026", "2026-2027")))
stopifnot(abs(fy$left_pct[fy$full == "2025-2026"][[1]] - rulers$left_pct[rulers$label == "2025" & rulers$kind == "civil"][[1]]) > 1)

# A YYYY-MM row covers the month through its last day, so a suivi file
# reaches the same December close as a voted plan. The plan stays one colour.
vol_path <- file.path(tempdir(), "fpa-volumes-ym.csv")
utils::write.csv(
  data.frame(periode = c("2023-09", "2026-12"), nuitees = c(10, 39), stringsAsFactors = FALSE),
  vol_path, row.names = FALSE
)
vol_dates <- fpa_chain_file_dates(vol_path)
stopifnot(as.Date("2026-12-01") %in% vol_dates)
stopifnot(as.Date("2026-12-31") %in% vol_dates)
span_cfg <- list(
  paths = list(ops_other = vol_path),
  sources = list(
    list(
      source_id = "vol", source_name = "Volumes", file_role = "ops_other",
      time_frame = "rolling", periods = list()
    ),
    list(
      source_id = "bud", source_name = "Budget", file_role = "budget",
      time_frame = "plan",
      periods = list(list(period_end = "2026-12-31", grain = "year"))
    )
  )
)
span_cov <- fpa_chain_coverage(span_cfg, today = as.Date("2026-09-28"))
vol <- span_cov[span_cov$id == "src:vol", ]
bud <- span_cov[span_cov$id == "src:bud", ]
stopifnot(identical(as.character(vol$end), "2026-12-31"))
stopifnot(identical(as.character(bud$end), "2026-12-31"))
stopifnot(isTRUE(vol$ahead_pct > 1))
stopifnot(isTRUE(bud$ahead_pct < 0.05))
lo <- min(c(span_cov$start, as.Date("2026-09-28")))
hi <- max(span_cov$end)
span_days <- as.numeric(hi - lo)
full <- as.numeric(bud$end - bud$start) / span_days * 100
clipped <- as.numeric(as.Date("2026-09-28") - bud$start) / span_days * 100
stopifnot(abs(bud$width_pct - full) < 0.05)
stopifnot(bud$width_pct > clipped + 1)
stopifnot(abs((bud$left_pct + bud$width_pct) - (vol$today_pct + vol$ahead_pct)) < 0.05)

message("PASS chain")
