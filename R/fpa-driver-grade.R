# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-driver-grade.R — Impact, Evidence, Independence, timing, verdict, artifacts.
# LAST CHANGE: 2026-09-30 (fpa_driver_state)
# Impact is an engine span. It is not available until a mechanism binds the series.
# Nothing is labelled statistically validated unless the permutation rank and the
# holdout both earn it. The engine never confirms a lag; a person does.

fpa_driver_data_budget <- function(n_months) {
  n <- as.integer(n_months %||% 0L)
  if (is.na(n) || n < 1L) {
    return(fpa_txt("loc_grade_short_budget"))
  }
  k <- max(1L, as.integer(round(n / 7)))
  paste0(
    "Vous avez ", n, fpa_txt("loc_grade_months_hist"), k,
    fpa_txt("loc_grade_confirmed"),
    fpa_txt("loc_grade_or_rules")
  )
}

#' causal_lag in months minus availability in days. Positive means it can forecast.
fpa_driver_usable_horizon <- function(causal_lag_months, availability_lag_days) {
  if (is.null(causal_lag_months) || length(causal_lag_months) != 1L ||
      is.na(causal_lag_months)) {
    return(NA_real_)
  }
  lag_days <- as.numeric(availability_lag_days %||% 0)
  if (is.na(lag_days)) lag_days <- 0
  as.numeric(causal_lag_months) * 30 - lag_days
}

fpa_driver_class <- function(usable_horizon_days, lag_status) {
  if (identical(lag_status, "refused_reverse_causality")) return("explanatory")
  if (is.na(usable_horizon_days)) return("explanatory")
  if (usable_horizon_days > 0) "forecasting" else "explanatory"
}

fpa_driver_evidence_class <- function(identity, n, permutation_rank, oos_skill,
                                      lag_status, evidence_sig) {
  if (identical(identity, "suspect_identity") || identical(identity, "part_whole")) {
    return("mechanically_calculated")
  }
  if (is.na(n) || n < 8L) return("insufficient_evidence")
  if (identical(lag_status, "refused_reverse_causality")) return("insufficient_evidence")
  earned <- isTRUE(evidence_sig) &&
    is.finite(permutation_rank) && permutation_rank <= 0.05 &&
    is.finite(oos_skill) && oos_skill > 0
  if (earned) return("statistically_validated")
  "insufficient_evidence"
}

fpa_driver_verdict <- function(n, evidence_sig, independence_sig, independence_status,
                               lag_status, evidence_class, driver_class, identity) {
  # A duplicate series is not a driver. A part-whole identity is a mechanism:
  # it is scored from its prior and is not failed for being certain.
  if (identical(identity, "part_whole")) return("Mechanism")
  if (identical(lag_status, "refused_reverse_causality")) return("Reject")
  if (identical(identity, "suspect_identity")) return("Reject")
  if (is.na(n) || n < 24L) return("Needs more history")
  keep <- identical(evidence_class, "statistically_validated") &&
    identical(driver_class, "forecasting") &&
    (isTRUE(independence_sig) || identical(independence_status, "not_available"))
  if (keep) return("Keep")
  if (isTRUE(evidence_sig)) return("Optional")
  "Reject"
}

#' One declared row from config$driver_bindings, or an empty spec.
#' relation, prior and lag_window_max are optional. An empty relation is filled
#' later when a product identity is detected.
fpa_driver_spec <- function(config, series_id) {
  binds <- config$driver_bindings %||% list()
  hit <- NULL
  for (b in binds) {
    if (identical(as.character(b$series_id %||% ""), as.character(series_id))) {
      hit <- b
      break
    }
  }
  prior <- suppressWarnings(as.numeric(if (is.null(hit)) NA else hit$prior %||% NA))
  window <- suppressWarnings(as.integer(if (is.null(hit)) 3L else hit$lag_window_max %||% 3L))
  if (!is.finite(window) || window < 0L) window <- 3L
  window <- min(as.integer(window), 3L)
  list(
    target_line = as.character(if (is.null(hit)) "" else hit$target_line %||% ""),
    relation = as.character(if (is.null(hit)) "" else hit$relation %||% ""),
    prior = if (length(prior) && is.finite(prior)) prior else NA_real_,
    lag_window_max = window,
    kind = as.character(if (is.null(hit)) "" else hit$kind %||% ""),
    bound = !is.null(hit)
  )
}

#' Column names the wizard declared with one kind (volume, rate, ...).
#' Empty when the config has no driver_bindings: callers fall back to names.
fpa_driver_bound_columns <- function(config, kind) {
  out <- character()
  for (b in config$driver_bindings %||% list()) {
    if (!identical(as.character(b$kind %||% ""), kind)) next
    col <- as.character(b$column %||% "")
    if (!nzchar(col)) col <- sub("^.*__", "", as.character(b$series_id %||% ""))
    if (nzchar(col)) out <- c(out, col)
  }
  unique(out)
}

#' Display names and formula membership declared by the wizard, keyed by column.
fpa_driver_bound_meta <- function(config) {
  labels <- character()
  formula <- character()
  kind <- character()
  for (b in config$driver_bindings %||% list()) {
    col <- as.character(b$column %||% "")
    if (!nzchar(col)) col <- sub("^.*__", "", as.character(b$series_id %||% ""))
    if (!nzchar(col)) next
    lab <- as.character(b$label %||% "")
    # A rule label is the column name reworded; only the assistant or the user names it.
    if (nzchar(lab) && as.character(b$origin %||% "") %in% c("ai", "user")) labels[[col]] <- lab
    f <- b$formula
    formula[[col]] <- if (is.null(f) || is.na(f)) "" else as.character(f)
    kind[[col]] <- as.character(b$kind %||% "")
  }
  list(labels = labels, formula = formula, kind = kind)
}

#' Driver score. An identity keeps its prior undamped. A measured independent
#' series is 0.75 evidence + 0.25 prior. An unmeasured series keeps 0.35 of its prior.
fpa_driver_score <- function(abs_r, prior, relation, measured) {
  pr <- if (is.finite(prior)) max(0, min(1, prior)) else 0
  rel <- as.character(relation %||% "independent")
  if (rel %in% c("part_whole", "derived")) return(pr)
  if (isTRUE(measured) && is.finite(abs_r)) return(0.75 * abs_r + 0.25 * pr)
  0.35 * pr
}

#' Default prior: a declared number wins. An identity with no number stays high.
fpa_driver_prior_value <- function(declared, relation) {
  if (is.finite(declared)) return(max(0, min(1, declared)))
  if (as.character(relation) %in% c("part_whole", "derived")) return(0.85)
  0.40
}

#' Mark both factors when their product reproduces another series or a P&L line.
#' Relative tolerance is 1e-3 of the scale of the product.
fpa_driver_product_ids <- function(monthly, lines = list()) {
  ids <- names(monthly)
  hit <- character(0)
  vecs <- c(monthly, lines)
  nms <- names(vecs)
  if (length(nms) < 3L) return(hit)
  close_enough <- function(prod, other) {
    ok <- is.finite(prod) & is.finite(other)
    if (sum(ok) < 8L) return(FALSE)
    scale <- max(abs(other[ok]))
    if (!is.finite(scale) || scale < 1e-8) return(FALSE)
    max(abs(prod[ok] - other[ok])) <= 1e-3 * scale
  }
  for (i in seq_along(ids)) {
    for (j in seq_along(ids)) {
      if (j <= i) next
      prod <- as.numeric(monthly[[ids[[i]]]]) * as.numeric(monthly[[ids[[j]]]])
      for (k in seq_along(nms)) {
        if (nms[[k]] %in% c(ids[[i]], ids[[j]])) next
        if (close_enough(prod, vecs[[nms[[k]]]])) {
          hit <- c(hit, ids[[i]], ids[[j]])
        }
      }
    }
  }
  unique(hit)
}

#' Confirmed is capped by round(n/7). Mechanisms are identities and stay out of
#' that cap. One measured series per tested line is taken before the rest fill by score.
fpa_driver_assign_tiers <- function(grades, history_n) {
  if (is.null(grades) || !nrow(grades)) return(grades)
  n <- nrow(grades)
  if (!"relation" %in% names(grades)) grades$relation <- "independent"
  if (!"prior" %in% names(grades)) grades$prior <- NA_real_
  grades$prior <- vapply(seq_len(n), function(i) {
    fpa_driver_prior_value(suppressWarnings(as.numeric(grades$prior[[i]])), grades$relation[[i]])
  }, numeric(1))
  # A measured link clears the r bar, beats chance on timing, and the series leads.
  beats_chance <- if ("permutation_rank" %in% names(grades)) {
    rank <- suppressWarnings(as.numeric(grades$permutation_rank))
    is.finite(rank) & rank <= 0.05
  } else rep(TRUE, n)
  leads <- !(as.character(grades$lag_status %||% "") %in% "refused_reverse_causality")
  measured <- !grades$relation %in% c("part_whole", "derived") &
    vapply(grades$evidence_sig, isTRUE, logical(1)) &
    is.finite(suppressWarnings(as.numeric(grades$evidence_r))) &
    beats_chance & leads
  eligible <- measured
  grades$score <- vapply(seq_len(n), function(i) {
    fpa_driver_score(
      abs(suppressWarnings(as.numeric(grades$evidence_r[[i]]))),
      grades$prior[[i]],
      grades$relation[[i]],
      measured[[i]]
    )
  }, numeric(1))
  grades$flag <- ifelse(
    grades$relation %in% c("part_whole", "derived"), "identite",
    ifelse(grades$lag_status == "refused_reverse_causality", "sens inverse",
           ifelse(nzchar(grades$redundancy_group), "doublon",
                  ifelse(grades$driver_class == "explanatory" & measured, "arrive tard", "")))
  )
  k <- max(1L, as.integer(round(as.numeric(history_n) / 7)))
  grades$tier <- "watch"
  grades$tier[grades$relation %in% c("part_whole", "derived")] <- "mechanism"
  pool <- which(eligible & grades$tier != "mechanism")
  chosen <- integer(0)
  if (length(pool)) {
    lines <- unique(grades$target_line[pool])
    for (ln in lines) {
      ix <- pool[grades$target_line[pool] == ln]
      ix <- ix[order(-grades$score[ix])]
      if (length(ix)) chosen <- c(chosen, ix[[1]])
    }
    rest <- setdiff(pool, chosen)
    rest <- rest[order(-grades$score[rest])]
    room <- max(0L, k - length(chosen))
    if (room > 0L && length(rest)) chosen <- c(chosen, head(rest, room))
    if (length(chosen) > k) {
      chosen <- chosen[order(-grades$score[chosen])]
      chosen <- head(chosen, k)
    }
  }
  grades$tier[chosen] <- "confirmed"
  grades
}

#' Euro span only when config$driver_bindings names this series.
#' Without that link, moving the series cannot change fpa_run, so Impact stays blank.
fpa_driver_impact <- function(series_id, config) {
  binds <- config$driver_bindings %||% list()
  hit <- NULL
  for (b in binds) {
    if (identical(as.character(b$series_id %||% ""), as.character(series_id))) {
      hit <- b
      break
    }
  }
  # A wizard row names the series; only a row with euro factors prices it.
  if (!is.null(hit) && is.null(hit$low_factor) && is.null(hit$high_factor) && is.null(hit$oi_span_eur)) {
    hit <- NULL
  }
  if (is.null(hit)) {
    return(list(
      eur = NA_real_,
      status = "not_available",
      reason = fpa_txt("loc_no_mech_euro")
    ))
  }
  path <- as.character(config$._config_path %||% "")
  low_f <- hit$low_factor
  high_f <- hit$high_factor
  line <- as.character(hit$target_line %||% "")
  horizon_set <- !is.null(config$horizon_months) && !identical(as.character(config$horizon_months), "0")
  if (!is.null(low_f) && !is.null(high_f) && nzchar(line) && nzchar(path)) {
    if (!horizon_set) {
      return(list(
        eur = NA_real_,
        status = "not_available",
        reason = fpa_txt("loc_no_forecast_month")
      ))
    }
    low <- tryCatch(
      fpa_run(path, write = FALSE, stop_on_error = FALSE,
              line_scale = stats::setNames(list(as.numeric(low_f)), line)),
      error = function(e) NULL
    )
    high <- tryCatch(
      fpa_run(path, write = FALSE, stop_on_error = FALSE,
              line_scale = stats::setNames(list(as.numeric(high_f)), line)),
      error = function(e) NULL
    )
    if (is.null(low$pnl) || is.null(high$pnl)) {
      return(list(
        eur = NA_real_, status = "not_available",
        reason = fpa_txt("loc_engine_could_not")
      ))
    }
    return(list(
      eur = sum(high$pnl$OI, na.rm = TRUE) - sum(low$pnl$OI, na.rm = TRUE),
      status = "engine",
      reason = fpa_txt("loc_gap_engine_span")
    ))
  }
  list(
    eur = as.numeric(hit$oi_span_eur %||% NA_real_),
    status = if (is.null(hit$oi_span_eur)) "not_available" else "engine",
    reason = as.character(hit$reason %||% "Ecart declare par le mecanisme, pas par l'historique.")
  )
}

fpa_driver_grade_one <- function(candidate, evaluated, config) {
  n <- as.integer(candidate$n %||% 0L)
  identity <- as.character(candidate$identity %||% "independent")
  availability <- candidate$availability_lag_days
  if (is.null(evaluated)) {
    horizon <- NA_real_
    lag_status <- if (nzchar(candidate$block_reason %||% "")) "not_tested" else "not_distinguishable"
    evid_sig <- FALSE
    evid_r <- NA_real_
    evid_crit <- NA_real_
    indep_r <- NA_real_
    indep_status <- "not_available"
    indep_sig <- FALSE
    perm_rank <- NA_real_
    perm_n <- 0L
    tests_n <- 0L
    oos <- NA_real_
    causal <- NA_integer_
    sentence <- candidate$block_reason %||% ""
  } else {
    causal <- evaluated$lag_selected
    horizon <- fpa_driver_usable_horizon(causal, availability)
    lag_status <- evaluated$lag_status
    evid_sig <- isTRUE(evaluated$evidence$significant)
    evid_r <- evaluated$evidence$r
    evid_crit <- evaluated$evidence$r_crit
    indep_r <- evaluated$independence$r
    indep_status <- evaluated$independence$status %||% "not_available"
    indep_sig <- isTRUE(evaluated$independence$significant)
    perm_rank <- evaluated$permutation_rank
    perm_n <- evaluated$permutation_iterations
    tests_n <- evaluated$tests_performed
    oos <- evaluated$oos_skill
    sentence <- evaluated$permutation_sentence
  }
  driver_class <- fpa_driver_class(horizon, lag_status)
  evidence_class <- fpa_driver_evidence_class(
    identity, n, perm_rank, oos, lag_status, evid_sig
  )
  verdict <- fpa_driver_verdict(
    n, evid_sig, indep_sig, indep_status, lag_status,
    evidence_class, driver_class, identity
  )
  if (isTRUE(candidate$constant)) verdict <- "Reject"
  impact <- fpa_driver_impact(candidate$series_id, config)
  if (identical(verdict, "Needs more history")) {
    verdict_reason <- fpa_txt("loc_hist_too_short_24")
  } else if (identical(lag_status, "refused_reverse_causality")) {
    verdict_reason <- "Le pic est a un decalage negatif: la serie suit la ligne, elle ne la precede pas."
  } else if (identical(identity, "suspect_identity")) {
    verdict_reason <- fpa_txt("loc_series_copies")
  } else if (identical(identity, "part_whole")) {
    verdict_reason <- fpa_txt("loc_series_sum")
  } else if (identical(verdict, "Mechanism")) {
    verdict_reason <- fpa_txt("loc_mech_not_proof")
  } else if (identical(verdict, "Keep")) {
    verdict_reason <- "Le lien tient apres pre-traitement, depasse le hasard, et bat le naif saisonnier."
  } else if (identical(verdict, "Optional")) {
    verdict_reason <- fpa_txt("loc_link_not_three")
  } else {
    verdict_reason <- "Pas de lien distinguable du hasard sur la ligne proposee."
  }
  data.frame(
    series_id = candidate$series_id,
    source_id = candidate$source_id,
    series_name = candidate$series_name,
    target_line = candidate$target_line,
    target_reason = candidate$target_reason,
    target_status = candidate$target_status,
    n = n,
    evidence_r = evid_r,
    evidence_r_crit = evid_crit,
    evidence_sig = evid_sig,
    independence_r = indep_r,
    independence_status = indep_status,
    impact_eur = impact$eur,
    impact_status = impact$status,
    impact_reason = impact$reason,
    causal_lag = causal,
    lag_unit = "months",
    usable_horizon_days = horizon,
    driver_class = driver_class,
    permutation_rank = perm_rank,
    permutation_iterations = perm_n,
    permutation_sentence = sentence,
    tests_performed = tests_n,
    oos_skill = oos,
    evidence_class = evidence_class,
    lag_status = lag_status,
    verdict = verdict,
    verdict_reason = verdict_reason,
    redundancy_group = "",
    availability_lag_days = as.integer(availability %||% NA_integer_),
    aggregation_rule = candidate$aggregation_rule,
    identity = identity,
    stringsAsFactors = FALSE
  )
}

fpa_driver_findings <- function(grades, budget_sentence) {
  findings <- fpa_findings_empty()
  if (nzchar(budget_sentence)) {
    findings <- rbind(findings, fpa_finding("driver_budget", "info", budget_sentence))
  }
  if (is.null(grades) || !nrow(grades)) return(findings)
  for (i in seq_len(nrow(grades))) {
    g <- grades[i, , drop = FALSE]
    if (identical(g$lag_status, "refused_reverse_causality")) {
      findings <- rbind(findings, fpa_finding(
        "driver_reverse", "warning",
        paste0(g$series_name, ": ", g$verdict_reason)
      ))
    } else if (nzchar(g$redundancy_group) && !identical(g$verdict, "Needs more history")) {
      findings <- rbind(findings, fpa_finding(
        "driver_redundancy", "info",
        paste0(
          g$series_name, " partage le signal ", g$redundancy_group,
          ". Gardez un driver, l'autre se surveille."
        )
      ))
    } else if (identical(g$verdict, "Needs more history")) {
      findings <- rbind(findings, fpa_finding(
        "driver_short_history", "info",
        paste0(g$series_name, ": ", g$verdict_reason, " (n=", g$n, ").")
      ))
    }
    if (identical(g$driver_class, "explanatory") && !is.na(g$causal_lag) &&
        !identical(g$verdict, "Needs more history")) {
      findings <- rbind(findings, fpa_finding(
        "driver_explanatory", "info",
        paste0(
          g$series_name,
          " est explicatif: le delai d'arrivee depasse le decalage causal, ",
          fpa_txt("loc_sign_reversed")
        )
      ))
    }
  }
  findings
}

fpa_driver_write_csv <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, path, row.names = FALSE, fileEncoding = "UTF-8", na = "")
}

#' Write the artifact set. Charts read these files and do not refit.
fpa_driver_write_artifacts <- function(pass, paths) {
  root <- file.path(paths$work, "drivers")
  dir.create(file.path(root, "lag-profiles"), recursive = TRUE, showWarnings = FALSE)
  fpa_driver_write_csv(pass$candidates, file.path(root, "driver-candidates.csv"))
  fpa_driver_write_csv(pass$grades, file.path(root, "driver-grades.csv"))
  fpa_driver_write_csv(pass$edges, file.path(root, "driver-edges.csv"))
  fpa_driver_write_csv(pass$aligned, file.path(root, "aligned-series.csv"))
  fpa_driver_write_csv(pass$oos, file.path(root, "oos-backtests.csv"))
  forms <- pass$formulas
  if (is.null(forms)) {
    forms <- data.frame(id = character(), label = character(), target = character(), factors = character(),
                        n = integer(), median_gap = numeric(), verified = logical(), note = character())
  }
  fpa_driver_write_csv(forms, file.path(root, "formula-checks.csv"))
  for (sid in names(pass$profiles)) {
    safe <- gsub("[^A-Za-z0-9_-]", "_", sid)
    fpa_driver_write_csv(pass$profiles[[sid]], file.path(root, "lag-profiles", paste0(safe, ".csv")))
  }
  fpa_driver_write_csv(pass$findings, file.path(paths$reports, "driver-findings.csv"))
  invisible(root)
}

#' Peak lag from a stored profile. Does not re-run the search.
fpa_driver_profile_peak <- function(profile) {
  if (is.null(profile) || !nrow(profile) || !any(is.finite(profile$r))) {
    return(list(lag = NA_integer_, r = NA_real_))
  }
  i <- which.max(abs(profile$r))
  list(lag = as.integer(profile$lag[[i]]), r = profile$r[[i]])
}

fpa_driver_targets_from_result <- function(res) {
  if (is.null(res) || is.null(res$pnl) || !nrow(res$pnl)) return(NULL)
  dates <- fpa_parse_period(as.character(res$pnl$periode))
  list(dates = dates, pnl = res$pnl)
}

#' Line targets on closed months: the monthly history plus the closed months of
#' the book, the same frame as the operating-income test. A forecast month is
#' built from the drivers themselves, so it never enters a test of those drivers.
fpa_driver_targets_closed <- function(config, res = NULL) {
  wide <- tryCatch(fpa_driver_bottom_outcome(config), error = function(e) NULL)
  if (is.null(wide) || nrow(wide) < 8L) return(fpa_driver_targets_from_result(res))
  wide <- wide[wide$periode <= format(Sys.Date(), "%Y-%m"), , drop = FALSE]
  status_path <- file.path(fpa_paths(config)$work, "period-status.csv")
  if (file.exists(status_path)) {
    st <- utils::read.csv(status_path, stringsAsFactors = FALSE)
    open <- as.character(st$periode[st$status != "actual"])
    wide <- wide[!wide$periode %in% open, , drop = FALSE]
  }
  if (nrow(wide) < 8L) return(fpa_driver_targets_from_result(res))
  rownames(wide) <- NULL
  list(dates = fpa_parse_period(as.character(wide$periode)), pnl = wide)
}

fpa_driver_pick_volume <- function(candidates, config = NULL) {
  if (is.null(candidates) || !nrow(candidates)) return(NA_character_)
  declared <- if (is.null(config)) character() else fpa_driver_bound_columns(config, "volume")
  hit <- which(as.character(candidates$series_name) %in% declared)
  if (length(hit)) return(candidates$series_id[[hit[[1]]]])
  f <- vapply(candidates$series_name, fpa_fold_label, character(1))
  hit <- grep("nuitee|volume|couvert|booking|reservation|quantite", f)
  if (!length(hit)) return(NA_character_)
  candidates$series_id[[hit[[1]]]]
}

fpa_driver_month_series <- function(raw, rule) {
  fpa_stats_aggregate_month(raw$dates, raw$values, rule = rule %||% "mean")
}

#' Every graded series against operating income, closed months only.
#' The revenue test does not answer the result. A series is never partialled
#' out of itself: volume is controlled for the rate, every other series for volume.
fpa_driver_bottom_done <- function(grade_df) {
  if (is.null(grade_df) || !nrow(grade_df)) return(TRUE)
  if (!"bottom_line" %in% names(grade_df)) return(FALSE)
  all(nzchar(as.character(grade_df$bottom_line)))
}

fpa_driver_apply_bottom <- function(grade_df, config) {
  grade_df$bottom_line <- ""
  grade_df$bottom_n <- NA_integer_
  grade_df$bottom_from <- ""
  grade_df$bottom_to <- ""
  grade_df$bottom_r <- NA_real_
  grade_df$bottom_lag <- NA_integer_
  grade_df$bottom_rank <- NA_real_
  grade_df$bottom_partial <- NA_real_
  grade_df$bottom_skill <- NA_real_
  grade_df$bottom_sig <- FALSE
  grade_df$bottom_control <- ""
  grade_df$bottom_eur10 <- NA_real_
  if (!nrow(grade_df)) return(grade_df)
  built <- tryCatch(fpa_driver_bottom_build(config), error = function(e) NULL)
  if (is.null(built)) return(grade_df)
  wide <- built$outcome
  series <- built$series
  declared_vol <- intersect(fpa_driver_bound_columns(config, "volume"), names(series))
  declared_rate <- intersect(fpa_driver_bound_columns(config, "rate"), names(series))
  volume_name <- if (length(declared_vol)) declared_vol[[1]] else fpa_driver_bottom_volume_name(names(series))
  rate_name <- if (length(declared_rate)) {
    declared_rate[[1]]
  } else if ("tarif" %in% names(series)) "tarif" else if ("prix" %in% names(series)) "prix" else NA_character_
  oi <- stats::setNames(as.numeric(wide$OI), as.character(wide$periode))
  for (i in seq_len(nrow(grade_df))) {
    name <- as.character(grade_df$series_name[[i]])
    if (!nzchar(name) || !name %in% names(series)) {
      grade_df$bottom_line[[i]] <- "absent"
      next
    }
    vec <- series[[name]]
    key <- sort(intersect(names(vec), names(oi)))
    key <- key[is.finite(vec[key]) & is.finite(oi[key])]
    grade_df$bottom_n[[i]] <- length(key)
    if (length(key) >= 1L) {
      grade_df$bottom_from[[i]] <- key[[1]]
      grade_df$bottom_to[[i]] <- key[[length(key)]]
    }
    if (length(key) < 8L) {
      grade_df$bottom_line[[i]] <- "short"
      next
    }
    control_name <- if (identical(name, volume_name)) rate_name else volume_name
    control <- NULL
    if (!is.na(control_name) && nzchar(control_name) && !identical(control_name, name) &&
        control_name %in% names(series)) {
      control <- as.numeric(series[[control_name]][key])
      if (sum(is.finite(control)) < 8L) {
        control <- NULL
        control_name <- ""
      }
    } else {
      control_name <- ""
    }
    got <- fpa_driver_bottom_eval(
      as.numeric(vec[key]), as.numeric(oi[key]), fpa_parse_period(key), control
    )
    lag <- got$lag
    if (!length(lag) || !is.finite(lag[[1]])) lag <- NA_integer_
    grade_df$bottom_line[[i]] <- "OI"
    grade_df$bottom_r[[i]] <- if (!length(got$r) || !is.finite(got$r[[1]])) NA_real_ else got$r[[1]]
    grade_df$bottom_lag[[i]] <- as.integer(lag[[1]])
    grade_df$bottom_rank[[i]] <- got$rank
    grade_df$bottom_partial[[i]] <- got$partial
    grade_df$bottom_skill[[i]] <- got$skill
    grade_df$bottom_sig[[i]] <- isTRUE(got$sig)
    grade_df$bottom_control[[i]] <- if (is.na(control_name)) "" else as.character(control_name)
    grade_df$bottom_eur10[[i]] <- got$eur10
  }
  grade_df
}

fpa_driver_bottom_volume_name <- function(names_s) {
  names_s <- as.character(names_s)
  if (!length(names_s)) return(NA_character_)
  folded <- vapply(names_s, fpa_fold_label, character(1))
  hit <- grep("nuitee|volume|couvert|booking|reservation|quantite", folded)
  if (!length(hit)) return(NA_character_)
  hit <- hit[!grepl("budget|prevu|cible", folded[hit])]
  if (!length(hit)) return(NA_character_)
  pref <- hit[grepl("vendu", folded[hit])]
  if (length(pref)) names_s[[pref[[1]]]] else names_s[[hit[[1]]]]
}

#' Prove each declared formula on the closed months: the product of the factors
#' against the target (a column, or LINE:<id>). A ratio factor may be in % or 0-1;
#' both scales are tried. Proven when the median gap is 2 % or less on 8+ months.
fpa_driver_check_formulas <- function(config, monthly, names_of, lines, keys) {
  empty <- data.frame(
    id = character(), label = character(), target = character(), factors = character(),
    n = integer(), median_gap = numeric(), verified = logical(), note = character(),
    stringsAsFactors = FALSE
  )
  forms <- config$driver_formulas %||% list()
  if (!length(forms)) return(empty)
  by_name <- stats::setNames(names(names_of), unname(names_of))
  series_of <- function(col) {
    sid <- by_name[[col]]
    if (is.null(sid) || is.na(sid)) return(NULL)
    as.numeric(monthly[[sid]])
  }
  rows <- lapply(forms, function(f) {
    id <- as.character(f$id %||% "")
    label <- as.character(f$label %||% id)
    target <- as.character(f$target %||% "")
    factors <- as.character(unlist(f$factors %||% list()))
    base <- data.frame(
      id = id, label = label, target = target, factors = paste(factors, collapse = " x "),
      n = 0L, median_gap = NA_real_, verified = FALSE, note = "", stringsAsFactors = FALSE
    )
    if (length(factors) < 2L || !nzchar(target)) {
      base$note <- "Formule incomplete."
      return(base)
    }
    y <- if (startsWith(target, "LINE:")) {
      ln <- sub("^LINE:", "", target)
      if (is.null(lines[[ln]])) NULL else abs(as.numeric(lines[[ln]]))
    } else {
      series_of(target)
    }
    xs <- lapply(factors, series_of)
    if (is.null(y) || any(vapply(xs, is.null, logical(1)))) {
      base$note <- fpa_txt("loc_formula_col_missing")
      return(base)
    }
    kinds <- vapply(factors, function(col) fpa_segment_kind(config, col), character(1))
    scaled <- kinds %in% c("ratio", "utilisation")
    options <- expand.grid(rep(list(c(1, 0.01)), sum(scaled)))
    best <- list(gap = Inf, n = 0L)
    for (o in seq_len(max(1L, nrow(options)))) {
      mult <- rep(1, length(factors))
      if (sum(scaled)) mult[scaled] <- unlist(options[o, ])
      prod <- Reduce(`*`, Map(function(x, m) x * m, xs, mult))
      ok <- is.finite(prod) & is.finite(y) & abs(y) > 1e-9
      if (sum(ok) < 1L) next
      gap <- stats::median(abs(prod[ok] - y[ok]) / abs(y[ok]))
      if (gap < best$gap) best <- list(gap = gap, n = sum(ok))
    }
    base$n <- as.integer(best$n)
    base$median_gap <- if (is.finite(best$gap)) best$gap else NA_real_
    base$verified <- is.finite(best$gap) && best$gap <= 0.02 && best$n >= 8L
    base$note <- if (base$verified) {
      fpa_txt("loc_formula_checked")
    } else if (best$n < 8L) {
      fpa_txt("loc_formula_few_months")
    } else {
      "Le produit ne reproduit pas la cible : formule non retenue."
    }
    base
  })
  do.call(rbind, rows)
}

#' Columns the forecast reads, mirrored from fpa_read_volumes (REV and PAYROLL
#' overlays) and fpa_volume_by_month (volume for line drivers volume / mixed).
fpa_driver_forecast_columns <- function(config, present) {
  present <- as.character(present)
  used <- character()
  mech <- list(
    capacity_x_util_x_rate = c("capacite", "taux_utilisation", "tarif"),
    qty_x_price = c("quantite", "prix"),
    customers_x_spend = c("nb_clients", "depense_moyenne"),
    headcount_x_cost = c("effectif", "cout_moyen")
  )
  for (id in names(mech)) {
    on <- tryCatch(fpa_mechanism_on(config, id), error = function(e) FALSE)
    if (isTRUE(on) && all(mech[[id]] %in% present)) used <- c(used, mech[[id]])
  }
  rows <- tryCatch(fpa_line_driver_rows(config), error = function(e) NULL)
  if (!is.null(rows) && any(rows$driver %in% c("volume", "mixed"))) {
    vol <- if ("nuitees_vendues" %in% present) {
      "nuitees_vendues"
    } else if (all(c("capacite", "taux_utilisation") %in% present)) {
      c("capacite", "taux_utilisation")
    } else if ("quantite" %in% present) {
      "quantite"
    } else if ("nb_clients" %in% present) {
      "nb_clients"
    } else {
      character()
    }
    used <- c(used, vol)
  }
  unique(used)
}

#' Four states per series, read by every driver view.
#' configured: named in a declared formula (factor or target), or bound part/whole.
#' proved: in a formula verified on the closed months. failed: declared only in
#' formulas that did not reproduce the target. stat: the statistical test holds.
#' used: the forecast reads the column (fpa_driver_forecast_columns).
fpa_driver_state <- function(grades, formulas = NULL, config = NULL) {
  if (is.null(grades) || !nrow(grades)) return(grades)
  name <- as.character(grades$series_name)
  in_forms <- function(ok) {
    if (is.null(formulas) || !nrow(formulas)) return(rep(FALSE, length(name)))
    f <- formulas[as.logical(formulas$verified) %in% ok, , drop = FALSE]
    cols <- unique(c(
      unlist(strsplit(as.character(f$factors), " x ", fixed = TRUE)),
      as.character(f$target)
    ))
    name %in% cols
  }
  proved <- in_forms(TRUE)
  declared <- in_forms(c(TRUE, FALSE, NA))
  failed <- declared & !proved
  rel <- as.character(grades$relation %||% rep("", length(name)))
  configured <- declared | rel %in% c("part_whole", "derived")
  ev <- as.character(grades$evidence_class %||% rep("", length(name)))
  sig <- if ("bottom_sig" %in% names(grades)) as.logical(grades$bottom_sig) else rep(NA, length(name))
  stat <- ev == "statistically_validated" | (!is.na(sig) & sig)
  used <- name %in% fpa_driver_forecast_columns(config, name)
  label <- rep("", length(name))
  label[proved & stat] <- fpa_txt("drv_state_proved_stat")
  label[proved & !stat] <- fpa_txt("drv_state_proved_open")
  label[failed] <- fpa_txt("drv_state_failed")
  label[failed & used] <- fpa_txt("drv_state_failed_used")
  grades$state_configured <- configured
  grades$state_proved <- proved
  grades$state_failed <- failed
  grades$state_stat <- stat
  grades$state_used <- used
  grades$state_label <- label
  grades
}

fpa_driver_bottom_eval <- function(drv, oi, dates, control) {
  ev <- tryCatch(
    fpa_stats_evaluate(
      drv, oi, dates, control = control,
      window = c(-3L, 3L), n_perm = 2000L, seed = 21L
    ),
    error = function(e) NULL
  )
  r <- if (is.null(ev)) NA_real_ else ev$evidence$r
  sig <- isTRUE(!is.null(ev) && ev$evidence$significant)
  list(
    r = r,
    lag = if (is.null(ev) || is.null(ev$lag_selected)) NA_integer_ else as.integer(ev$lag_selected),
    rank = if (is.null(ev)) NA_real_ else ev$permutation_rank,
    partial = if (is.null(ev)) NA_real_ else ev$independence$r,
    skill = if (is.null(ev)) NA_real_ else ev$oos_skill,
    sig = sig,
    # No euro from a correlation: rate, occupancy and nights share the season,
    # so any monthly slope splits their effect at random. Euros come from the engine.
    eur10 = NA_real_
  )
}

#' Named monthly series from the activity file and the extra csv files.
fpa_driver_bottom_series <- function(config) {
  paths <- fpa_paths(config)
  files <- character()
  vol <- as.character(config$paths$ops_other %||% "")
  if (!nzchar(vol) || !file.exists(vol)) vol <- file.path(paths$input, "volumes-mensuel-2026.csv")
  if (file.exists(vol)) files <- c(files, vol)
  for (ex in config$extra_files %||% list()) {
    p <- as.character(ex$path %||% ex$file %||% "")
    if (nzchar(p) && file.exists(p) && grepl("\\.csv$", p, ignore.case = TRUE)) files <- c(files, p)
  }
  series <- list()
  for (f in unique(files)) {
    df <- utils::read.csv(f, stringsAsFactors = FALSE, check.names = FALSE)
    df <- fpa_segment_collapse(df, config)
    if (!"periode" %in% names(df)) next
    per <- as.character(df$periode)
    for (col in setdiff(names(df), "periode")) {
      if (col %in% names(series)) next
      series[[col]] <- stats::setNames(suppressWarnings(as.numeric(df[[col]])), per)
    }
  }
  series
}

fpa_driver_bottom_build <- function(config) {
  wide <- fpa_driver_bottom_outcome(config)
  if (is.null(wide) || !nrow(wide)) return(NULL)
  series <- fpa_driver_bottom_series(config)
  if (!length(series)) return(NULL)
  list(outcome = wide, series = series)
}

#' Operating income on the monthly history plus closed months of the current book.
fpa_driver_bottom_outcome <- function(config) {
  paths <- fpa_paths(config)
  hist <- fpa_line_history(config, paths)
  map <- fpa_line_map(config, "pnl")
  if (is.null(hist) || !nrow(hist) || is.null(map)) return(NULL)
  fun <- vapply(hist$libelle, function(lab) {
    got <- fpa_lookup_line(lab, map)
    if (is.na(got)) "" else as.character(got)
  }, character(1))
  hist$fun <- fun
  periods <- sort(unique(hist$periode))
  sum_line <- function(per, id) sum(hist$montant[hist$periode == per & hist$fun == id], na.rm = TRUE)
  wide <- data.frame(
    periode = periods,
    REV = vapply(periods, sum_line, numeric(1), id = "REV"),
    OTHER_OP_INC = vapply(periods, sum_line, numeric(1), id = "OTHER_OP_INC"),
    COGS = vapply(periods, sum_line, numeric(1), id = "COGS"),
    PAYROLL = vapply(periods, sum_line, numeric(1), id = "PAYROLL"),
    OPEX_OTHER = vapply(periods, sum_line, numeric(1), id = "OPEX_OTHER"),
    D_A = vapply(periods, sum_line, numeric(1), id = "D_A"),
    FIN = vapply(periods, sum_line, numeric(1), id = "FIN"),
    EXCEP = vapply(periods, sum_line, numeric(1), id = "EXCEP"),
    TAX = vapply(periods, sum_line, numeric(1), id = "TAX"),
    stringsAsFactors = FALSE
  )
  wide <- fpa_pnl_derive(wide)
  book <- file.path(paths$work, "pnl-monthly.csv")
  if (file.exists(book)) {
    extra <- utils::read.csv(book, stringsAsFactors = FALSE)
    extra <- extra[!extra$periode %in% wide$periode & extra$periode <= format(Sys.Date(), "%Y-%m"), , drop = FALSE]
    keep <- intersect(c("periode", "REV", "OTHER_OP_INC", "COGS", "PAYROLL", "OPEX_OTHER", "D_A", "FIN", "EXCEP", "TAX"), names(extra))
    if (nrow(extra) && all(c("periode", "OI") %in% names(extra))) {
      extra <- extra[, c("periode", "REV", "OTHER_OP_INC", "COGS", "PAYROLL", "OPEX_OTHER", "D_A", "FIN", "EXCEP", "TAX", "OI", "NI")]
      wide$NI <- wide$OI - wide$FIN - wide$EXCEP - wide$TAX
      wide <- rbind(wide[, names(extra)], extra[, names(extra)])
    } else if (nrow(extra) && length(keep) >= 2L) {
      extra <- fpa_pnl_derive(extra[, keep, drop = FALSE])
      wide <- rbind(wide, extra[, names(wide)])
    }
  }
  if (!"OI" %in% names(wide) || sum(is.finite(wide$OI)) < 8L) return(NULL)
  wide <- wide[order(wide$periode), , drop = FALSE]
  wide
}

#' Run discovery and the three tests. Writes work/drivers when write = TRUE.
fpa_driver_pass <- function(config_path, res = NULL, n_perm = 2000L,
                            write = TRUE, seed = 21L, window = c(-3L, 3L)) {
  config <- fpa_load_config(config_path)
  config$._config_path <- config_path
  paths <- fpa_paths(config)
  closed_through <- ""
  st <- if (is.null(res)) NULL else res$period_status
  if (!is.null(st) && nrow(st) && "status" %in% names(st)) {
    act <- as.character(st$periode[as.character(st$status) == "actual"])
    act <- act[nzchar(act)]
    if (length(act)) closed_through <- max(act)
  }
  profiled <- fpa_profile_drivers(config, paths, closed_through = closed_through)
  candidates <- profiled$candidates
  targets <- fpa_driver_targets_closed(config, res)
  volume_id <- fpa_driver_pick_volume(candidates, config)
  history_n <- if (!is.null(targets)) sum(!is.na(targets$dates)) else {
    if (nrow(candidates)) max(candidates$n) else 0L
  }
  budget <- fpa_driver_data_budget(history_n)

  grades <- list()
  profiles <- list()
  aligned_rows <- list()
  oos_rows <- list()
  edges <- list()
  resid <- list()
  line_info <- list()

  volume_month <- NULL
  if (!is.na(volume_id) && !is.null(profiled$series[[volume_id]])) {
    rule <- candidates$aggregation_rule[candidates$series_id == volume_id][[1]]
    volume_month <- fpa_driver_month_series(profiled$series[[volume_id]], rule)
  }

  for (i in seq_len(nrow(candidates))) {
    cand <- candidates[i, , drop = FALSE]
    sid <- cand$series_id
    spec <- fpa_driver_spec(config, sid)
    if (nzchar(spec$target_line)) {
      cand$target_line <- spec$target_line
      cand$target_reason <- fpa_txt("loc_line_in_bindings")
      cand$target_status <- "declared"
    }
    raw <- profiled$series[[sid]]
    evaluated <- NULL
    if (!nzchar(cand$block_reason) && !is.null(raw) && !is.null(targets)) {
      month <- fpa_driver_month_series(raw, cand$aggregation_rule)
      line <- cand$target_line
      if (!line %in% names(targets$pnl)) line <- "REV"
      tgt <- as.numeric(targets$pnl[[line]])
      ctl <- NULL
      if (!is.na(volume_id) && !identical(sid, volume_id) && !is.null(volume_month)) {
        aligned_ctl <- fpa_stats_align(
          volume_month$dates, volume_month$values,
          targets$dates, tgt
        )
        ctl <- aligned_ctl$driver
      }
      aligned <- fpa_stats_align(month$dates, month$values, targets$dates, tgt, ctl)
      ok <- is.finite(aligned$driver) & is.finite(aligned$target)
      if (sum(ok) >= 8L) {
        use_ctl <- if (!is.null(ctl) && sum(is.finite(aligned$control) & ok) >= 8L) {
          aligned$control
        } else {
          NULL
        }
        evaluated <- tryCatch(
          fpa_stats_evaluate(
            aligned$driver, aligned$target, aligned$date,
            control = use_ctl,
            window = c(-spec$lag_window_max, spec$lag_window_max),
            n_perm = n_perm, seed = seed
          ),
          error = function(e) NULL
        )
        if (!is.null(evaluated)) {
          keys <- aligned$periode[ok]
          line_info[[sid]] <- list(n = sum(ok), from = keys[[1]], to = keys[[length(keys)]])
          profiles[[sid]] <- evaluated$profile
          resid[[sid]] <- evaluated$driver_resid
          aligned$driver_resid <- evaluated$driver_resid
          aligned$target_resid <- evaluated$target_resid
          aligned$series_id <- sid
          aligned_rows[[length(aligned_rows) + 1]] <- aligned
          if (nrow(evaluated$oos_points)) {
            pts <- evaluated$oos_points
            pts$series_id <- sid
            oos_rows[[length(oos_rows) + 1]] <- pts
          }
          edges[[length(edges) + 1]] <- data.frame(
            series_id = sid,
            series_name = cand$series_name,
            target_line = line,
            lag = evaluated$lag_selected,
            r = evaluated$evidence$r,
            euro_span = NA_real_,
            lag_status = evaluated$lag_status,
            stringsAsFactors = FALSE
          )
        }
      }
    }
    grades[[length(grades) + 1]] <- fpa_driver_grade_one(cand, evaluated, config)
  }

  grade_df <- if (length(grades)) {
    do.call(rbind, grades)
  } else {
    fpa_driver_grade_one(
      data.frame(
        series_id = "none", source_id = "", series_name = "", target_line = "REV",
        target_reason = "", target_status = "suggested", n = 0L, identity = "independent",
        availability_lag_days = NA_integer_, aggregation_rule = "mean", block_reason = fpa_txt("loc_no_driver"),
        stringsAsFactors = FALSE
      ),
      NULL, config
    )[0, , drop = FALSE]
  }
  if (nrow(grade_df) && length(resid)) {
    long_ids <- grade_df$series_id[!is.na(grade_df$n) & grade_df$n >= 24]
    groups <- fpa_stats_redundancy(resid[names(resid) %in% long_ids])
    for (sid in names(groups)) {
      grade_df$redundancy_group[grade_df$series_id == sid] <- groups[[sid]]
    }
  }
  formulas <- NULL
  if (nrow(grade_df)) {
    grade_df$segment_note <- vapply(as.character(grade_df$source_id), function(src) {
      fpa_segment_note(profiled$segments[[src]])
    }, character(1))
  }
  if (nrow(grade_df)) {
    grade_df$relation <- "independent"
    grade_df$prior <- NA_real_
    monthly <- list()
    if (!is.null(targets)) {
      keys <- format(as.Date(targets$dates), "%Y-%m")
      for (i in seq_len(nrow(candidates))) {
        sid <- candidates$series_id[[i]]
        raw <- profiled$series[[sid]]
        if (is.null(raw)) next
        month <- fpa_driver_month_series(raw, candidates$aggregation_rule[[i]])
        monthly[[sid]] <- month$values[match(keys, format(as.Date(month$dates), "%Y-%m"))]
      }
      lines <- list()
      for (ln in c("REV", "PAYROLL", "COGS", "OPEX_OTHER")) {
        if (ln %in% names(targets$pnl)) lines[[ln]] <- as.numeric(targets$pnl[[ln]])
      }
      parts <- fpa_driver_product_ids(monthly, lines)
      grade_df$relation[grade_df$series_id %in% parts] <- "part_whole"
      names_of <- stats::setNames(as.character(candidates$series_name), as.character(candidates$series_id))
      formulas <- fpa_driver_check_formulas(config, monthly, names_of, lines, keys)
      if (nrow(formulas)) {
        proven <- unique(unlist(strsplit(formulas$factors[formulas$verified], " x ", fixed = TRUE)))
        grade_df$relation[grade_df$series_name %in% proven] <- "part_whole"
      }
    }
    for (i in seq_len(nrow(grade_df))) {
      spec <- fpa_driver_spec(config, grade_df$series_id[[i]])
      if (nzchar(spec$relation)) grade_df$relation[[i]] <- spec$relation
      if (is.finite(spec$prior)) grade_df$prior[[i]] <- spec$prior
    }
    grade_df <- fpa_driver_assign_tiers(grade_df, history_n)
  }
  if (nrow(grade_df)) {
    pick <- function(sid, field, empty) {
      got <- line_info[[sid]][[field]]
      if (is.null(got)) empty else got
    }
    grade_df$line_n <- vapply(grade_df$series_id, pick, integer(1), field = "n", empty = NA_integer_)
    grade_df$line_from <- vapply(grade_df$series_id, pick, character(1), field = "from", empty = "")
    grade_df$line_to <- vapply(grade_df$series_id, pick, character(1), field = "to", empty = "")
  }
  findings <- fpa_driver_findings(grade_df, budget)
  if (nrow(grade_df)) grade_df <- fpa_driver_apply_bottom(grade_df, config)
  edge_df <- if (length(edges)) do.call(rbind, edges) else data.frame(
    series_id = character(), series_name = character(), target_line = character(),
    lag = integer(), r = numeric(), euro_span = numeric(), lag_status = character(),
    stringsAsFactors = FALSE
  )
  aligned_df <- if (length(aligned_rows)) do.call(rbind, aligned_rows) else data.frame(
    periode = character(), date = as.Date(character()), driver = numeric(),
    target = numeric(), control = numeric(), driver_resid = numeric(),
    target_resid = numeric(), series_id = character(), stringsAsFactors = FALSE
  )
  oos_df <- if (length(oos_rows)) do.call(rbind, oos_rows) else data.frame(
    periode = character(), actual = numeric(), predicted = numeric(),
    seasonal_naive = numeric(), series_id = character(), stringsAsFactors = FALSE
  )

  testable <- if (nrow(candidates)) sum(!nzchar(candidates$block_reason)) else 0L
  refused <- if (nrow(grade_df)) sum(grade_df$lag_status == "refused_reverse_causality") else 0L
  summary_text <- paste0(
    "Candidats : ", nrow(candidates), " (testables : ", testable, ")\n",
    budget, "\n",
    "Refus (sens inverse) : ", refused, "\n",
    fpa_txt("loc_euro_unavailable")
  )
  pass <- list(
    candidates = candidates,
    grades = grade_df,
    formulas = formulas,
    edges = edge_df,
    profiles = profiles,
    aligned = aligned_df,
    oos = oos_df,
    findings = findings,
    summary_text = summary_text,
    data_budget = budget,
    paths = paths
  )
  if (isTRUE(write)) fpa_driver_write_artifacts(pass, paths)
  pass
}
