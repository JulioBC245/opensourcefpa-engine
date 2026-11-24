# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-variance.R — Actual versus phased budget, with an explicit residual.
# LAST CHANGE: 2026-09-23
# A decomposition is only filled when a mechanism can name the pieces.
# Whatever is left stays in unexplained. It is never folded into a driver.
# Sign convention (Wattig): unfavourable gaps are negative — revenue shortfall
# and expense overrun both hurt the result, so both write as negative here.

fpa_variance_empty <- function() {
  data.frame(
    periode = character(), line_id = character(),
    actual = numeric(), budget = numeric(),
    variance = numeric(), variance_pct = numeric(),
    price = numeric(), volume = numeric(), mix = numeric(),
    rate = numeric(), headcount = numeric(),
    unexplained = numeric(), status = character(),
    stringsAsFactors = FALSE
  )
}

#' Join monthly actuals to the phased budget on period and line.
#' @param volumes optional volumes$data with periode and driver columns.
#' @param status optional period status (periode, status). Each row then says
#'   whether its "actual" side is a closed month or the forecast on file.
fpa_variance_build <- function(pnl, budget, volumes = NULL, config = NULL, status = NULL) {
  if (is.null(pnl) || !nrow(pnl) || is.null(budget) || !nrow(budget)) {
    return(fpa_variance_empty())
  }
  lines <- intersect(unique(as.character(budget$functional_label)), names(pnl))
  rows <- list()
  for (line in lines) {
    b <- budget[as.character(budget$functional_label) == line, , drop = FALSE]
    for (i in seq_len(nrow(b))) {
      p <- as.character(b$periode[[i]])
      if (!nzchar(p) || !(p %in% pnl$periode)) next
      actual <- as.numeric(pnl[[line]][pnl$periode == p][[1]])
      bud <- as.numeric(b$montant[[i]])
      variance <- actual - bud
      pct <- if (is.finite(bud) && abs(bud) > 1e-9) variance / bud else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(
        periode = p, line_id = line, actual = actual, budget = bud,
        variance = variance, variance_pct = pct,
        price = NA_real_, volume = NA_real_, mix = NA_real_,
        rate = NA_real_, headcount = NA_real_,
        unexplained = variance,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!is.null(volumes) && nrow(volumes) &&
      all(c("periode", "nuitees_budget", "tarif_budget") %in% names(volumes)) &&
      "REV" %in% names(pnl)) {
    have <- if (length(rows)) {
      vapply(rows, function(r) if (identical(r$line_id[[1]], "REV")) r$periode[[1]] else "", character(1))
    } else {
      character(0)
    }
    for (i in seq_len(nrow(volumes))) {
      p <- as.character(volumes$periode[[i]])
      q0 <- suppressWarnings(as.numeric(volumes$nuitees_budget[[i]]))
      p0 <- suppressWarnings(as.numeric(volumes$tarif_budget[[i]]))
      if (!nzchar(p) || !(p %in% pnl$periode) || p %in% have) next
      if (!is.finite(q0) || !is.finite(p0)) next
      actual <- as.numeric(pnl$REV[pnl$periode == p][[1]])
      bud <- q0 * p0
      variance <- actual - bud
      pct <- if (abs(bud) > 1e-9) variance / bud else NA_real_
      rows[[length(rows) + 1L]] <- data.frame(
        periode = p, line_id = "REV", actual = actual, budget = bud,
        variance = variance, variance_pct = pct,
        price = NA_real_, volume = NA_real_, mix = NA_real_,
        rate = NA_real_, headcount = NA_real_,
        unexplained = variance,
        stringsAsFactors = FALSE
      )
    }
  }
  if (!length(rows)) return(fpa_variance_empty())
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out <- fpa_variance_decompose(out, volumes, config)
  fpa_variance_tag_status(out, status)
}

#' "actual" or "forecast" on every variance row. Without a status file the
#' column stays "actual", which is what the table always assumed.
fpa_variance_tag_status <- function(var_df, status = NULL) {
  if (is.null(var_df)) return(var_df)
  var_df$status <- rep("actual", nrow(var_df))
  if (!nrow(var_df) || is.null(status) || !nrow(status) ||
      !all(c("periode", "status") %in% names(status))) {
    return(var_df)
  }
  fc <- as.character(status$periode[as.character(status$status) == "forecast"])
  var_df$status[as.character(var_df$periode) %in% fc] <- "forecast"
  var_df
}

#' Price, volume and mix only for revenue when the mechanism is on and both
#' the actual and the budget carry a quantity and a price. Otherwise the
#' whole gap stays unexplained.
fpa_variance_decompose <- function(var_df, volumes, config) {
  if (is.null(var_df) || !nrow(var_df) || is.null(config)) return(var_df)
  if (!fpa_mechanism_on(config, "capacity_x_util_x_rate") &&
      !fpa_mechanism_on(config, "qty_x_price") &&
      !fpa_mechanism_on(config, "headcount_x_cost")) {
    return(var_df)
  }
  if (is.null(volumes) || !nrow(volumes) || !("periode" %in% names(volumes))) return(var_df)
  volumes$periode <- as.character(volumes$periode)
  for (i in seq_len(nrow(var_df))) {
    if (!identical(var_df$line_id[[i]], "REV")) next
    p <- var_df$periode[[i]]
    sl <- volumes[volumes$periode == p, , drop = FALSE]
    if (!nrow(sl)) next
    if (all(c("nuitees_vendues", "tarif", "nuitees_budget", "tarif_budget") %in% names(sl))) {
      q1 <- as.numeric(sl$nuitees_vendues[[1]])
      p1 <- as.numeric(sl$tarif[[1]])
      q0 <- as.numeric(sl$nuitees_budget[[1]])
      p0 <- as.numeric(sl$tarif_budget[[1]])
      if (all(is.finite(c(q1, p1, q0, p0)))) {
        price <- q0 * (p1 - p0)
        volume <- p0 * (q1 - q0)
        mix <- (q1 - q0) * (p1 - p0)
        var_df$price[[i]] <- price
        var_df$volume[[i]] <- volume
        var_df$mix[[i]] <- mix
        var_df$unexplained[[i]] <- var_df$variance[[i]] - price - volume - mix
      }
    }
  }
  if (fpa_mechanism_on(config, "headcount_x_cost")) {
    for (i in seq_len(nrow(var_df))) {
      if (!identical(var_df$line_id[[i]], "PAYROLL")) next
      p <- var_df$periode[[i]]
      sl <- volumes[volumes$periode == p, , drop = FALSE]
      if (!nrow(sl)) next
      need <- c("effectif", "cout_moyen", "effectif_budget", "cout_moyen_budget")
      if (!all(need %in% names(sl))) next
      h1 <- as.numeric(sl$effectif[[1]])
      r1 <- as.numeric(sl$cout_moyen[[1]])
      h0 <- as.numeric(sl$effectif_budget[[1]])
      r0 <- as.numeric(sl$cout_moyen_budget[[1]])
      if (!all(is.finite(c(h1, r1, h0, r0)))) next
      rate <- h0 * (r1 - r0)
      head <- r0 * (h1 - h0)
      var_df$rate[[i]] <- rate
      var_df$headcount[[i]] <- head
      var_df$unexplained[[i]] <- var_df$variance[[i]] - rate - head
    }
  }
  var_df
}

#' Doctrine-signed gap: unfavourable (hurts OI) is negative for every line.
fpa_variance_signed <- function(line_id, variance) {
  v <- suppressWarnings(as.numeric(variance))
  s <- fpa_pnl_ni_sign(as.character(line_id %||% ""))
  if (!is.finite(v) || !is.finite(s) || s == 0) return(v)
  v * s
}

fpa_variance_driver_labels <- function() {
  c(
    price = "prix", volume = "volume", mix = "mix",
    rate = "taux", headcount = "effectifs", unexplained = fpa_txt("loc_remains_to_explain")
  )
}

#' Biggest material month for one line, Wattig commentary slots filled from the engine only.
fpa_variance_headline <- function(var_df, line, abs_floor = 1000, reference = "budget") {
  empty <- fpa_txt("var_none")
  if (is.null(var_df) || !nrow(var_df)) return(empty)
  line <- as.character(line %||% "")
  if (!nzchar(line)) return(empty)
  sl <- var_df[as.character(var_df$line_id) == line, , drop = FALSE]
  if (!nrow(sl)) return(empty)
  signed <- vapply(seq_len(nrow(sl)), function(i) {
    fpa_variance_signed(line, sl$variance[[i]])
  }, numeric(1))
  ok <- is.finite(signed) & abs(signed) > abs_floor
  if (!any(ok)) return(empty)
  i <- which(ok)[which.max(abs(signed[ok]))]
  amt <- signed[[i]]
  fav <- if (amt >= 0) fpa_txt("var_fav") else fpa_txt("var_unfav")
  label <- tryCatch(fpa_line_fr(line), error = function(e) line)
  if (!nzchar(label %||% "")) label <- line
  drivers <- fpa_variance_driver_labels()
  parts <- c()
  amounts <- c()
  for (nm in names(drivers)) {
    if (!(nm %in% names(sl))) next
    raw <- suppressWarnings(as.numeric(sl[[nm]][[i]]))
    if (!is.finite(raw) || abs(raw) < 0.5) next
    parts <- c(parts, unname(drivers[[nm]]))
    amounts <- c(amounts, fpa_variance_signed(line, raw))
  }
  if (length(parts) >= 2L) {
    ord <- order(abs(amounts), decreasing = TRUE)
    parts <- parts[ord]
    amounts <- amounts[ord]
    if (amounts[[1]] * amounts[[2]] < 0) {
      fpa_txt(
        "var_headline_driven",
        label, fav, reference, fpa_synthese_eur(amt), sl$periode[[i]],
        parts[[1]], fpa_synthese_eur(amounts[[1]]),
        parts[[2]], fpa_synthese_eur(amounts[[2]])
      )
    } else {
      fpa_txt(
        "var_headline_one",
        label, fav, reference, fpa_synthese_eur(amt), sl$periode[[i]],
        parts[[1]], fpa_synthese_eur(amounts[[1]])
      )
    }
  } else if (length(parts) == 1L) {
    fpa_txt(
      "var_headline_one",
      label, fav, reference, fpa_synthese_eur(amt), sl$periode[[i]],
      parts[[1]], fpa_synthese_eur(amounts[[1]])
    )
  } else {
    fpa_txt(
      "var_headline_plain",
      label, fav, reference, fpa_synthese_eur(amt), sl$periode[[i]]
    )
  }
}

fpa_variance_sign_note <- function() {
  fpa_txt("var_sign")
}

#' Rest of the forecast year versus budget, plus the closed months of that year.
#' The outlook is the forecast already on file. The closed gap is not extended.
fpa_variance_rest_of_year <- function(var_df, line, status = NULL, abs_floor = 1000) {
  if (is.null(var_df) || !nrow(var_df) || !nzchar(as.character(line %||% ""))) {
    return(fpa_txt("var_rest_none"))
  }
  line <- as.character(line)
  sl <- var_df[as.character(var_df$line_id) == line, , drop = FALSE]
  if (!nrow(sl)) return(fpa_txt("var_rest_none"))
  forecast <- character()
  actual <- character()
  if (!is.null(status) && nrow(status) && all(c("periode", "status") %in% names(status))) {
    per <- as.character(status$periode)
    flag <- as.character(status$status)
    forecast <- per[flag == "forecast"]
    actual <- per[flag != "forecast"]
  }
  if (!length(forecast)) return(fpa_txt("var_rest_none"))
  year <- substr(sort(forecast)[[1]], 1L, 4L)
  in_year <- function(periods) periods[substr(periods, 1L, 4L) == year]
  rest_p <- intersect(as.character(sl$periode), in_year(forecast))
  closed_p <- intersect(as.character(sl$periode), in_year(actual))
  if (!length(rest_p)) return(fpa_txt("var_rest_none"))
  signed_sum <- function(periods) {
    rows <- sl[as.character(sl$periode) %in% periods, , drop = FALSE]
    if (!nrow(rows)) return(0)
    sum(vapply(seq_len(nrow(rows)), function(i) {
      fpa_variance_signed(line, rows$variance[[i]])
    }, numeric(1)), na.rm = TRUE)
  }
  rest <- signed_sum(rest_p)
  closed <- signed_sum(closed_p)
  from <- min(rest_p)
  to <- max(rest_p)
  year_gap <- closed + rest
  if (!is.finite(rest) || abs(rest) <= abs_floor) {
    fpa_txt("var_rest_flat", from, to, fpa_synthese_eur(year_gap))
  } else {
    fpa_txt("var_rest_gap", from, to, fpa_synthese_eur(rest), fpa_synthese_eur(year_gap))
  }
}

#' Why line for a KPI card: largest absolute driver on the selected slice, or residual.
fpa_variance_why <- function(var_df, line) {
  if (is.null(var_df) || !nrow(var_df) || !nzchar(as.character(line %||% ""))) {
    return(fpa_txt("var_why_none"))
  }
  sl <- var_df[as.character(var_df$line_id) == as.character(line), , drop = FALSE]
  if (!nrow(sl)) return(fpa_txt("var_why_none"))
  drivers <- fpa_variance_driver_labels()
  tot <- numeric(0)
  labs <- character(0)
  for (nm in names(drivers)) {
    if (!(nm %in% names(sl))) next
    s <- sum(suppressWarnings(as.numeric(sl[[nm]])), na.rm = TRUE)
    if (!is.finite(s) || abs(s) < 0.5) next
    tot <- c(tot, fpa_variance_signed(line, s))
    labs <- c(labs, unname(drivers[[nm]]))
  }
  if (!length(tot)) return(fpa_txt("var_why_none"))
  j <- which.max(abs(tot))
  fpa_txt("var_why", labs[[j]], fpa_synthese_eur(tot[[j]]))
}

fpa_variance_findings <- function(var_df, abs_floor = 1000) {
  findings <- fpa_findings_empty()
  if (is.null(var_df) || !nrow(var_df)) return(findings)
  signed <- vapply(seq_len(nrow(var_df)), function(i) {
    fpa_variance_signed(var_df$line_id[[i]], var_df$variance[[i]])
  }, numeric(1))
  big <- var_df[is.finite(signed) & abs(signed) > abs_floor, , drop = FALSE]
  signed <- signed[is.finite(signed) & abs(signed) > abs_floor]
  if (!nrow(big)) return(findings)
  ord <- order(abs(signed), decreasing = TRUE)
  big <- big[ord, , drop = FALSE]
  signed <- signed[ord]
  big <- utils::head(big, 5)
  signed <- utils::head(signed, 5)
  for (i in seq_len(nrow(big))) {
    label <- tryCatch(fpa_line_fr(big$line_id[[i]]), error = function(e) big$line_id[[i]])
    unexp <- fpa_variance_signed(big$line_id[[i]], big$unexplained[[i]])
    findings <- rbind(findings, fpa_finding(
      "variance", "info",
      fpa_txt(
        "var_finding",
        label, big$periode[[i]],
        fpa_synthese_eur(signed[[i]]),
        fpa_synthese_eur(unexp)
      )
    ))
  }
  findings
}
