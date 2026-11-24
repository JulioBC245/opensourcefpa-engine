# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-driver-map.R — Readings for the driver map. No invented euros, owners, or edges.
# LAST CHANGE: 2026-09-22
# A cell is either a computed figure or an explicit gap. Zero is a real figure
# when the engine produced it. A missing run is not drawn as zero.

fpa_driver_map_empty <- function() {
  data.frame(
    series_id = character(),
    series_name = character(),
    value = numeric(),
    state = character(),
    tier = character(),
    flag = character(),
    score_kind = character(),
    stringsAsFactors = FALSE
  )
}

#' One heatmap, same drivers, one question.
#' kind: score (0 to 1), partial (signed, only when measured), euro (engine only).
#' Missing rows stay in the frame, after the measured ones, with value NA.
fpa_driver_map_heat <- function(grades, kind = c("score", "partial", "euro")) {
  kind <- match.arg(kind)
  if (is.null(grades) || !nrow(grades)) return(fpa_driver_map_empty())
  n <- nrow(grades)
  value <- rep(NA_real_, n)
  state <- rep("missing", n)
  if (identical(kind, "score")) {
    value <- suppressWarnings(as.numeric(grades$score))
    state <- ifelse(is.finite(value), "value", "missing")
    key <- ifelse(state == "value", value, -Inf)
  } else if (identical(kind, "partial")) {
    raw <- suppressWarnings(as.numeric(grades$independence_r))
    measured <- as.character(grades$independence_status) == "measured" & is.finite(raw)
    value[measured] <- raw[measured]
    state[measured] <- "value"
    key <- ifelse(measured, abs(raw), -Inf)
  } else {
    raw <- suppressWarnings(as.numeric(grades$impact_eur))
    ok <- as.character(grades$impact_status) == "engine" & is.finite(raw)
    value[ok] <- raw[ok]
    state[ok] <- "value"
    key <- ifelse(state == "value", abs(value), -Inf)
  }
  ord <- order(-key, as.character(grades$series_name))
  tier <- if ("tier" %in% names(grades)) as.character(grades$tier) else rep("", n)
  flag <- if ("flag" %in% names(grades)) as.character(grades$flag) else rep("", n)
  rel <- if ("relation" %in% names(grades)) as.character(grades$relation) else rep("", n)
  measured <- if ("evidence_sig" %in% names(grades)) {
    tolower(as.character(grades$evidence_sig)) %in% c("true", "t", "1")
  } else {
    rep(FALSE, n)
  }
  if ("permutation_rank" %in% names(grades)) {
    rank <- suppressWarnings(as.numeric(grades$permutation_rank))
    measured <- measured & is.finite(rank) & rank <= 0.05
  }
  if ("lag_status" %in% names(grades)) {
    measured <- measured & as.character(grades$lag_status) != "refused_reverse_causality"
  }
  score_kind <- rep("", n)
  if (identical(kind, "score")) {
    score_kind <- ifelse(
      state != "value", "missing",
      ifelse(rel %in% c("part_whole", "derived") | tier == "mechanism", "prior",
             ifelse(measured, "mix", "held"))
    )
  }
  data.frame(
    series_id = as.character(grades$series_id)[ord],
    series_name = as.character(grades$series_name)[ord],
    value = value[ord],
    state = state[ord],
    tier = tier[ord],
    flag = flag[ord],
    score_kind = score_kind[ord],
    stringsAsFactors = FALSE
  )
}

#' Volume series the partial correlation already removed, named from the candidates.
#' NA when no series matched the volume rule.
fpa_driver_map_volume <- function(candidates, grades = NULL) {
  id <- fpa_driver_pick_volume(candidates)
  if (is.na(id)) return(list(id = NA_character_, name = ""))
  name <- ""
  if (!is.null(grades) && nrow(grades) && id %in% as.character(grades$series_id)) {
    name <- as.character(grades$series_name[match(id, as.character(grades$series_id))])
  } else if (!is.null(candidates) && nrow(candidates) && id %in% as.character(candidates$series_id)) {
    name <- as.character(candidates$series_name[match(id, as.character(candidates$series_id))])
  }
  if (!nzchar(name)) name <- as.character(id)
  list(id = as.character(id), name = name)
}

#' Flags that actually occur. A blank cell is not a flag.
fpa_driver_map_flags <- function(grades) {
  known <- c("identite", "arrive tard", "doublon", "sens inverse")
  if (is.null(grades) || !nrow(grades) || !"flag" %in% names(grades)) return(character(0))
  hit <- as.character(grades$flag)
  hit <- hit[!is.na(hit) & nzchar(hit)]
  known[known %in% hit]
}

#' Series to line. The euro comes from the grade, never from a blank edge cell.
fpa_driver_map_links <- function(grades) {
  empty <- data.frame(
    series_id = character(), series_name = character(), target_line = character(),
    lag = numeric(), lag_status = character(), r = numeric(),
    n = integer(), r_crit = numeric(), sig = logical(),
    euro = numeric(), euro_state = character(),
    stringsAsFactors = FALSE
  )
  if (is.null(grades) || !nrow(grades)) return(empty)
  euro <- suppressWarnings(as.numeric(grades$impact_eur))
  ok <- as.character(grades$impact_status) == "engine" & is.finite(euro)
  euro[!ok] <- NA_real_
  data.frame(
    series_id = as.character(grades$series_id),
    series_name = as.character(grades$series_name),
    target_line = as.character(grades$target_line),
    lag = suppressWarnings(as.numeric(grades$causal_lag)),
    lag_status = as.character(grades$lag_status),
    r = suppressWarnings(as.numeric(grades$evidence_r)),
    n = if ("n" %in% names(grades)) suppressWarnings(as.integer(grades$n)) else rep(NA_integer_, nrow(grades)),
    r_crit = if ("evidence_r_crit" %in% names(grades)) suppressWarnings(as.numeric(grades$evidence_r_crit)) else rep(NA_real_, nrow(grades)),
    sig = fpa_driver_map_as_flag(if ("evidence_sig" %in% names(grades)) grades$evidence_sig else rep(NA, nrow(grades))),
    euro = euro,
    euro_state = ifelse(ok, "value", "missing"),
    stringsAsFactors = FALSE
  )
}

#' TRUE / FALSE from a grade cell. Anything else stays NA.
fpa_driver_map_as_flag <- function(x) {
  if (is.logical(x)) return(x)
  raw <- tolower(trimws(as.character(x)))
  ifelse(raw %in% c("true", "t", "1"), TRUE, ifelse(raw %in% c("false", "f", "0"), FALSE, NA))
}

#' How an edge should read. evidence_r is already the residual (season and trend
#' removed). Below the critical |r| for that sample, the number is noise.
#' A flat series (pairs exist, correlation does not) is constant.
#' A payroll edge states the direction. The sign is the residual against a
#' cost stored as a positive number. Headcount and average cost are the
#' factors of that line: each one raises it, so the chip does not follow a
#' negative residual.
fpa_driver_map_edge_kind <- function(r, n = NA_integer_, r_crit = NA_real_, sig = NA, target_line = "", series_name = "") {
  r <- suppressWarnings(as.numeric(r))
  if (length(r) != 1L || !is.finite(r[[1]])) r <- NA_real_ else r <- r[[1]]
  n <- suppressWarnings(as.integer(n))
  if (length(n) != 1L || !is.finite(n[[1]])) n <- NA_integer_ else n <- n[[1]]
  crit <- suppressWarnings(as.numeric(r_crit))
  if (length(crit) != 1L || !is.finite(crit[[1]])) crit <- NA_real_ else crit <- crit[[1]]
  flag <- fpa_driver_map_as_flag(sig)
  if (length(flag) != 1L) flag <- NA else flag <- flag[[1]]
  line <- as.character(target_line %||% "")
  if (length(line) != 1L) line <- ""
  if (!is.finite(r)) {
    if (is.finite(n) && n >= 8L) return(list(kind = "constant", n = n, r = NA_real_))
    return(list(kind = "none", n = n, r = NA_real_))
  }
  noise <- isTRUE(!is.na(flag) && !flag) || (!isTRUE(flag) && is.finite(crit) && abs(r) + 1e-9 < crit)
  if (noise) return(list(kind = "noise", n = n, r = r))
  if (identical(line, "PAYROLL")) {
    name <- as.character(series_name %||% "")
    if (length(name) != 1L) name <- ""
    if (name %in% c("effectif", "cout_moyen")) return(list(kind = "pay_factor", n = n, r = r))
    return(list(kind = if (r < 0) "pay_down" else "pay_up", n = n, r = r))
  }
  list(kind = "r", n = n, r = r)
}

#' Text on a driver edge. Residual correlation when it was measured, euros when
#' the engine priced the series. A missing euro is omitted, not drawn as zero.
fpa_driver_map_edge_tag <- function(r, euro, euro_state) {
  bits <- character(0)
  r <- suppressWarnings(as.numeric(r))
  if (length(r) == 1L && is.finite(r)) {
    bits <- c(bits, paste0("r ", if (r > 0) "+" else "", formatC(r, format = "f", digits = 2, decimal.mark = ",")))
  }
  euro <- suppressWarnings(as.numeric(euro))
  if (identical(as.character(euro_state), "value") && length(euro) == 1L && is.finite(euro)) {
    bits <- c(bits, paste0(
      format(round(euro), big.mark = " ", scientific = FALSE, trim = TRUE),
      " EUR"
    ))
  }
  if (!length(bits)) return("")
  paste(bits, collapse = " · ")
}

#' Sum of engine euros on one target line. NA when that line has none.
fpa_driver_map_line_euro <- function(links) {
  if (is.null(links) || !nrow(links)) return(stats::setNames(numeric(0), character(0)))
  lines <- unique(as.character(links$target_line))
  out <- stats::setNames(rep(NA_real_, length(lines)), lines)
  for (ln in lines) {
    v <- suppressWarnings(as.numeric(links$euro[links$target_line == ln & links$euro_state == "value"]))
    v <- v[is.finite(v)]
    if (length(v)) out[[ln]] <- sum(v)
  }
  out
}

#' Subtotal identities of the monthly P&L. Costs are stored positive.
FPA_DRIVER_MAP_TREE <- list(
  GP = c(REV = 1, OTHER_OP_INC = 1, COGS = -1),
  EBITDA = c(GP = 1, PAYROLL = -1, OPEX_OTHER = -1),
  OI = c(EBITDA = 1, D_A = -1)
)
FPA_DRIVER_MAP_BASE <- c(REV = 1, OTHER_OP_INC = 1, COGS = -1, PAYROLL = -1, OPEX_OTHER = -1, D_A = -1)

#' Share of each part in the month-to-month spread of its subtotal:
#' cov(sign * part, node) / var(node). Parts of one node sum to 1 when the identity holds.
#' Realised months only. Returns NULL under three months.
fpa_driver_map_shares <- function(pnl, status = NULL) {
  if (is.null(pnl) || !nrow(pnl) || !"periode" %in% names(pnl)) return(NULL)
  fc <- if (!is.null(status) && nrow(status) && all(c("periode", "status") %in% names(status))) {
    as.character(status$periode[as.character(status$status) == "forecast"])
  } else character(0)
  keep <- !(as.character(pnl$periode) %in% fc)
  rev <- suppressWarnings(as.numeric(pnl$REV %||% NA))
  keep <- keep & is.finite(rev)
  pnl <- pnl[keep, , drop = FALSE]
  n <- nrow(pnl)
  col <- function(id) {
    if (id %in% names(pnl)) {
      v <- suppressWarnings(as.numeric(pnl[[id]]))
      v[!is.finite(v)] <- 0
      return(v)
    }
    parts <- FPA_DRIVER_MAP_TREE[[id]]
    if (is.null(parts)) return(rep(0, n))
    Reduce(`+`, lapply(names(parts), function(p) parts[[p]] * col(p)))
  }
  share_of <- function(parts, y) {
    vy <- if (n > 1L) stats::var(y) else NA_real_
    out <- vapply(names(parts), function(p) {
      x <- parts[[p]] * col(p)
      if (!is.finite(vy) || vy <= 0) return(NA_real_)
      stats::cov(x, y) / vy
    }, numeric(1))
    stats::setNames(out, names(parts))
  }
  if (n < 3L) return(list(n = n, ok = FALSE))
  nodes <- lapply(names(FPA_DRIVER_MAP_TREE), function(node) {
    parts <- FPA_DRIVER_MAP_TREE[[node]]
    y <- col(node)
    rebuilt <- Reduce(`+`, lapply(names(parts), function(p) parts[[p]] * col(p)))
    share <- share_of(parts, y)
    list(share = share, pct = fpa_driver_map_round_pct(share), gap = max(abs(y - rebuilt)))
  })
  names(nodes) <- names(FPA_DRIVER_MAP_TREE)
  base <- names(FPA_DRIVER_MAP_BASE)
  zero <- base[vapply(base, function(id) all(abs(col(id)) < 0.5), logical(1))]
  fixed <- base[vapply(base, function(id) {
    v <- col(id)
    !all(abs(v) < 0.5) && (max(v) - min(v)) < 0.5
  }, logical(1))]
  to_oi <- share_of(FPA_DRIVER_MAP_BASE, col("OI"))
  list(
    ok = TRUE, n = n,
    months = as.character(pnl$periode),
    nodes = nodes,
    to_oi = to_oi,
    to_oi_pct = fpa_driver_map_round_pct(to_oi),
    zero = zero,
    fixed = fixed
  )
}

#' Whole percents that add to the rounded total (largest remainder). NA stays NA.
fpa_driver_map_round_pct <- function(share) {
  out <- stats::setNames(rep(NA_integer_, length(share)), names(share))
  ok <- is.finite(share)
  if (!any(ok)) return(out)
  raw <- 100 * share[ok]
  base <- floor(raw)
  left <- as.integer(round(sum(raw)) - sum(base))
  if (left > 0L) {
    pick <- order(raw - base, decreasing = TRUE)[seq_len(min(left, length(raw)))]
    base[pick] <- base[pick] + 1
  }
  out[ok] <- as.integer(base)
  out
}

#' "82 %", "−3 %". NA stays empty.
fpa_driver_map_share_txt <- function(v) {
  v <- suppressWarnings(as.numeric(v))
  if (length(v) != 1L || !is.finite(v)) return("")
  pct <- round(100 * v)
  paste0(if (pct < 0) "\u2212" else "", format(abs(pct), big.mark = " ", trim = TRUE), " %")
}

#' Left node per series, right node per target line, y of the line at the
#' middle of the series that point to it. No edge is added.
fpa_driver_map_layout <- function(links) {
  if (is.null(links) || !nrow(links)) {
    links$y_left <- numeric()
    links$y_right <- numeric()
    return(links)
  }
  ord <- order(links$target_line, links$series_name)
  links <- links[ord, , drop = FALSE]
  links$y_left <- 70 + (seq_len(nrow(links)) - 1) * 72
  lines <- unique(links$target_line)
  y_right <- stats::setNames(rep(NA_real_, length(lines)), lines)
  for (ln in lines) {
    y_right[[ln]] <- mean(links$y_left[links$target_line == ln])
  }
  links$y_right <- unname(y_right[links$target_line])
  links
}

#' Ink for a 0–1 score. Missing stays neutral.
fpa_driver_map_mix <- function(t, from, to) {
  t <- min(1, max(0, t))
  c <- round(from + (to - from) * t)
  sprintf("#%02x%02x%02x", c[[1]], c[[2]], c[[3]])
}

#' Label printed in a cell. A gap never becomes 0.
fpa_driver_map_cell_label <- function(kind, state, value) {
  value <- suppressWarnings(as.numeric(value))
  if (!identical(as.character(state), "value") || !is.finite(value)) {
    if (identical(kind, "euro")) return("Pas chiffré")
    if (identical(kind, "partial")) return("Non mesuré")
    return("Pas de score")
  }
  if (identical(kind, "score")) return(format(round(value, 2), nsmall = 2))
  if (identical(kind, "partial")) {
    return(paste0(if (value > 0) "+" else "", format(round(value, 2), nsmall = 2)))
  }
  paste0(format(round(value), big.mark = " ", scientific = FALSE, trim = TRUE), " EUR")
}

fpa_driver_map_score_note <- function(score_kind) {
  if (identical(as.character(score_kind), "prior")) return("prior")
  if (identical(as.character(score_kind), "held")) return("sans mesure")
  ""
}

fpa_driver_map_score_ink <- function(score) {
  s <- suppressWarnings(as.numeric(score))
  if (!is.finite(s)) return(list(bg = "#f4f7f8", fg = "#5c6b73"))
  s <- min(1, max(0, s))
  list(
    bg = fpa_driver_map_mix(s, c(0xe6, 0xed, 0xef), c(0x1f, 0x4e, 0x5f)),
    fg = if (s >= 0.55) "#ffffff" else "#1a2b32"
  )
}

#' Ink for residual correlation on an edge. Strength is |r|: near 1 is strong
#' green with white type; near 0 is gray with a fainter face. The visual scale
#' is squared so a weak |r| stays gray instead of mid-green.
fpa_driver_map_r_ink <- function(r) {
  r <- suppressWarnings(as.numeric(r))
  if (!length(r) || !is.finite(r[[1]])) return(list(bg = "#f4f7f8", fg = "#9aa5ab"))
  s <- min(1, max(0, abs(r[[1]])))
  vis <- s * s
  list(
    bg = fpa_driver_map_mix(vis, c(0xe8, 0xec, 0xee), c(0x17, 0x7a, 0x4c)),
    fg = if (vis >= 0.42) "#ffffff" else if (vis >= 0.12) "#1a2b32" else "#8a969c"
  )
}

fpa_driver_map_signed_ink <- function(x, max_abs = NA_real_) {
  v <- suppressWarnings(as.numeric(x))
  if (!is.finite(v)) return(list(bg = "#f4f7f8", fg = "#5c6b73"))
  cap <- suppressWarnings(as.numeric(max_abs))
  if (!is.finite(cap) || cap <= 0) cap <- 1
  mag <- min(1, abs(v) / cap)
  if (abs(v) >= cap - 1e-9) mag <- max(mag, 0.85)
  to <- if (v < 0) c(0x8c, 0x3a, 0x3a) else c(0x1f, 0x4e, 0x5f)
  from <- if (v < 0) c(0xf8, 0xee, 0xee) else c(0xe6, 0xed, 0xef)
  list(
    bg = fpa_driver_map_mix(mag, from, to),
    fg = if (mag >= 0.45) "#ffffff" else "#1a2b32"
  )
}
