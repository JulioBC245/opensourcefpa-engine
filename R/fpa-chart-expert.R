# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-chart-expert.R — Experimental Plotly 3D notebook under Pilotage.
# The surface, the path and the bars are Plotly 3D traces.
# Every hover string is built here so a test can read it without a browser.

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(a, b) if (is.null(a) || !length(a)) b else a
}

fpa_chart_num <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("n/a")
  format(round(x[[1]]), big.mark = " ", scientific = FALSE, trim = TRUE)
}

fpa_chart_signed <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("n/a")
  paste0(if (x[[1]] > 0) "+" else "", fpa_chart_num(x))
}

fpa_chart_pct <- function(x, digits = 1, signed = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("n/a")
  mark <- if (identical(fpa_lang_current(), "en")) "." else ","
  body <- format(round(x[[1]] * 100, digits), nsmall = digits, decimal.mark = mark, trim = TRUE)
  paste0(if (signed && x[[1]] > 0) "+" else "", body, " %")
}

fpa_chart_pts <- function(x, signed = TRUE) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("n/a")
  mark <- if (identical(fpa_lang_current(), "en")) "." else ","
  body <- format(round(abs(x[[1]]) * 100, 1), nsmall = 1, decimal.mark = mark, trim = TRUE)
  sign <- if (!signed || x[[1]] == 0) "" else if (x[[1]] > 0) "+" else "-"
  paste0(sign, body, " pts")
}

fpa_chart_col <- function(df, name) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df) || !name %in% names(df)) return(numeric())
  suppressWarnings(as.numeric(df[[name]]))
}

fpa_chart_actual <- function(pnl, status) {
  periode <- as.character(pnl$periode)
  act <- rep(TRUE, length(periode))
  if (!is.null(status) && is.data.frame(status) && nrow(status) &&
      all(c("periode", "status") %in% names(status))) {
    st <- tolower(as.character(status$status))
    closed <- as.character(status$periode)[st %in% c("actual", "realise", "réalisé", "reel", "réel", "clos", "closed")]
    act <- periode %in% closed
  }
  act
}

#' Fixed and variable cost from closed months. Cost is revenue minus operating income.
fpa_chart_fit <- function(pnl, status) {
  empty <- list(ok = FALSE, n = 0L, F = NA_real_, v = NA_real_, R0 = NA_real_, oi0 = NA_real_)
  if (is.null(pnl) || !is.data.frame(pnl) || !nrow(pnl)) return(empty)
  if (!all(c("periode", "REV", "OI") %in% names(pnl))) return(empty)
  act <- fpa_chart_actual(pnl, status)
  rev <- fpa_chart_col(pnl, "REV")[act]
  oi <- fpa_chart_col(pnl, "OI")[act]
  keep <- is.finite(rev) & is.finite(oi) & rev > 0
  rev <- rev[keep]
  oi <- oi[keep]
  n <- length(rev)
  if (n < 4L) return(empty)
  cost <- rev - oi
  fit <- tryCatch(stats::lm(cost ~ rev), error = function(e) NULL)
  if (is.null(fit)) return(empty)
  coefs <- stats::coef(fit)
  if (length(coefs) < 2L || !all(is.finite(coefs))) return(empty)
  R0 <- mean(rev)
  v <- unname(coefs[[2]])
  F <- unname(coefs[[1]])
  # A negative slope means costs did not rise with revenue. Treat the cost as fixed
  # so a price cut still has a break-even volume, instead of a cost that falls as sales grow.
  if (!is.finite(v) || v < 0) {
    v <- 0
    F <- mean(cost)
  }
  if (!is.finite(v) || v >= 1) return(empty)
  list(ok = TRUE, n = n, F = F, v = v, R0 = R0, oi0 = (1 - v) * R0 - F, months = n)
}

#' Operating income after a price shock p and a volume shock q.
#' Variable cost follows volume. Price changes revenue only.
fpa_chart_oi <- function(fit, p, q) {
  fit$R0 * (1 + p) * (1 + q) - fit$F - fit$v * fit$R0 * (1 + q)
}

#' Volume points still above the break-even volume, at this price. NA when price never covers the variable cost.
fpa_chart_cushion <- function(fit, p, q) {
  gap <- (1 + p) - fit$v
  if (!is.finite(gap) || gap <= 0 || !is.finite(fit$R0) || fit$R0 <= 0) return(NA_real_)
  q_be <- fit$F / (fit$R0 * gap) - 1
  q - q_be
}

fpa_chart_be_tip <- function(fit, p, q) {
  oi <- fpa_chart_oi(fit, p, q)
  rev <- fit$R0 * (1 + p) * (1 + q)
  margin <- if (is.finite(rev) && rev != 0) oi / rev else NA_real_
  cushion <- fpa_chart_cushion(fit, p, q)
  reading <- if (!is.finite(cushion)) {
    fpa_screen("chart_tip_be_never")
  } else if (cushion >= 0) {
    fpa_screen("chart_tip_be_ok", fpa_chart_pts(cushion, signed = FALSE))
  } else {
    fpa_screen("chart_tip_be_loss", fpa_chart_pts(abs(cushion), signed = FALSE))
  }
  dist <- if (is.finite(cushion)) fpa_chart_pts(cushion) else fpa_screen("chart_tip_be_never")
  fpa_screen(
    "chart_tip_be",
    fpa_chart_pct(p), fpa_chart_pct(q),
    fpa_chart_num(oi), fpa_chart_num(rev), fpa_chart_pct(margin, signed = FALSE),
    fpa_chart_num(fit$F), fpa_chart_pct(fit$v, signed = FALSE),
    dist, reading
  )
}

fpa_chart_mon <- function(periode) {
  mo <- suppressWarnings(as.integer(substr(as.character(periode), 6L, 7L)))
  vapply(mo, function(m) {
    if (!is.finite(m) || m < 1L || m > 12L) return("")
    fpa_screen(paste0("month_s_", m))
  }, "")
}

#' Keep the first point, the last point, and points far enough apart that the words can be read.
fpa_chart_thin <- function(x, y, z, min_gap = 0.22) {
  n <- length(x)
  if (n <= 2L) return(seq_len(n))
  scale <- function(v) {
    r <- range(v, na.rm = TRUE)
    span <- diff(r)
    if (!is.finite(span) || span == 0) return(rep(0, length(v)))
    (v - r[[1]]) / span
  }
  X <- cbind(scale(x), scale(y), scale(z))
  keep <- 1L
  last <- 1L
  if (n > 2L) {
    for (i in 2:(n - 1L)) {
      d <- sqrt(sum((X[i, ] - X[last, ])^2))
      if (is.finite(d) && d >= min_gap) {
        keep <- c(keep, i)
        last <- i
      }
    }
  }
  unique(c(keep, n))
}

fpa_chart_text <- function(p, x, y, z, lab, color = "#f7f1df", size = 12, position = "top center") {
  ok <- is.finite(x) & is.finite(y) & is.finite(z) & nzchar(as.character(lab))
  if (!any(ok)) return(p)
  plotly::add_trace(
    p, x = x[ok], y = y[ok], z = z[ok],
    type = "scatter3d", mode = "text",
    text = as.character(lab[ok]),
    textfont = list(size = size, color = color, family = "Segoe UI, sans-serif"),
    textposition = position,
    hoverinfo = "skip", showlegend = FALSE, name = ""
  )
}

#' Axis window around the values, with room for a label. A flat series still gets a visible span.
fpa_chart_span <- function(v, pad = 0.22) {
  v <- v[is.finite(v)]
  if (!length(v)) return(c(0, 1))
  lo <- min(v)
  hi <- max(v)
  span <- hi - lo
  if (!is.finite(span) || span < 1e-8) span <- max(abs(hi) * 0.25, 1)
  c(lo - span * pad, hi + span * pad)
}
fpa_chart_ratio <- function(x, digits = 2) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || !is.finite(x[[1]])) return("n/a")
  mark <- if (identical(fpa_lang_current(), "en")) "." else ","
  format(round(x[[1]], digits), nsmall = digits, decimal.mark = mark, trim = TRUE)
}

fpa_chart_eq <- function(left, mid, right) {
  if (!nzchar(mid)) paste(left, "=", right) else paste(left, "=", mid, "=", right)
}

fpa_chart_eur <- function(x) paste(fpa_chart_num(x), "EUR")

fpa_chart_math_labels <- function(periods) {
  labs <- fpa_chart_mon(periods)
  if (any(duplicated(labs))) labs <- paste(labs, substr(periods, 1L, 4L))
  labs
}

fpa_chart_math_pick <- function(periods, periode) {
  periode <- as.character(periode %||% "")
  if (nzchar(periode) && periode %in% periods) periode else periods[[1]]
}

#' Cloud equations come in pairs (margin, then cash). Other charts stay one block.
fpa_chart_eq_groups <- function(which, eqs) {
  if (identical(as.character(which %||% ""), "cloud") && length(eqs) >= 2L && length(eqs) %% 2L == 0L) {
    n <- length(eqs) / 2L
    lapply(seq_len(n), function(i) eqs[c(i * 2L - 1L, i * 2L)])
  } else {
    list(eqs)
  }
}

fpa_chart_num_toks <- function(text) {
  text <- as.character(text %||% "")
  if (!nzchar(text)) return(list())
  shown <- text
  text <- gsub("\u2212", "-", text, fixed = TRUE)
  rx <- "(?:-|\\+)?(?:\\d{1,3}(?: \\d{3})+|\\d+)(?:[.,]\\d+)?(?: ?%)?"
  m <- tryCatch(gregexpr(rx, text, perl = TRUE)[[1]], error = function(e) -1L)
  if (m[[1]] < 0) return(list())
  lens <- attr(m, "match.length")
  lapply(seq_along(m), function(i) {
    tok <- substr(shown, m[[i]], m[[i]] + lens[[i]] - 1L)
    raw <- sub("%$", "", trimws(tok))
    raw <- gsub("\u2212", "-", raw, fixed = TRUE)
    raw <- gsub(" ", "", raw, fixed = TRUE)
    raw <- gsub(",", ".", raw, fixed = TRUE)
    list(
      start = m[[i]],
      end = m[[i]] + lens[[i]] - 1L,
      text = tok,
      value = suppressWarnings(as.numeric(raw))
    )
  })
}

fpa_chart_rel_gap <- function(a, b) {
  if (!is.finite(a) || !is.finite(b)) return(0)
  den <- max(abs(a), abs(b))
  if (!is.finite(den) || den < 1e-9) 0 else abs(b - a) / den
}

#' Two golds only. Rank 1 is the deeper gold. The result of an equation is never marked.
fpa_chart_hot_style <- function(rank) {
  cols <- c("rgb(201,162,39)", "rgb(232,201,120)")
  bg <- cols[[max(1L, min(2L, as.integer(rank)))]]
  sprintf("background:%s;font-weight:700;border-radius:3px;padding:0 3px;", bg)
}

#' Mark the two driver numbers that move most. The figure after the last equals stays plain.
#' The same driver keeps the same gold in both months, and wherever that pair of figures repeats.
fpa_chart_eq_marks <- function(left, right) {
  n <- min(length(left), length(right))
  empty <- list(left = list(), right = list())
  if (!n) return(empty)
  left <- as.character(left)[seq_len(n)]
  right <- as.character(right)[seq_len(n)]
  parsed <- lapply(seq_len(n), function(i) {
    list(a = fpa_chart_num_toks(left[[i]]), b = fpa_chart_num_toks(right[[i]]))
  })
  cands <- list()
  for (i in seq_len(n)) {
    a <- parsed[[i]]$a
    b <- parsed[[i]]$b
    k <- min(length(a), length(b))
    if (k < 2L) next
    for (j in seq_len(k - 1L)) {
      gap <- fpa_chart_rel_gap(a[[j]]$value, b[[j]]$value)
      if (!is.finite(gap) || gap <= 0.004) next
      key <- sprintf("%.6g|%.6g", a[[j]]$value, b[[j]]$value)
      cands[[length(cands) + 1L]] <- list(i = i, j = j, gap = gap, key = key)
    }
  }
  rank_of <- character()
  if (length(cands)) {
    keys <- unique(vapply(cands, function(c) c$key, ""))
    best <- vapply(keys, function(key) max(vapply(cands, function(c) if (identical(c$key, key)) c$gap else 0, numeric(1))), numeric(1))
    keys <- keys[order(best, decreasing = TRUE)]
    keys <- keys[seq_len(min(2L, length(keys)))]
    rank_of <- stats::setNames(seq_along(keys), keys)
  }
  style_at <- function(i, j, ntok) {
    if (j >= ntok) return("")
    hit <- Filter(function(c) c$i == i && c$j == j && c$key %in% names(rank_of), cands)
    if (!length(hit)) return("")
    fpa_chart_hot_style(rank_of[[hit[[1]]$key]])
  }
  pieces_of <- function(text, toks, i) {
    if (!length(toks)) return(list(list(text = text, hot = FALSE, style = "")))
    styles <- vapply(seq_along(toks), function(j) style_at(i, j, length(toks)), "")
    if (!any(nzchar(styles))) return(list(list(text = text, hot = FALSE, style = "")))
    out <- list()
    pos <- 1L
    for (j in seq_along(toks)) {
      if (toks[[j]]$start > pos) {
        out[[length(out) + 1L]] <- list(text = substr(text, pos, toks[[j]]$start - 1L), hot = FALSE, style = "")
      }
      out[[length(out) + 1L]] <- list(
        text = toks[[j]]$text,
        hot = nzchar(styles[[j]]),
        style = styles[[j]]
      )
      pos <- toks[[j]]$end + 1L
    }
    if (pos <= nchar(text)) {
      out[[length(out) + 1L]] <- list(text = substr(text, pos, nchar(text)), hot = FALSE, style = "")
    }
    out
  }
  list(
    left = lapply(seq_len(n), function(i) pieces_of(left[[i]], parsed[[i]]$a, i)),
    right = lapply(seq_len(n), function(i) pieces_of(right[[i]], parsed[[i]]$b, i))
  )
}

fpa_chart_year <- function(pnl, status) {
  act <- fpa_chart_actual(pnl, status)
  periode <- as.character(pnl$periode)
  closed <- periode[act]
  if (!length(closed)) return(periode)
  year <- substr(closed[[length(closed)]], 1L, 4L)
  periode[substr(periode, 1L, 4L) == year]
}

fpa_chart_slice <- function(df, periode) {
  if (is.null(df) || !is.data.frame(df) || !nrow(df) || !"periode" %in% names(df)) return(df)
  df[as.character(df$periode) %in% periode, , drop = FALSE]
}

fpa_chart_dark <- function(p, scene) {
  plotly::layout(
    p,
    paper_bgcolor = "#0f2f3a",
    plot_bgcolor = "#0f2f3a",
    font = list(color = "#e6edef", family = "Segoe UI, sans-serif"),
    margin = list(l = 8, r = 28, t = 46, b = 28),
    legend = list(
      font = list(color = "#e6edef", size = 12),
      bgcolor = "rgba(15,47,58,0.85)",
      orientation = "h",
      x = 0, xanchor = "left",
      y = 1, yanchor = "bottom"
    ),
    hoverlabel = list(
      bgcolor = "#0f2f3a",
      bordercolor = "#c9a227",
      font = list(color = "#f7f1df", size = 13, family = "Segoe UI, sans-serif"),
      align = "left",
      namelength = 0
    ),
    scene = scene
  )
}

fpa_chart_axis <- function(title, vals = NULL, text = NULL) {
  ax <- list(
    title = list(text = title, font = list(color = "#d5e2e6", size = 13)),
    tickfont = list(color = "#d5e2e6", size = 11),
    gridcolor = "#345864",
    zerolinecolor = "#c9a227",
    backgroundcolor = "#123843",
    showbackground = TRUE
  )
  if (!is.null(vals)) ax$tickvals <- vals
  if (!is.null(text)) ax$ticktext <- text
  ax
}

fpa_chart_spin <- function(p, spin) {
  htmlwidgets::onRender(p, "function(el, x) {
    function rescale() {
      var maxz = 0, n = 0;
      (el.data || []).forEach(function(tr) {
        if (tr.type !== 'mesh3d' || tr.legendgroup === 'wall') return;
        if (tr.visible === false || tr.visible === 'legendonly') return;
        var zs = tr.z || [];
        for (var i = 0; i < zs.length; i++) if (+zs[i] > maxz) maxz = +zs[i];
        n++;
      });
      if (!n || maxz <= 0) return;
      Plotly.relayout(el, {'scene.zaxis.range': [0, maxz * 1.08]});
    }
    el.on('plotly_legendclick', function() { setTimeout(rescale, 80); });
    el.on('plotly_legenddoubleclick', function() { setTimeout(rescale, 80); });
    if (el._fpaSpin) { clearInterval(el._fpaSpin); el._fpaSpin = null; }
    if (!x.spin) return;
    var ang = 0.55;
    var paused = false;
    el._fpaSpin = setInterval(function() {
      if (paused || !el.isConnected) return;
      ang += 0.035;
      Plotly.relayout(el, {'scene.camera.eye': {
        x: 1.75 * Math.cos(ang),
        y: 1.75 * Math.sin(ang),
        z: 0.72
      }});
    }, 90);
    el.on('plotly_hover', function() { paused = true; });
    el.on('plotly_unhover', function() { setTimeout(function() { paused = false; }, 900); });
  }", data = list(spin = isTRUE(spin)))
}

fpa_chart_eye <- function() list(eye = list(x = 1.65, y = 1.65, z = 0.95))

fpa_chart_scale <- function() {
  list(
    list(0, "#8c3a3a"),
    list(0.42, "#e7b3a0"),
    list(0.5, "#f4f0e6"),
    list(0.58, "#8fb8c4"),
    list(1, "#1f4e5f")
  )
}

fpa_chart_pct_axis <- function(title, lo, hi, digits = 0) {
  span <- hi - lo
  step <- if (digits >= 1) {
    raw <- if (!is.finite(span) || span <= 0) 10 else span / 4 * 100
    step_pct <- if (raw <= 6) 5 else if (raw <= 12) 10 else if (raw <= 25) 20 else if (raw <= 40) 25 else 50
    step_pct / 100
  } else if (is.finite(span) && span > 0.85) {
    0.2
  } else {
    0.1
  }
  ticks <- seq(floor(lo / step) * step, ceiling(hi / step) * step, by = step)
  ticks <- round(ticks[ticks >= lo - 1e-8 & ticks <= hi + 1e-8], 6)
  if (!length(ticks)) ticks <- c(lo, hi)
  labs <- vapply(ticks, function(v) fpa_chart_pct(v, digits = digits, signed = FALSE), "")
  ax <- fpa_chart_axis(title, ticks, labs)
  ax$tickmode <- "array"
  ax$range <- c(min(lo, min(ticks)), max(hi, max(ticks)))
  ax
}

#' Closed months set the average. Forecast months in the same year are extra points on that average.
#' `show` NULL draws every month. A character vector draws that subset. Character(0) draws none.
fpa_chart_be_points <- function(pnl, status, vol) {
  if (is.null(vol) || !is.data.frame(vol) || !nrow(vol)) return(NULL)
  if (!all(c("periode", "nuitees_vendues", "tarif") %in% names(vol))) return(NULL)
  if (is.null(pnl) || !nrow(pnl)) return(NULL)
  act <- fpa_chart_actual(pnl, status)
  per_all <- as.character(pnl$periode)
  rev <- fpa_chart_col(pnl, "REV")
  window <- fpa_chart_year(pnl, status)
  closed_p <- per_all[act & is.finite(rev) & rev > 0]
  keep_p <- per_all[per_all %in% window & is.finite(rev) & rev > 0]
  if (!length(keep_p)) return(NULL)
  m <- match(keep_p, as.character(vol$periode))
  known <- !is.na(m)
  if (!any(known)) return(NULL)
  keep_p <- keep_p[known]
  m <- m[known]
  nights <- suppressWarnings(as.numeric(vol$nuitees_vendues[m]))
  rate <- suppressWarnings(as.numeric(vol$tarif[m]))
  ok <- is.finite(nights) & nights > 0 & is.finite(rate) & rate > 0
  if (!any(ok)) return(NULL)
  per <- keep_p[ok]
  nights <- nights[ok]
  rate <- rate[ok]
  forecast <- !(per %in% closed_p)
  if (!any(!forecast)) return(NULL)
  n0 <- mean(nights[!forecast])
  r0 <- mean(rate[!forecast])
  if (!is.finite(n0) || n0 <= 0 || !is.finite(r0) || r0 <= 0) return(NULL)
  list(
    per = per, nights = nights, rate = rate, n0 = n0, r0 = r0,
    q = nights / n0 - 1, p = rate / r0 - 1, forecast = forecast
  )
}

fpa_chart_plot_be <- function(fit, spin, pnl = NULL, status = NULL, marks = NULL, show = NULL) {
  pts <- fpa_chart_be_points(pnl, status, marks)
  dot_p <- if (is.null(pts)) numeric() else pts$p
  dot_q <- if (is.null(pts)) numeric() else pts$q
  p_lo <- if (length(dot_p)) min(-0.2, dot_p) else -0.2
  p_hi <- if (length(dot_p)) max(0.2, dot_p) else 0.2
  q_lo <- if (length(dot_q)) min(-0.3, dot_q) else -0.3
  q_hi <- if (length(dot_q)) max(0.4, dot_q) else 0.4
  price <- seq(p_lo, p_hi, length.out = 17L)
  vol <- seq(q_lo, q_hi, length.out = 17L)
  z <- matrix(NA_real_, nrow = length(vol), ncol = length(price))
  text <- matrix("", nrow = length(vol), ncol = length(price))
  for (i in seq_along(vol)) {
    for (j in seq_along(price)) {
      z[i, j] <- fpa_chart_oi(fit, price[[j]], vol[[i]])
      text[i, j] <- fpa_chart_be_tip(fit, price[[j]], vol[[i]])
    }
  }
  span <- max(abs(z), na.rm = TRUE)
  if (!is.finite(span) || span <= 0) span <- 1
  z0 <- matrix(0, nrow = length(vol), ncol = length(price))
  p <- plotly::plot_ly()
  p <- plotly::add_surface(
    p, x = price, y = vol, z = z0,
    colorscale = list(list(0, "#c9a227"), list(1, "#c9a227")),
    opacity = 0.28, showscale = FALSE, hoverinfo = "skip",
    name = fpa_screen("chart_name_zero"),
    cauto = FALSE, cmin = 0, cmax = 1
  )
  p <- plotly::add_surface(
    p, x = price, y = vol, z = z, text = text, hoverinfo = "text",
    opacity = 0.4,
    colorscale = fpa_chart_scale(), cauto = FALSE, cmin = -span, cmax = span,
    contours = list(z = list(
      show = TRUE, usecolormap = FALSE, color = "#c9a227", width = 4,
      start = 0, end = 0, size = 1, project = list(z = FALSE)
    )),
    colorbar = list(title = fpa_screen("chart_ax_oi"), tickfont = list(color = "#e6edef"), titlefont = list(color = "#e6edef")),
    name = fpa_screen("chart_name_oi")
  )
  be_p <- price[(1 + price) > fit$v]
  be_q <- if (length(be_p)) fit$F / (fit$R0 * ((1 + be_p) - fit$v)) - 1 else numeric()
  keep <- is.finite(be_q) & be_q >= min(vol) & be_q <= max(vol)
  if (any(keep)) {
    p <- plotly::add_trace(
      p, x = be_p[keep], y = be_q[keep], z = rep(0, sum(keep)),
      type = "scatter3d", mode = "lines",
      line = list(color = "#c9a227", width = 8),
      name = fpa_screen("chart_name_line"),
      hoverinfo = "skip"
    )
    mid <- max(1L, ceiling(sum(keep) / 2))
    p <- fpa_chart_text(
      p, be_p[keep][mid], be_q[keep][mid], 0,
      fpa_screen("chart_name_line"), color = "#c9a227", size = 13
    )
  }
  # Cross on the dotted income surface, along average price and average volume.
  if (min(price) <= 0 && max(price) >= 0) {
    p <- plotly::add_trace(
      p, x = rep(0, length(vol)), y = vol,
      z = vapply(vol, function(q) fpa_chart_oi(fit, 0, q), numeric(1)),
      type = "scatter3d", mode = "lines",
      line = list(color = "#f7f1df", width = 3),
      name = "be-zero-cross", showlegend = FALSE, hoverinfo = "skip"
    )
  }
  if (min(vol) <= 0 && max(vol) >= 0) {
    p <- plotly::add_trace(
      p, x = price, y = rep(0, length(price)),
      z = vapply(price, function(px) fpa_chart_oi(fit, px, 0), numeric(1)),
      type = "scatter3d", mode = "lines",
      line = list(color = "#f7f1df", width = 3),
      name = "be-zero-cross", showlegend = FALSE, hoverinfo = "skip"
    )
  }
  now <- fpa_screen(
    "chart_tip_now",
    fpa_chart_num(fit$oi0), fpa_chart_num(fit$R0),
    fpa_chart_pct(if (fit$R0 > 0) fit$oi0 / fit$R0 else NA_real_, signed = FALSE),
    fpa_chart_be_tip(fit, 0, 0)
  )
  p <- plotly::add_trace(
    p, x = 0, y = 0, z = fit$oi0,
    type = "scatter3d", mode = "markers",
    marker = list(size = 8, color = "#c9a227", line = list(color = "#f7f1df", width = 1)),
    name = fpa_screen("chart_name_now"),
    text = now, hoverinfo = "text"
  )
  # A second hit target. Plotly's surface hover is thin; these points carry the same reading.
  gx <- as.vector(col(z))
  gy <- as.vector(row(z))
  p <- plotly::add_trace(
    p,
    x = price[gx], y = vol[gy], z = as.vector(z),
    type = "scatter3d", mode = "markers",
    marker = list(size = 3.5, color = as.vector(z), colorscale = fpa_chart_scale(), cmin = -span, cmax = span, opacity = 0.35, showscale = FALSE),
    text = as.vector(text), hoverinfo = "text",
    name = fpa_screen("chart_name_oi"), showlegend = FALSE
  )
  if (!is.null(pts)) {
    keep <- if (is.null(show)) rep(TRUE, length(pts$per)) else pts$per %in% as.character(show)
    add_months <- function(p, idx, fill, edge) {
      if (!length(idx)) return(p)
      tips <- vapply(idx, function(i) {
        when <- paste(fpa_chart_mon(pts$per[[i]]), substr(pts$per[[i]], 1L, 4L))
        if (isTRUE(pts$forecast[[i]])) when <- paste(when, fpa_screen("chart_st_fc"))
        fpa_screen(
          "chart_tip_month",
          when,
          fpa_chart_num(pts$nights[[i]]), fpa_chart_pct(pts$q[[i]]),
          fpa_chart_num(pts$rate[[i]]), fpa_chart_pct(pts$p[[i]]),
          fpa_chart_num(fpa_chart_oi(fit, pts$p[[i]], pts$q[[i]]))
        )
      }, "")
      plotly::add_trace(
        p,
        x = pts$p[idx], y = pts$q[idx],
        z = vapply(idx, function(i) fpa_chart_oi(fit, pts$p[[i]], pts$q[[i]]), numeric(1)),
        type = "scatter3d", mode = "markers+text",
        marker = list(size = 8, color = fill, opacity = 1, line = list(color = edge, width = 2)),
        text = fpa_chart_mon(pts$per[idx]),
        textfont = list(color = "#f7f1df", size = 12),
        textposition = "top center",
        name = "be-month",
        showlegend = FALSE,
        hovertext = tips,
        hoverinfo = "text"
      )
    }
    idx <- which(keep)
    p <- add_months(p, idx[!pts$forecast[idx]], "#3ec4ff", "#062028")
    p <- add_months(p, idx[pts$forecast[idx]], "#ffffff", "#0f2f3a")
  }
  scene <- list(
    xaxis = fpa_chart_pct_axis(fpa_screen("chart_ax_price"), p_lo, p_hi),
    yaxis = fpa_chart_pct_axis(fpa_screen("chart_ax_volume"), q_lo, q_hi),
    zaxis = fpa_chart_axis(paste0(fpa_screen("chart_ax_oi"), " (EUR)")),
    camera = fpa_chart_eye(),
    aspectmode = "cube"
  )
  fpa_chart_spin(fpa_chart_dark(p, scene), spin)
}

fpa_chart_driver_move <- function(cur, prev, labels) {
  if (is.null(prev)) return(fpa_screen("chart_tip_first"))
  d <- abs(cur - prev)
  i <- which.max(d)
  if (!length(i) || !is.finite(d[[i]])) return(fpa_screen("chart_tip_first"))
  fpa_screen("chart_tip_move", prev$when, labels[[i]], fpa_chart_pct(cur[[i]] - prev$vals[[i]]))
}

fpa_chart_plot_dupont <- function(pnl, bs, status, spin) {
  pnl <- fpa_chart_slice(pnl, fpa_chart_year(pnl, status))
  bs <- fpa_chart_slice(bs, as.character(pnl$periode))
  if (is.null(bs) || !nrow(bs) || !nrow(pnl)) return(NULL)
  m <- match(as.character(pnl$periode), as.character(bs$periode))
  if (any(is.na(m))) return(NULL)
  bs <- bs[m, , drop = FALSE]
  rev <- fpa_chart_col(pnl, "REV")
  ni <- fpa_chart_col(pnl, "NI")
  assets <- fpa_chart_col(bs, "CASH") + fpa_chart_col(bs, "AR") + fpa_chart_col(bs, "INV") +
    fpa_chart_col(bs, "PPE") + fpa_chart_col(bs, "OTHER_CA")
  equity <- fpa_chart_col(bs, "EQUITY")
  ok <- is.finite(rev) & rev > 0 & is.finite(assets) & assets > 0 & is.finite(equity) & equity != 0 & is.finite(ni)
  if (sum(ok) < 3L) return(NULL)
  margin <- ni / rev
  turn <- rev / assets
  lev <- assets / equity
  roe <- ni / equity
  act <- fpa_chart_actual(pnl, status)
  lev0 <- mean(lev[ok & act], na.rm = TRUE)
  if (!is.finite(lev0)) lev0 <- mean(lev[ok])
  mg <- seq(min(margin[ok]) - 0.02, max(margin[ok]) + 0.02, length.out = 12L)
  tv <- seq(min(turn[ok]) * 0.9, max(turn[ok]) * 1.1, length.out = 12L)
  sheet <- outer(tv, mg, function(t, m) m * t * lev0)
  p <- plotly::plot_ly()
  p <- plotly::add_surface(
    p, x = mg, y = tv, z = sheet, hoverinfo = "skip", opacity = 0.45,
    colorscale = fpa_chart_scale(), showscale = FALSE,
    name = fpa_screen("chart_ax_roe")
  )
  labels <- c(fpa_screen("chart_ax_margin"), fpa_screen("chart_ax_turn"), fpa_screen("chart_ax_roe"))
  tips <- character(sum(ok))
  idx <- which(ok)
  prev <- NULL
  for (k in seq_along(idx)) {
    i <- idx[[k]]
    cur <- c(margin[[i]], turn[[i]], lev[[i]])
    move <- if (is.null(prev)) {
      fpa_screen("chart_tip_first")
    } else {
      d <- abs(cur - prev$vals)
      j <- which.max(d)
      fpa_screen("chart_tip_move", prev$when, labels[[j]], fpa_chart_pct(cur[[j]] - prev$vals[[j]]))
    }
    tips[[k]] <- fpa_screen(
      "chart_tip_dupont",
      as.character(pnl$periode[[i]]),
      if (isTRUE(act[[i]])) fpa_screen("chart_st_act") else fpa_screen("chart_st_fc"),
      fpa_chart_pct(roe[[i]]),
      fpa_chart_pct(margin[[i]]),
      format(round(turn[[i]], 2), nsmall = 2, trim = TRUE),
      format(round(lev[[i]], 2), nsmall = 2, trim = TRUE),
      fpa_chart_num(ni[[i]]), fpa_chart_num(rev[[i]]),
      move
    )
    prev <- list(vals = cur, when = as.character(pnl$periode[[i]]))
  }
  p <- plotly::add_trace(
    p, x = margin[ok], y = turn[ok], z = roe[ok],
    type = "scatter3d", mode = "lines+markers",
    line = list(color = "#f7f1df", width = 4),
    marker = list(size = 5, color = ifelse(act[ok], "#1f4e5f", "#c9a227")),
    text = tips, hoverinfo = "text",
    name = fpa_screen("chart_name_path")
  )
  lab_i <- fpa_chart_thin(margin[ok], turn[ok], roe[ok])
  months <- fpa_chart_mon(as.character(pnl$periode[ok]))
  p <- fpa_chart_text(p, margin[ok][lab_i], turn[ok][lab_i], roe[ok][lab_i], months[lab_i])
  scene <- list(
    xaxis = fpa_chart_pct_axis(fpa_screen("chart_ax_margin"), min(mg), max(mg), digits = 1),
    yaxis = fpa_chart_axis(fpa_screen("chart_ax_turn")),
    zaxis = fpa_chart_axis(fpa_screen("chart_ax_roe")),
    camera = fpa_chart_eye(),
    aspectmode = "cube"
  )
  fpa_chart_spin(fpa_chart_dark(p, scene), spin)
}

fpa_chart_plot_terrain <- function(pnl, status, spin) {
  pnl <- fpa_chart_slice(pnl, fpa_chart_year(pnl, status))
  if (is.null(pnl) || !nrow(pnl)) return(NULL)
  ids <- c("REV", "GP", "EBITDA", "OI", "NI")
  keys <- c("chart_line_rev", "chart_line_gp", "chart_line_ebitda", "chart_line_oi", "chart_line_ni")
  if (!all(ids %in% names(pnl))) return(NULL)
  months <- as.character(pnl$periode)
  z <- sapply(ids, function(id) fpa_chart_col(pnl, id))
  if (!is.matrix(z)) z <- matrix(z, ncol = length(ids))
  rev <- fpa_chart_col(pnl, "REV")
  tips <- character()
  xx <- numeric()
  yy <- numeric()
  zz <- numeric()
  for (j in seq_along(ids)) {
    for (i in seq_along(months)) {
      share <- if (is.finite(rev[[i]]) && rev[[i]] != 0) z[i, j] / rev[[i]] else NA_real_
      prev_i <- if (i == 1L) NA_integer_ else i - 1L
      gap <- if (is.na(prev_i)) fpa_screen("chart_tip_first") else fpa_chart_signed(z[i, j] - z[prev_i, j])
      share_read <- ""
      if (!is.na(prev_i) && is.finite(rev[[prev_i]]) && rev[[prev_i]] != 0 && is.finite(share)) {
        prev_share <- z[prev_i, j] / rev[[prev_i]]
        share_read <- if (share >= prev_share) fpa_screen("chart_tip_land_share_up") else fpa_screen("chart_tip_land_share_down")
      }
      xx <- c(xx, i)
      yy <- c(yy, j)
      zz <- c(zz, z[i, j])
      tips <- c(tips, fpa_screen(
        "chart_tip_land", months[[i]], fpa_screen(keys[[j]]),
        fpa_chart_num(z[i, j]), fpa_chart_pct(share, signed = FALSE),
        if (is.na(prev_i)) months[[i]] else months[[prev_i]],
        gap, share_read
      ))
    }
  }
  surf <- t(z)
  p <- plotly::plot_ly()
  p <- plotly::add_surface(
    p, x = seq_along(months), y = seq_along(ids), z = surf,
    hoverinfo = "skip", colorscale = fpa_chart_scale(), showscale = FALSE,
    name = fpa_screen("chart_line_oi")
  )
  p <- plotly::add_trace(
    p, x = xx, y = yy, z = zz, type = "scatter3d", mode = "markers",
    marker = list(size = 4, color = zz, colorscale = fpa_chart_scale(), showscale = FALSE),
    text = tips, hoverinfo = "text", name = fpa_screen("chart_name_path"),
    showlegend = FALSE
  )
  scene <- list(
    xaxis = fpa_chart_axis(fpa_screen("chart_ax_month"), seq_along(months), months),
    yaxis = fpa_chart_axis(fpa_screen("chart_ax_line"), seq_along(ids), vapply(keys, fpa_screen, "")),
    zaxis = fpa_chart_axis(paste0(fpa_screen("chart_ax_amount"), " (EUR)")),
    camera = fpa_chart_eye(),
    aspectmode = "cube"
  )
  fpa_chart_spin(fpa_chart_dark(p, scene), spin)
}

fpa_chart_plot_cube <- function(pnl, bs, status, spin) {
  pnl <- fpa_chart_slice(pnl, fpa_chart_year(pnl, status))
  bs <- fpa_chart_slice(bs, as.character(pnl$periode))
  if (is.null(bs) || !nrow(bs) || !nrow(pnl)) return(NULL)
  m <- match(as.character(pnl$periode), as.character(bs$periode))
  if (any(is.na(m))) return(NULL)
  bs <- bs[m, , drop = FALSE]
  rev <- fpa_chart_col(pnl, "REV")
  cogs <- fpa_chart_col(pnl, "COGS")
  spend <- cogs + fpa_chart_col(pnl, "PAYROLL") + fpa_chart_col(pnl, "OPEX_OTHER")
  dso <- ifelse(rev > 0, fpa_chart_col(bs, "AR") / rev * 30, NA_real_)
  dio <- ifelse(cogs > 0, fpa_chart_col(bs, "INV") / cogs * 30, 0)
  dpo <- ifelse(spend > 0, fpa_chart_col(bs, "AP") / spend * 30, NA_real_)
  ok <- is.finite(dso) & is.finite(dio) & is.finite(dpo)
  if (sum(ok) < 3L) return(NULL)
  act <- fpa_chart_actual(pnl, status)
  xr <- fpa_chart_span(dso[ok], 0.35)
  yr <- fpa_chart_span(dio[ok], 0.45)
  zr <- fpa_chart_span(dpo[ok], 0.35)
  xs <- seq(xr[[1]], xr[[2]], length.out = 8L)
  ys <- seq(yr[[1]], yr[[2]], length.out = 8L)
  plane <- outer(ys, xs, function(a, b) a + b)
  tips <- vapply(which(ok), function(i) {
    ccc <- dso[[i]] + dio[[i]] - dpo[[i]]
    tied <- ccc / 30 * rev[[i]]
    read <- if (ccc <= 0) fpa_screen("chart_tip_cube_short") else fpa_screen("chart_tip_cube_long")
    fpa_screen(
      "chart_tip_cube",
      as.character(pnl$periode[[i]]),
      if (isTRUE(act[[i]])) fpa_screen("chart_st_act") else fpa_screen("chart_st_fc"),
      fpa_chart_num(ccc),
      fpa_chart_num(dso[[i]]), fpa_chart_num(dio[[i]]), fpa_chart_num(dpo[[i]]),
      fpa_chart_num(tied), read
    )
  }, "")
  p <- plotly::plot_ly()
  p <- plotly::add_surface(
    p, x = xs, y = ys, z = plane, hoverinfo = "skip", opacity = 0.28,
    colorscale = list(list(0, "#c9a227"), list(1, "#c9a227")),
    showscale = FALSE, name = fpa_screen("chart_name_plane"),
    cauto = FALSE, cmin = 0, cmax = 1
  )
  p <- plotly::add_trace(
    p, x = dso[ok], y = dio[ok], z = dpo[ok],
    type = "scatter3d", mode = "lines+markers",
    line = list(color = "#f7f1df", width = 4),
    marker = list(size = 5, color = ifelse((dso + dio - dpo)[ok] <= 0, "#1f4e5f", "#8c3a3a")),
    text = tips, hoverinfo = "text",
    name = fpa_screen("chart_name_path")
  )
  lab_i <- fpa_chart_thin(dso[ok], dio[ok], dpo[ok], min_gap = 0.5)
  months <- fpa_chart_mon(as.character(pnl$periode[ok]))
  p <- fpa_chart_text(
    p, dso[ok][lab_i], dio[ok][lab_i], dpo[ok][lab_i], months[lab_i],
    position = "top right"
  )
  ax <- function(title, range) {
    a <- fpa_chart_axis(title)
    a$range <- range
    a
  }
  scene <- list(
    xaxis = ax(fpa_screen("chart_ax_dso"), xr),
    yaxis = ax(fpa_screen("chart_ax_dio"), yr),
    zaxis = ax(fpa_screen("chart_ax_dpo"), zr),
    camera = list(eye = list(x = 1.15, y = -1.7, z = 0.72)),
    aspectmode = "cube"
  )
  fpa_chart_spin(fpa_chart_dark(p, scene), spin)
}

fpa_chart_box <- function(x, y, z1, w = 0.28) {
  z1 <- max(0, z1)
  vx <- c(x - w, x + w, x + w, x - w, x - w, x + w, x + w, x - w)
  vy <- c(y - w, y - w, y + w, y + w, y - w, y - w, y + w, y + w)
  vz <- c(0, 0, 0, 0, z1, z1, z1, z1)
  faces <- matrix(c(
    0, 1, 2, 0, 2, 3,
    4, 6, 5, 4, 7, 6,
    0, 4, 5, 0, 5, 1,
    1, 5, 6, 1, 6, 2,
    2, 6, 7, 2, 7, 3,
    3, 7, 4, 3, 4, 0
  ), ncol = 3, byrow = TRUE)
  list(x = vx, y = vy, z = vz, i = faces[, 1], j = faces[, 2], k = faces[, 3])
}

fpa_chart_plot_sky <- function(bs, status, spin, show = NULL) {
  bs <- fpa_chart_slice(bs, fpa_chart_year(bs, status))
  if (is.null(bs) || !nrow(bs)) return(NULL)
  ids <- c("CASH", "AR", "INV", "PPE", "AP", "DEBT", "EQUITY")
  keys <- c("chart_bs_cash", "chart_bs_ar", "chart_bs_inv", "chart_bs_ppe", "chart_bs_ap", "chart_bs_debt", "chart_bs_eq")
  cols <- c("#8fd4e2", "#3eafc6", "#1f7a94", "#14384c", "#c4a06a", "#e07a6a", "#e6c15a")
  # Assets on 1:4, a gap for the wall, financing on 6:8.
  y_at <- c(1, 2, 3, 4, 6, 7, 8)
  debt <- fpa_chart_col(bs, "ST_DEBT") + fpa_chart_col(bs, "LT_DEBT")
  mat <- cbind(
    fpa_chart_col(bs, "CASH"), fpa_chart_col(bs, "AR"), fpa_chart_col(bs, "INV"),
    fpa_chart_col(bs, "PPE"), fpa_chart_col(bs, "AP"), debt, fpa_chart_col(bs, "EQUITY")
  )
  if (!nrow(mat) || !any(is.finite(mat))) return(NULL)
  if (is.null(show)) show <- ids
  show <- as.character(show)
  months <- as.character(bs$periode)
  total <- rowSums(abs(mat), na.rm = TRUE)
  p <- plotly::plot_ly()
  for (j in seq_along(ids)) {
    if (!ids[[j]] %in% show) next
    vx <- vy <- vz <- numeric()
    tips_x <- tips_y <- tips_z <- numeric()
    tips <- character()
    ii <- jj <- kk <- numeric()
    for (i in seq_along(months)) {
      box <- fpa_chart_box(i, y_at[[j]], mat[i, j])
      off <- length(vx)
      vx <- c(vx, box$x)
      vy <- c(vy, box$y)
      vz <- c(vz, box$z)
      ii <- c(ii, box$i + off)
      jj <- c(jj, box$j + off)
      kk <- c(kk, box$k + off)
      prev <- if (i == 1L) NA_real_ else mat[i - 1L, j]
      gap <- if (!is.finite(prev)) fpa_screen("chart_tip_first") else paste0(fpa_chart_signed(mat[i, j] - prev), " EUR")
      share <- if (is.finite(total[[i]]) && total[[i]] > 0) abs(mat[i, j]) / total[[i]] else NA_real_
      tips_x <- c(tips_x, i)
      tips_y <- c(tips_y, y_at[[j]])
      tips_z <- c(tips_z, max(0, mat[i, j]))
      tips <- c(tips, fpa_screen(
        "chart_tip_sky", months[[i]], fpa_screen(keys[[j]]),
        fpa_chart_num(mat[i, j]), fpa_chart_pct(share, signed = FALSE),
        if (i == 1L) months[[i]] else months[[i - 1L]], gap
      ))
    }
    p <- plotly::add_mesh(
      p, x = vx, y = vy, z = vz, i = ii, j = jj, k = kk,
      facecolor = rep(cols[[j]], length(ii)),
      name = fpa_screen(keys[[j]]),
      legendgroup = ids[[j]],
      showlegend = TRUE,
      hoverinfo = "skip", flatshading = TRUE, opacity = 0.95,
      lighting = list(ambient = 0.65, diffuse = 0.6, specular = 0.15)
    )
    p <- plotly::add_trace(
      p, x = tips_x, y = tips_y, z = tips_z,
      type = "scatter3d", mode = "markers",
      marker = list(size = 3, color = "#f7f1df"),
      text = tips, hoverinfo = "text",
      name = fpa_screen(keys[[j]]),
      legendgroup = ids[[j]],
      showlegend = FALSE
    )
  }
  shown <- mat[, ids %in% show, drop = FALSE]
  shown <- shown[is.finite(shown)]
  zmax <- if (!length(shown)) 1 else max(0, shown) * 1.08
  wall <- list(
    x = c(0.35, length(months) + 0.65, length(months) + 0.65, 0.35),
    y = rep(5, 4),
    z = c(0, 0, zmax, zmax)
  )
  p <- plotly::add_mesh(
    p, x = wall$x, y = wall$y, z = wall$z,
    i = c(0, 0), j = c(1, 2), k = c(2, 3),
    facecolor = rep("#d5e6ea", 2),
    opacity = 0.28, flatshading = TRUE, hoverinfo = "skip",
    name = fpa_screen("chart_sky_wall"),
    legendgroup = "wall", showlegend = FALSE,
    lighting = list(ambient = 0.9, diffuse = 0.2, specular = 0.05)
  )
  p <- fpa_chart_text(p, 0.7, 2.5, 0, fpa_screen("chart_sky_assets"), size = 13, position = "top center")
  p <- fpa_chart_text(p, 0.7, 7, 0, fpa_screen("chart_sky_funding"), size = 13, position = "top center")
  zax <- fpa_chart_axis(paste0(fpa_screen("chart_ax_amount"), " (EUR)"))
  zax$range <- c(0, zmax)
  scene <- list(
    xaxis = fpa_chart_axis(fpa_screen("chart_ax_month"), seq_along(months), fpa_chart_mon(months)),
    yaxis = fpa_chart_axis(fpa_screen("chart_ax_line"), y_at, vapply(keys, fpa_screen, "")),
    zaxis = zax,
    camera = fpa_chart_eye(),
    aspectmode = "manual",
    aspectratio = list(x = 1.35, y = 1.05, z = 0.72)
  )
  fpa_chart_spin(fpa_chart_dark(p, scene), spin)
}

fpa_chart_case_key <- function(case) {
  hit <- tolower(as.character(case))
  if (hit %in% c("low", "prudent", "cautious")) return("chart_case_low")
  if (hit %in% c("high", "ambitieux", "ambitious")) return("chart_case_high")
  "chart_case_base"
}

fpa_chart_plot_cloud <- function(scenarios, spin) {
  if (is.null(scenarios) || !is.data.frame(scenarios) || !nrow(scenarios)) return(NULL)
  if (!all(c("case", "periode", "REV", "OI", "cash_close") %in% names(scenarios))) return(NULL)
  year <- substr(as.character(scenarios$periode), 1L, 4L)
  scenarios <- scenarios[year == year[[1]], , drop = FALSE]
  base_key <- vapply(scenarios$case, fpa_chart_case_key, "")
  base <- scenarios[base_key == "chart_case_base", , drop = FALSE]
  p <- plotly::plot_ly()
  colors <- c(chart_case_low = "#8c3a3a", chart_case_base = "#ffffff", chart_case_high = "#c9a227")
  # Draw the expected path last so it sits on top where the three lines share a month.
  rank <- c(chart_case_low = 1, chart_case_high = 2, chart_case_base = 3)
  for (key in c("chart_case_low", "chart_case_high", "chart_case_base")) {
    part <- scenarios[base_key == key, , drop = FALSE]
    if (!nrow(part)) next
    rev <- fpa_chart_col(part, "REV")
    oi <- fpa_chart_col(part, "OI")
    cash <- fpa_chart_col(part, "cash_close")
    margin <- ifelse(rev > 0, oi / rev, NA_real_)
    month <- as.integer(substr(as.character(part$periode), 6L, 7L))
    tips <- vapply(seq_len(nrow(part)), function(i) {
      b <- base[as.character(base$periode) == as.character(part$periode[[i]]), , drop = FALSE]
      if (nrow(b)) {
        bm <- if (b$REV[[1]] > 0) b$OI[[1]] / b$REV[[1]] else NA_real_
        d_m <- fpa_chart_pct(margin[[i]] - bm)
        d_c <- paste0(fpa_chart_signed(cash[[i]] - b$cash_close[[1]]), " EUR")
      } else {
        d_m <- "n/a"
        d_c <- "n/a"
      }
      fpa_screen(
        "chart_tip_cloud", fpa_screen(key), as.character(part$periode[[i]]),
        fpa_chart_pct(margin[[i]]), fpa_chart_num(rev[[i]]), fpa_chart_num(cash[[i]]),
        d_m, d_c
      )
    }, "")
    p <- plotly::add_trace(
      p, x = month, y = margin, z = cash,
      type = "scatter3d", mode = "lines+markers",
      line = list(color = colors[[key]], width = if (identical(key, "chart_case_base")) 7 else 4),
      marker = list(size = if (identical(key, "chart_case_base")) 6 else 4, color = colors[[key]]),
      text = tips, hoverinfo = "text",
      name = fpa_screen(key),
      legendrank = rank[[key]]
    )
    if (identical(key, "chart_case_base")) {
      lab_i <- fpa_chart_thin(month, margin, cash, min_gap = 0.48)
      lab_i <- lab_i[lab_i < length(month)]
      if (length(lab_i)) {
        p <- fpa_chart_text(
          p, month[lab_i], margin[lab_i], cash[lab_i], fpa_chart_mon(part$periode)[lab_i],
          position = "bottom center"
        )
      }
    }
    name_pos <- c(
      chart_case_low = "bottom left",
      chart_case_high = "top right",
      chart_case_base = "top left"
    )
    p <- fpa_chart_text(
      p, month[[length(month)]], margin[[length(margin)]], cash[[length(cash)]],
      fpa_screen(key), color = colors[[key]], size = 13,
      position = unname(name_pos[[key]])
    )
  }
  cash_all <- fpa_chart_col(scenarios, "cash_close")
  margin_all <- ifelse(scenarios$REV > 0, scenarios$OI / scenarios$REV, NA_real_)
  month_all <- as.integer(substr(as.character(scenarios$periode), 6L, 7L))
  xax <- fpa_chart_axis(fpa_screen("chart_ax_month"))
  xax$range <- fpa_chart_span(month_all, 0.18)
  yr <- fpa_chart_span(margin_all, 0.28)
  yax <- fpa_chart_pct_axis(fpa_screen("chart_ax_margin"), yr[[1]], yr[[2]], digits = 1)
  zax <- fpa_chart_axis(paste0(fpa_screen("chart_ax_cash"), " (EUR)"))
  zax$range <- fpa_chart_span(cash_all, 0.22)
  scene <- list(
    xaxis = xax,
    yaxis = yax,
    zaxis = zax,
    camera = fpa_chart_eye(),
    aspectmode = "cube"
  )
  fpa_chart_spin(fpa_chart_dark(p, scene), spin)
}

fpa_chart_expert_plot <- function(pnl, bs, status, scenarios, which = "be", spin = FALSE, sky = NULL, marks = NULL, be = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) return(NULL)
  which <- as.character(which %||% "be")
  fig <- if (identical(which, "dupont")) {
    fpa_chart_plot_dupont(pnl, bs, status, spin)
  } else if (identical(which, "terrain")) {
    fpa_chart_plot_terrain(pnl, status, spin)
  } else if (identical(which, "cube")) {
    fpa_chart_plot_cube(pnl, bs, status, spin)
  } else if (identical(which, "sky")) {
    fpa_chart_plot_sky(bs, status, spin, sky)
  } else if (identical(which, "cloud")) {
    fpa_chart_plot_cloud(scenarios, spin)
  } else {
    fit <- fpa_chart_fit(pnl, status)
    if (!isTRUE(fit$ok)) NULL else fpa_chart_plot_be(fit, spin, pnl, status, marks, show = be)
  }
  fig
}

fpa_chart_expert_sub <- function(pnl, bs, status, scenarios, which = "be") {
  which <- as.character(which %||% "be")
  if (identical(which, "be")) {
    fit <- fpa_chart_fit(pnl, status)
    if (!isTRUE(fit$ok)) return(fpa_screen("chart_empty"))
    return(paste(
      fpa_screen(
        "chart_sub_be", fpa_chart_num(fit$F), fpa_chart_pct(fit$v, signed = FALSE),
        fit$n, fpa_chart_num(fit$R0), fpa_chart_num(fit$oi0)
      ),
      if (fit$v == 0) fpa_screen("chart_sub_be_flat") else ""
    ))
  }
  window <- if (identical(which, "cloud")) {
    if (is.null(scenarios) || !nrow(scenarios)) character() else {
      year <- substr(as.character(scenarios$periode[[1]]), 1L, 4L)
      unique(as.character(scenarios$periode)[substr(scenarios$periode, 1L, 4L) == year])
    }
  } else if (identical(which, "sky")) {
    fpa_chart_year(bs, status)
  } else {
    fpa_chart_year(pnl, status)
  }
  if (!length(window)) return(fpa_screen("chart_empty"))
  fpa_screen("chart_sub_win", paste(window[[1]], window[[length(window)]], sep = " – "), length(unique(window)))
}

fpa_chart_expert_blurb <- function(which = "be") {
  which <- as.character(which %||% "be")
  if (!which %in% c("be", "dupont", "terrain", "cube", "sky", "cloud")) which <- "be"
  list(
    see = fpa_screen(paste0("chart_see_", which)),
    hover = fpa_screen(paste0("chart_hover_", which))
  )
}

fpa_chart_signed_join <- function(values, signs) {
  parts <- character()
  for (i in seq_along(values)) {
    bit <- fpa_chart_num(abs(values[[i]]))
    if (!length(parts)) {
      parts <- if (signs[[i]] < 0) paste0("− ", bit) else bit
    } else {
      parts <- c(parts, if (signs[[i]] < 0) "−" else "+", bit)
    }
  }
  paste(parts, collapse = " ")
}

fpa_chart_tied <- function(name, values, signs, total, unit = "EUR") {
  s <- sum(signs * values)
  right <- paste(fpa_chart_num(total), unit)
  if (is.finite(s) && is.finite(total) && abs(s - total) < 1) {
    fpa_chart_eq(name, fpa_chart_signed_join(values, signs), right)
  } else {
    fpa_chart_eq(name, "", right)
  }
}

fpa_chart_math <- function(which, pnl, bs, status, scenarios, periode = NULL, marks = NULL) {
  which <- as.character(which %||% "be")
  empty <- list(defs = character(), periods = character(), labels = character(), eqs = character(), gloss = character())
  built <- if (identical(which, "dupont")) {
    fpa_chart_math_dupont(pnl, bs, status)
  } else if (identical(which, "cube")) {
    fpa_chart_math_cube(pnl, bs, status)
  } else if (identical(which, "terrain")) {
    fpa_chart_math_terrain(pnl, status)
  } else if (identical(which, "sky")) {
    fpa_chart_math_sky(bs, status)
  } else if (identical(which, "cloud")) {
    fpa_chart_math_cloud(scenarios)
  } else {
    fpa_chart_math_be(pnl, status, marks)
  }
  if (is.null(built) || !length(built$periods)) return(empty)
  per <- fpa_chart_math_pick(built$periods, periode)
  eqs <- built$eq(per)
  gloss <- if (is.function(built$gloss)) built$gloss(per) else rep("", length(eqs))
  if (length(gloss) != length(eqs)) gloss <- rep("", length(eqs))
  list(
    defs = built$defs,
    periods = built$periods,
    labels = built$labels,
    forecast = if (is.null(built$forecast)) rep(FALSE, length(built$periods)) else as.logical(built$forecast),
    eqs = eqs,
    gloss = gloss
  )
}

fpa_chart_math_dupont <- function(pnl, bs, status) {
  pnl <- fpa_chart_slice(pnl, fpa_chart_year(pnl, status))
  bs <- fpa_chart_slice(bs, as.character(pnl$periode))
  if (is.null(bs) || !nrow(bs) || !nrow(pnl)) return(NULL)
  m <- match(as.character(pnl$periode), as.character(bs$periode))
  if (any(is.na(m))) return(NULL)
  bs <- bs[m, , drop = FALSE]
  rev <- fpa_chart_col(pnl, "REV")
  ni <- fpa_chart_col(pnl, "NI")
  assets <- fpa_chart_col(bs, "CASH") + fpa_chart_col(bs, "AR") + fpa_chart_col(bs, "INV") +
    fpa_chart_col(bs, "PPE") + fpa_chart_col(bs, "OTHER_CA")
  equity <- fpa_chart_col(bs, "EQUITY")
  ok <- is.finite(rev) & rev > 0 & is.finite(assets) & assets > 0 & is.finite(equity) & equity != 0 & is.finite(ni)
  if (!any(ok)) return(NULL)
  periods <- as.character(pnl$periode)[ok]
  list(
    defs = c(
      fpa_screen("chart_def_margin"),
      fpa_screen("chart_def_turn"),
      fpa_screen("chart_def_roe")
    ),
    periods = periods,
    labels = fpa_chart_math_labels(periods),
    eq = function(per) {
      i <- match(per, as.character(pnl$periode))
      c(
        fpa_chart_eq(
          fpa_screen("chart_ax_margin"),
          paste(fpa_chart_num(ni[[i]]), "/", fpa_chart_num(rev[[i]])),
          fpa_chart_pct(ni[[i]] / rev[[i]], signed = FALSE)
        ),
        fpa_chart_eq(
          fpa_screen("chart_ax_turn"),
          paste(fpa_chart_num(rev[[i]]), "/", fpa_chart_num(assets[[i]])),
          fpa_chart_ratio(rev[[i]] / assets[[i]])
        ),
        fpa_chart_eq(
          fpa_screen("chart_ax_roe"),
          paste(fpa_chart_num(ni[[i]]), "/", fpa_chart_num(equity[[i]])),
          fpa_chart_pct(ni[[i]] / equity[[i]], signed = FALSE)
        )
      )
    },
    gloss = function(per) {
      i <- match(per, as.character(pnl$periode))
      c(
        fpa_screen("chart_gloss_margin", fpa_chart_num(ni[[i]]), fpa_chart_num(rev[[i]]), fpa_chart_pct(ni[[i]] / rev[[i]], signed = FALSE)),
        fpa_screen("chart_gloss_turn", fpa_chart_num(rev[[i]]), fpa_chart_num(assets[[i]]), fpa_chart_ratio(rev[[i]] / assets[[i]])),
        fpa_screen("chart_gloss_roe", fpa_chart_num(ni[[i]]), fpa_chart_num(equity[[i]]), fpa_chart_pct(ni[[i]] / equity[[i]], signed = FALSE))
      )
    }
  )
}

fpa_chart_math_cube <- function(pnl, bs, status) {
  pnl <- fpa_chart_slice(pnl, fpa_chart_year(pnl, status))
  bs <- fpa_chart_slice(bs, as.character(pnl$periode))
  if (is.null(bs) || !nrow(bs) || !nrow(pnl)) return(NULL)
  m <- match(as.character(pnl$periode), as.character(bs$periode))
  if (any(is.na(m))) return(NULL)
  bs <- bs[m, , drop = FALSE]
  rev <- fpa_chart_col(pnl, "REV")
  cogs <- fpa_chart_col(pnl, "COGS")
  spend <- cogs + fpa_chart_col(pnl, "PAYROLL") + fpa_chart_col(pnl, "OPEX_OTHER")
  dso <- ifelse(rev > 0, fpa_chart_col(bs, "AR") / rev * 30, NA_real_)
  dio <- ifelse(cogs > 0, fpa_chart_col(bs, "INV") / cogs * 30, 0)
  dpo <- ifelse(spend > 0, fpa_chart_col(bs, "AP") / spend * 30, NA_real_)
  ok <- is.finite(dso) & is.finite(dio) & is.finite(dpo)
  if (!any(ok)) return(NULL)
  periods <- as.character(pnl$periode)[ok]
  list(
    defs = c(
      fpa_screen("chart_def_dso"),
      fpa_screen("chart_def_dio"),
      fpa_screen("chart_def_dpo")
    ),
    periods = periods,
    labels = fpa_chart_math_labels(periods),
    eq = function(per) {
      i <- match(per, as.character(pnl$periode))
      days <- function(x) paste(fpa_chart_ratio(x, 1), fpa_screen("chart_math_days"))
      c(
        fpa_chart_eq(
          fpa_screen("chart_ax_dso"),
          paste(fpa_chart_num(fpa_chart_col(bs, "AR")[[i]]), "/", fpa_chart_num(rev[[i]]), "× 30"),
          days(dso[[i]])
        ),
        fpa_chart_eq(
          fpa_screen("chart_ax_dio"),
          paste(fpa_chart_num(fpa_chart_col(bs, "INV")[[i]]), "/", fpa_chart_num(cogs[[i]]), "× 30"),
          days(dio[[i]])
        ),
        fpa_chart_eq(
          fpa_screen("chart_ax_dpo"),
          paste(fpa_chart_num(fpa_chart_col(bs, "AP")[[i]]), "/", fpa_chart_num(spend[[i]]), "× 30"),
          days(dpo[[i]])
        )
      )
    },
    gloss = function(per) {
      i <- match(per, as.character(pnl$periode))
      c(
        fpa_screen("chart_gloss_dso", fpa_chart_num(fpa_chart_col(bs, "AR")[[i]]), fpa_chart_num(rev[[i]]), paste(fpa_chart_ratio(dso[[i]], 1), fpa_screen("chart_math_days"))),
        fpa_screen("chart_gloss_dio", fpa_chart_num(fpa_chart_col(bs, "INV")[[i]]), fpa_chart_num(cogs[[i]]), paste(fpa_chart_ratio(dio[[i]], 1), fpa_screen("chart_math_days"))),
        fpa_screen("chart_gloss_dpo", fpa_chart_num(fpa_chart_col(bs, "AP")[[i]]), fpa_chart_num(spend[[i]]), paste(fpa_chart_ratio(dpo[[i]], 1), fpa_screen("chart_math_days")))
      )
    }
  )
}

fpa_chart_math_terrain <- function(pnl, status) {
  pnl <- fpa_chart_slice(pnl, fpa_chart_year(pnl, status))
  if (is.null(pnl) || !nrow(pnl)) return(NULL)
  need <- c("REV", "GP", "EBITDA", "OI", "NI", "COGS", "PAYROLL", "OPEX_OTHER", "D_A")
  if (!all(need %in% names(pnl))) return(NULL)
  periods <- as.character(pnl$periode)
  list(
    defs = c(
      fpa_screen("chart_def_month"),
      fpa_screen("chart_def_line"),
      fpa_screen("chart_def_amount")
    ),
    periods = periods,
    labels = fpa_chart_math_labels(periods),
    eq = function(per) {
      i <- match(per, periods)
      rev <- fpa_chart_col(pnl, "REV")[[i]]
      cogs <- fpa_chart_col(pnl, "COGS")[[i]]
      gp <- fpa_chart_col(pnl, "GP")[[i]]
      pay <- fpa_chart_col(pnl, "PAYROLL")[[i]]
      opex <- fpa_chart_col(pnl, "OPEX_OTHER")[[i]]
      ebitda <- fpa_chart_col(pnl, "EBITDA")[[i]]
      da <- fpa_chart_col(pnl, "D_A")[[i]]
      oi <- fpa_chart_col(pnl, "OI")[[i]]
      ni <- fpa_chart_col(pnl, "NI")[[i]]
      c(
        fpa_chart_eq(fpa_screen("chart_line_rev"), "", fpa_chart_eur(rev)),
        fpa_chart_tied(fpa_screen("chart_line_gp"), c(rev, cogs), c(1, -1), gp),
        fpa_chart_tied(fpa_screen("chart_line_ebitda"), c(gp, pay, opex), c(1, -1, -1), ebitda),
        fpa_chart_tied(fpa_screen("chart_line_oi"), c(ebitda, da), c(1, -1), oi),
        fpa_chart_eq(fpa_screen("chart_line_ni"), "", fpa_chart_eur(ni))
      )
    },
    gloss = function(per) {
      i <- match(per, periods)
      rev <- fpa_chart_col(pnl, "REV")[[i]]
      cogs <- fpa_chart_col(pnl, "COGS")[[i]]
      gp <- fpa_chart_col(pnl, "GP")[[i]]
      pay <- fpa_chart_col(pnl, "PAYROLL")[[i]]
      opex <- fpa_chart_col(pnl, "OPEX_OTHER")[[i]]
      ebitda <- fpa_chart_col(pnl, "EBITDA")[[i]]
      da <- fpa_chart_col(pnl, "D_A")[[i]]
      oi <- fpa_chart_col(pnl, "OI")[[i]]
      ni <- fpa_chart_col(pnl, "NI")[[i]]
      c(
        fpa_screen("chart_gloss_rev", fpa_chart_eur(rev)),
        fpa_screen("chart_gloss_gp", fpa_chart_num(rev), fpa_chart_num(cogs), fpa_chart_eur(gp)),
        fpa_screen("chart_gloss_ebitda", fpa_chart_num(gp), fpa_chart_num(pay), fpa_chart_num(opex), fpa_chart_eur(ebitda)),
        fpa_screen("chart_gloss_oi_line", fpa_chart_num(ebitda), fpa_chart_num(da), fpa_chart_eur(oi)),
        fpa_screen("chart_gloss_ni", fpa_chart_eur(ni))
      )
    }
  )
}

fpa_chart_math_sky <- function(bs, status) {
  bs <- fpa_chart_slice(bs, fpa_chart_year(bs, status))
  if (is.null(bs) || !nrow(bs)) return(NULL)
  periods <- as.character(bs$periode)
  list(
    defs = c(
      fpa_screen("chart_def_month"),
      fpa_screen("chart_def_side"),
      fpa_screen("chart_def_amount")
    ),
    periods = periods,
    labels = fpa_chart_math_labels(periods),
    eq = function(per) {
      i <- match(per, periods)
      cash <- fpa_chart_col(bs, "CASH")[[i]]
      ar <- fpa_chart_col(bs, "AR")[[i]]
      inv <- fpa_chart_col(bs, "INV")[[i]]
      ppe <- fpa_chart_col(bs, "PPE")[[i]]
      ap <- fpa_chart_col(bs, "AP")[[i]]
      debt <- fpa_chart_col(bs, "ST_DEBT")[[i]] + fpa_chart_col(bs, "LT_DEBT")[[i]]
      eqy <- fpa_chart_col(bs, "EQUITY")[[i]]
      c(
        fpa_chart_eq(fpa_screen("chart_bs_cash"), "", fpa_chart_eur(cash)),
        fpa_chart_eq(fpa_screen("chart_bs_ar"), "", fpa_chart_eur(ar)),
        fpa_chart_eq(fpa_screen("chart_bs_inv"), "", fpa_chart_eur(inv)),
        fpa_chart_eq(fpa_screen("chart_bs_ppe"), "", fpa_chart_eur(ppe)),
        fpa_chart_tied(fpa_screen("chart_sky_assets"), c(cash, ar, inv, ppe), c(1, 1, 1, 1), cash + ar + inv + ppe),
        fpa_chart_eq(fpa_screen("chart_bs_ap"), "", fpa_chart_eur(ap)),
        fpa_chart_eq(fpa_screen("chart_bs_debt"), "", fpa_chart_eur(debt)),
        fpa_chart_eq(fpa_screen("chart_bs_eq"), "", fpa_chart_eur(eqy)),
        fpa_chart_tied(fpa_screen("chart_sky_funding"), c(ap, debt, eqy), c(1, 1, 1), ap + debt + eqy)
      )
    },
    gloss = function(per) {
      i <- match(per, periods)
      cash <- fpa_chart_col(bs, "CASH")[[i]]
      ar <- fpa_chart_col(bs, "AR")[[i]]
      inv <- fpa_chart_col(bs, "INV")[[i]]
      ppe <- fpa_chart_col(bs, "PPE")[[i]]
      ap <- fpa_chart_col(bs, "AP")[[i]]
      debt <- fpa_chart_col(bs, "ST_DEBT")[[i]] + fpa_chart_col(bs, "LT_DEBT")[[i]]
      eqy <- fpa_chart_col(bs, "EQUITY")[[i]]
      c(
        fpa_screen("chart_gloss_bs_cash", fpa_chart_eur(cash)),
        fpa_screen("chart_gloss_bs_ar", fpa_chart_eur(ar)),
        fpa_screen("chart_gloss_bs_inv", fpa_chart_eur(inv)),
        fpa_screen("chart_gloss_bs_ppe", fpa_chart_eur(ppe)),
        fpa_screen("chart_gloss_sky_assets", fpa_chart_num(cash), fpa_chart_num(ar), fpa_chart_num(inv), fpa_chart_num(ppe), fpa_chart_eur(cash + ar + inv + ppe)),
        fpa_screen("chart_gloss_bs_ap", fpa_chart_eur(ap)),
        fpa_screen("chart_gloss_bs_debt", fpa_chart_eur(debt)),
        fpa_screen("chart_gloss_bs_eq", fpa_chart_eur(eqy)),
        fpa_screen("chart_gloss_sky_fund", fpa_chart_num(ap), fpa_chart_num(debt), fpa_chart_num(eqy), fpa_chart_eur(ap + debt + eqy))
      )
    }
  )
}

fpa_chart_math_cloud <- function(scenarios) {
  if (is.null(scenarios) || !nrow(scenarios)) return(NULL)
  if (!all(c("case", "periode", "REV", "OI", "cash_close") %in% names(scenarios))) return(NULL)
  year <- substr(as.character(scenarios$periode), 1L, 4L)
  scenarios <- scenarios[year == year[[1]], , drop = FALSE]
  periods <- unique(as.character(scenarios$periode))
  if (!length(periods)) return(NULL)
  list(
    defs = c(
      fpa_screen("chart_def_month"),
      fpa_screen("chart_def_margin"),
      fpa_screen("chart_def_cash")
    ),
    periods = periods,
    labels = fpa_chart_math_labels(periods),
    eq = function(per) {
      part <- scenarios[as.character(scenarios$periode) == per, , drop = FALSE]
      keys <- vapply(part$case, fpa_chart_case_key, "")
      ord <- order(match(keys, c("chart_case_base", "chart_case_low", "chart_case_high")))
      part <- part[ord, , drop = FALSE]
      keys <- keys[ord]
      unlist(lapply(seq_len(nrow(part)), function(i) {
        rev <- fpa_chart_col(part, "REV")[[i]]
        oi <- fpa_chart_col(part, "OI")[[i]]
        cash <- fpa_chart_col(part, "cash_close")[[i]]
        who <- fpa_screen(keys[[i]])
        margin <- if (is.finite(rev) && rev != 0) oi / rev else NA_real_
        c(
          fpa_chart_eq(
            paste(who, fpa_screen("chart_ax_margin")),
            paste(fpa_chart_num(oi), "/", fpa_chart_num(rev)),
            fpa_chart_pct(margin, signed = FALSE)
          ),
          fpa_chart_eq(paste(who, fpa_screen("chart_ax_cash")), "", fpa_chart_eur(cash))
        )
      }))
    },
    gloss = function(per) {
      part <- scenarios[as.character(scenarios$periode) == per, , drop = FALSE]
      keys <- vapply(part$case, fpa_chart_case_key, "")
      ord <- order(match(keys, c("chart_case_base", "chart_case_low", "chart_case_high")))
      part <- part[ord, , drop = FALSE]
      unlist(lapply(seq_len(nrow(part)), function(i) {
        rev <- fpa_chart_col(part, "REV")[[i]]
        oi <- fpa_chart_col(part, "OI")[[i]]
        cash <- fpa_chart_col(part, "cash_close")[[i]]
        margin <- if (is.finite(rev) && rev != 0) oi / rev else NA_real_
        c(
          fpa_screen("chart_gloss_cloud_m", fpa_chart_num(oi), fpa_chart_num(rev), fpa_chart_pct(margin, signed = FALSE)),
          fpa_screen("chart_gloss_cloud_c", fpa_chart_eur(cash))
        )
      }))
    }
  )
}

fpa_chart_math_be <- function(pnl, status, marks) {
  fit <- fpa_chart_fit(pnl, status)
  if (!isTRUE(fit$ok)) return(NULL)
  defs <- c(
    fpa_screen("chart_def_price"),
    fpa_screen("chart_def_volume"),
    fpa_screen("chart_def_oi")
  )
  centre <- function() {
    c(
      fpa_chart_eq(fpa_screen("chart_ax_price"), "", fpa_chart_pct(0, digits = 0, signed = FALSE)),
      fpa_chart_eq(fpa_screen("chart_ax_volume"), "", fpa_chart_pct(0, digits = 0, signed = FALSE)),
      fpa_chart_eq(
        fpa_screen("chart_ax_oi"),
        paste(fpa_chart_num(fit$R0), "× (1 −", fpa_chart_pct(fit$v, signed = FALSE), ") −", fpa_chart_num(fit$F)),
        fpa_chart_eur(fit$oi0)
      )
    )
  }
  gloss_centre <- function() {
    c(
      fpa_screen("chart_gloss_price_zero"),
      fpa_screen("chart_gloss_volume_zero"),
      fpa_screen(
        "chart_gloss_oi_centre",
        fpa_chart_num(fit$R0), fpa_chart_pct(fit$v, signed = FALSE),
        fpa_chart_num(fit$F), fpa_chart_eur(fit$oi0)
      )
    )
  }
  pts <- fpa_chart_be_points(pnl, status, marks)
  if (is.null(pts)) {
    return(list(
      defs = defs,
      periods = "centre",
      labels = fpa_screen("chart_math_centre"),
      eq = function(per) centre(),
      gloss = function(per) gloss_centre()
    ))
  }
  list(
    defs = defs,
    periods = pts$per,
    labels = fpa_chart_math_labels(pts$per),
    forecast = pts$forecast,
    eq = function(when) {
      i <- match(when, pts$per)
      if (is.na(i)) return(centre())
      p <- pts$p[[i]]
      q <- pts$q[[i]]
      oi <- fpa_chart_oi(fit, p, q)
      mid <- paste(
        fpa_chart_num(fit$R0), "×", fpa_chart_ratio(1 + p), "×", fpa_chart_ratio(1 + q),
        "−", fpa_chart_num(fit$F)
      )
      if (fit$v > 0) {
        mid <- paste(mid, "−", fpa_chart_num(fit$v * fit$R0 * (1 + q)))
      }
      c(
        fpa_chart_eq(
          fpa_screen("chart_ax_price"),
          paste(fpa_chart_num(pts$rate[[i]]), "/", fpa_chart_num(pts$r0), "− 1"),
          fpa_chart_pct(p)
        ),
        fpa_chart_eq(
          fpa_screen("chart_ax_volume"),
          paste(fpa_chart_num(pts$nights[[i]]), "/", fpa_chart_num(pts$n0), "− 1"),
          fpa_chart_pct(q)
        ),
        fpa_chart_eq(fpa_screen("chart_ax_oi"), mid, fpa_chart_eur(oi))
      )
    },
    gloss = function(when) {
      i <- match(when, pts$per)
      if (is.na(i)) return(gloss_centre())
      p <- pts$p[[i]]
      q <- pts$q[[i]]
      oi <- fpa_chart_oi(fit, p, q)
      price <- fpa_screen(
        "chart_gloss_price",
        fpa_chart_num(pts$rate[[i]]), fpa_chart_num(pts$r0), fpa_chart_pct(p)
      )
      volume <- fpa_screen(
        "chart_gloss_volume",
        fpa_chart_num(pts$nights[[i]]), fpa_chart_num(pts$n0), fpa_chart_pct(q)
      )
      income <- if (fit$v > 0) {
        fpa_screen(
          "chart_gloss_oi_var",
          fpa_chart_num(fit$R0), fpa_chart_ratio(1 + p), fpa_chart_ratio(1 + q),
          fpa_chart_num(fit$F), fpa_chart_num(fit$v * fit$R0 * (1 + q)), fpa_chart_eur(oi)
        )
      } else {
        fpa_screen(
          "chart_gloss_oi",
          fpa_chart_num(fit$R0), fpa_chart_ratio(1 + p), fpa_chart_ratio(1 + q),
          fpa_chart_num(fit$F), fpa_chart_eur(oi)
        )
      }
      c(price, volume, income)
    }
  )
}
