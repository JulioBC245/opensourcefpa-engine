# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# fpa-bs.R — Rolling balance sheet + equity roll; A = L + E assertion (no plug)
# LAST CHANGE: 2026-10-12 — compare two balance-sheet dates

#' Build monthly BS from opening, P&L, WC, cash bridge, capex, debt.
#' Cash comes from the bridge (never plugged).
#' Equity_t = Equity_{t-1} + NI_t
#' PPE_t = PPE_{t-1} + Capex_t - D&A_t
#' Debt_t = Debt_{t-1} + draws - repayments
fpa_build_bs <- function(pnl, wc, cash_bridge, capex, debt, opening,
                         other_ca = NULL, other_cl = NULL) {
  periods <- pnl$periode
  ids <- fpa_bs_input_ids()
  out <- data.frame(periode = periods, stringsAsFactors = FALSE)
  for (id in ids) out[[id]] <- NA_real_

  eq <- fpa_open_num(opening, "EQUITY")
  ppe <- fpa_open_num(opening, "PPE")
  st <- fpa_open_num(opening, "ST_DEBT")
  lt <- fpa_open_num(opening, "LT_DEBT")
  oca <- fpa_open_num(opening, "OTHER_CA")
  ocl <- fpa_open_num(opening, "OTHER_CL")

  draws <- debt$draw
  repays <- debt$repay
  breaks <- debt$break_cost

  for (i in seq_along(periods)) {
    p <- periods[[i]]
    out$CASH[[i]] <- cash_bridge$cash_close[[i]]
    out$AR[[i]] <- wc$AR[[p]]
    out$INV[[i]] <- wc$INV[[p]]
    out$AP[[i]] <- wc$AP[[p]]
    ppe <- ppe + (capex[[p]] %||% 0) - pnl$D_A[[i]]
    out$PPE[[i]] <- ppe
    # Debt: apply draws to LT by default; repayments reduce LT then ST
    d_in <- draws[[p]] %||% 0
    d_out <- repays[[p]] %||% 0
    lt <- lt + d_in
    pay <- d_out
    if (pay > 0) {
      from_lt <- min(lt, pay)
      lt <- lt - from_lt
      pay <- pay - from_lt
      st <- max(0, st - pay)
    }
    out$ST_DEBT[[i]] <- max(0, st)
    out$LT_DEBT[[i]] <- max(0, lt)
    out$OTHER_CA[[i]] <- if (!is.null(other_ca)) other_ca[[p]] %||% oca else oca
    out$OTHER_CL[[i]] <- if (!is.null(wc$OTHER_CL)) wc$OTHER_CL[[p]] else if (!is.null(other_cl)) other_cl[[p]] %||% ocl else ocl
    # Break cost hits cash via CFF; absorb in equity so A = L + E holds (no plug).
    bc <- breaks[[p]] %||% 0
    eq <- eq + pnl$NI[[i]] - bc
    out$EQUITY[[i]] <- eq
  }
  out
}

#' Assert A = L + E for every month. Returns findings-style list.
fpa_assert_identity <- function(bs, tol = 1) {
  gaps <- numeric(0)
  msgs <- character(0)
  for (i in seq_len(nrow(bs))) {
    assets <- bs$CASH[[i]] + bs$AR[[i]] + bs$INV[[i]] + bs$PPE[[i]] + bs$OTHER_CA[[i]]
    liab <- bs$AP[[i]] + bs$ST_DEBT[[i]] + bs$LT_DEBT[[i]] + bs$OTHER_CL[[i]]
    eq <- bs$EQUITY[[i]]
    gap <- assets - (liab + eq)
    gaps <- c(gaps, gap)
    if (abs(gap) > tol) {
      msgs <- c(msgs, sprintf(
        "%s: ecart A-(L+E)=%.2f (A=%.2f L=%.2f E=%.2f)",
        bs$periode[[i]], gap, assets, liab, eq
      ))
    }
  }
  list(ok = length(msgs) == 0, gaps = gaps, messages = msgs)
}

#' Fixed reading order. Assets stay a group. Liabilities and equity stay the other group.
fpa_bs_side_ids <- function() {
  list(
    asset = c("PPE", "CASH", "AR", "INV", "OTHER_CA"),
    claim = c("EQUITY", "LT_DEBT", "AP", "OTHER_CL", "ST_DEBT")
  )
}

fpa_bs_amount_at <- function(bs, period, id) {
  if (is.null(bs) || !nrow(bs) || !id %in% names(bs)) return(NA_real_)
  i <- match(as.character(period), as.character(bs$periode))
  if (is.na(i)) return(NA_real_)
  v <- suppressWarnings(as.numeric(bs[[id]][[i]]))
  if (length(v) != 1L || !is.finite(v)) return(NA_real_)
  v
}

#' One row per balance-sheet line, from period A to period B.
#' Shares are the line's place in its own side. Deltas on a side sum to the side's change.
fpa_bs_compare <- function(bs, period_a, period_b) {
  empty_lines <- data.frame(
    id = character(), side = character(),
    amount_a = numeric(), amount_b = numeric(), delta = numeric(),
    pct = numeric(), share_a = numeric(), share_b = numeric(), share_pts = numeric(),
    move = character(), stringsAsFactors = FALSE
  )
  empty_sides <- data.frame(
    side = character(), total_a = numeric(), total_b = numeric(), delta = numeric(),
    pct = numeric(), stringsAsFactors = FALSE
  )
  out <- list(
    period_a = as.character(period_a %||% ""),
    period_b = as.character(period_b %||% ""),
    lines = empty_lines,
    sides = empty_sides
  )
  if (is.null(bs) || !nrow(bs)) return(out)
  sides <- fpa_bs_side_ids()
  amt <- function(period, id) {
    v <- fpa_bs_amount_at(bs, period, id)
    if (!is.finite(v)) 0 else v
  }
  side_total <- function(period, ids) sum(vapply(ids, function(id) amt(period, id), numeric(1)))
  one_side <- function(side, ids) {
    tot_a <- side_total(period_a, ids)
    tot_b <- side_total(period_b, ids)
    rows <- lapply(ids, function(id) {
      a <- amt(period_a, id)
      b <- amt(period_b, id)
      delta <- b - a
      alive_a <- abs(a) >= 0.5
      alive_b <- abs(b) >= 0.5
      if (!alive_a && !alive_b) return(NULL)
      move <- if (!alive_a && alive_b) {
        "new"
      } else if (alive_a && !alive_b) {
        "gone"
      } else if (delta > 0.5) {
        "up"
      } else if (delta < -0.5) {
        "down"
      } else {
        "flat"
      }
      pct <- if (alive_a && move != "gone") 100 * delta / abs(a) else NA_real_
      share <- function(v, tot) if (abs(tot) < 0.5) NA_real_ else v / tot
      sa <- share(a, tot_a)
      sb <- share(b, tot_b)
      pts <- if (is.finite(sa) && is.finite(sb)) 100 * (sb - sa) else NA_real_
      data.frame(
        id = id, side = side,
        amount_a = a, amount_b = b, delta = delta,
        pct = pct, share_a = sa, share_b = sb, share_pts = pts,
        move = move, stringsAsFactors = FALSE
      )
    })
    rows <- rows[!vapply(rows, is.null, logical(1))]
    lines <- if (length(rows)) do.call(rbind, rows) else empty_lines
    side_pct <- if (abs(tot_a) >= 0.5) 100 * (tot_b - tot_a) / abs(tot_a) else NA_real_
    list(
      lines = lines,
      side = data.frame(
        side = side, total_a = tot_a, total_b = tot_b,
        delta = tot_b - tot_a, pct = side_pct, stringsAsFactors = FALSE
      )
    )
  }
  asset <- one_side("asset", sides$asset)
  claim <- one_side("claim", sides$claim)
  out$lines <- rbind(asset$lines, claim$lines)
  out$sides <- rbind(asset$side, claim$side)
  rownames(out$lines) <- NULL
  rownames(out$sides) <- NULL
  out
}

#' Deliberately break identity (for gate 6): add phantom amount to OTHER_CA without funding.
fpa_bs_break_identity <- function(bs, amount = 1000) {
  bs2 <- bs
  bs2$OTHER_CA <- bs2$OTHER_CA + amount
  bs2
}
