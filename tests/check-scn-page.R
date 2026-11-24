# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
# The Scénarios tab: month rows, chosen path, totals and cash must tie out.
root <- "."
source(file.path(root, "R/fpa-copy.R"), encoding = "UTF-8")
source(file.path(root, "R/fpa-scenarios.R"), encoding = "UTF-8")
source(file.path(root, "R/fpa-match.R"), encoding = "UTF-8")
source(file.path(root, "R/fpa-scn-page.R"), encoding = "UTF-8")
if (!requireNamespace("shiny", quietly = TRUE)) stop("shiny missing")

open <- c("2026-10", "2026-11", "2026-12")
pl <- function(oi) data.frame(periode = c("2026-09", open), OI = c(500, oi), stringsAsFactors = FALSE)
cash <- function(x) data.frame(periode = c("2026-09", open), cash_close = c(30000, x), stringsAsFactors = FALSE)
v <- list(
  open = open, ready = TRUE,
  cases = c(`2026-10` = "low", `2026-11` = "base", `2026-12` = "high"),
  low = pl(c(-4000, -3000, -4875)),
  base = pl(c(-2500, -2000, -2619)),
  high = pl(c(-1000, -500, -503)),
  pnl = pl(c(-4000, -2000, -503)),
  budget = data.frame(periode = open, OI = c(-3500, -3500, -3525), stringsAsFactors = FALSE),
  cash = cash(c(31000, 32000, 36001)),
  cash_low = cash(c(31000, 30000, 31000)),
  cash_high = cash(c(33000, 37000, 42215))
)
cash_base <- cash(c(32000, 34000, 37099))

rows <- fpa_scn_page_rows(v)
html <- paste(as.character(fpa_scn_page_result(v, cash_base)), collapse = "\n")
num <- function(x) gsub(" ", " ", fpa_scn_eur(x))
picks <- regmatches(html, gregexpr('class="n[^"]*is-pick"', html))[[1]]

ok <- c(
  rows_3 = nrow(rows) == 3L,
  closed_month_out = !("2026-09" %in% rows$periode),
  chosen_is_path = all(rows$path == c(rows$low[1], rows$base[2], rows$high[3])),
  low_total = sum(rows$low) == -11875,
  base_total = sum(rows$base) == -7119,
  high_total = sum(rows$high) == -2003,
  budget_total = sum(rows$budget) == -10525,
  html_low = grepl(num(-11875), html, fixed = TRUE),
  html_base = grepl(num(-7119), html, fixed = TRUE),
  html_high = grepl(num(-2003), html, fixed = TRUE),
  html_budget = grepl(num(-10525), html, fixed = TRUE),
  html_path = grepl(num(sum(rows$path)), html, fixed = TRUE),
  html_gap = grepl(paste0("+", num(sum(rows$path) - sum(rows$budget))), html, fixed = TRUE),
  cash_base_dec = grepl(num(37099), html, fixed = TRUE),
  cash_high_dec = grepl(num(42215), html, fixed = TRUE),
  cash_path_dec = grepl(num(36001), html, fixed = TRUE),
  one_tick_per_month = length(picks) == 3L,
  neg_red = grepl("is-neg", html, fixed = TRUE),
  no_raw_oi = !grepl(">OI<", html, fixed = TRUE),
  month_labels = grepl(">déc.<", html, fixed = TRUE) && grepl("décembre", html, fixed = TRUE)
)

empty <- paste(as.character(fpa_scn_page_result(list(open = character(), ready = TRUE))), collapse = "")
wait <- paste(as.character(fpa_scn_page_result(modifyList(v, list(ready = FALSE)))), collapse = "")
one <- modifyList(v, list(open = "2026-10", cases = c(`2026-10` = "high")))
one_rows <- fpa_scn_page_rows(one)
one_html <- paste(as.character(fpa_scn_page_result(one, cash_base)), collapse = "")
frac <- modifyList(v, list(base = pl(c(-1277.4, -2379.4, -3462.4)), pnl = pl(c(-1277.4, -2379.4, -3462.4))))
frac_html <- paste(as.character(fpa_scn_page_result(frac, cash_base)), collapse = "")
ok <- c(ok,
  round_note_off_when_tied = !grepl(fpa_txt("scnp_round"), html, fixed = TRUE),
  round_note_on_when_off = grepl(fpa_txt("scnp_round"), frac_html, fixed = TRUE),
  frac_total_exact = grepl(num(-7119), frac_html, fixed = TRUE)
)
ok <- c(ok,
  empty_msg = grepl(fpa_txt("scnp_none"), empty, fixed = TRUE),
  wait_msg = grepl(fpa_txt("scnp_wait"), wait, fixed = TRUE),
  one_month = nrow(one_rows) == 1L && one_rows$path == -4000,
  one_month_cash = grepl(num(31000), one_html, fixed = TRUE)
)

where <- paste(as.character(fpa_scn_where()), collapse = "")
n_items <- sum(lengths(lapply(fpa_scn_where_groups(), `[[`, "items")))
ok <- c(ok,
  where_6_tabs = length(gregexpr('data-tab="', where, fixed = TRUE)[[1]]) == 6L,
  where_items_have_copy = all(vapply(unlist(lapply(fpa_scn_where_groups(), `[[`, "items")), function(id) nzchar(fpa_txt(paste0("scnp_w_", id))), logical(1))),
  where_18_items = n_items == 18L,
  toc_7 = length(fpa_scnp_toc) == 7L,
  toc_copy = all(vapply(fpa_scnp_toc, function(x) nzchar(fpa_txt(x[["key"]])), logical(1)))
)

cat(sprintf("scn page %s/%s\n", sum(ok), length(ok)))
if (any(!ok)) {
  cat(paste(names(ok)[!ok], collapse = ", "), "\n")
  stop("scn page check failed")
}
cat("scn page ok\n")
