# SPDX-License-Identifier: AGPL-3.0-or-later
# Copyright (C) 2026 Julio Bauer Cantuaria
library(plotly)
source("R/fpa-stmt-view.R", encoding = "UTF-8")
FPA_HAS_PLOTLY <- TRUE
`%||%` <- function(a, b) if (is.null(a)) b else a
fpa_shiny_eur <- function(x) {
  format(round(as.numeric(x)), big.mark = " ", scientific = FALSE, trim = TRUE)
}
fpa_shiny_line_name <- function(id) as.character(id)
lines <- readLines("app.R", encoding = "UTF-8")
i0 <- grep("^fpa_shiny_pnl_waterfall_plot <-", lines)[[1]]
i1 <- grep("^fpa_shiny_bs_treemap <-", lines)[[1]] - 1L
eval(parse(text = paste(lines[i0:i1], collapse = "\n")))
row <- data.frame(
  REV = 100, OTHER_OP_INC = 2, COGS = 5, GP = 97, PAYROLL = 20, OPEX_OTHER = 10,
  EBITDA = 67, D_A = 5, OI = 62, FIN = 1, EXCEP = 0, TAX = 2, NI = 59
)
p <- fpa_shiny_pnl_waterfall_plot(row, compact = TRUE)
stopifnot(length(p$x$attrs) >= 1L)
stopifnot(!is.null(p$x$attrs[[1]]$text))
stopifnot(!is.null(p$x$attrs[[1]]$width))
built <- plotly_build(p)
stopifnot(length(built$x$data) >= 2L)
stopifnot(identical(built$x$data[[2]]$type, "scatter"))
stopifnot(!is.null(built$x$data[[1]]$text))
cat("PLOT OK\n")
