# Moved from R/outlier_summary.R on 2026-11-16: unreachable from the
# package API, the Shiny app, vignettes or tests (call-graph check).
# Not part of the package build (dev/ is .Rbuildignore'd).

#' Plot Monthly Outlier Summaries by Year and Algorithm
#'
#' @param grand.data.error.totals Output of [monthly.outlier.summary()].
#'
#' @return A ggplot object.
#' @noRd
outlier.summary.chart <- function(grand.data.error.totals) {
  yearly <- yearly.outlier.summary(grand.data.error.totals)

  ggplot2::ggplot(grand.data.error.totals) +
    ggplot2::geom_line(
      ggplot2::aes(x = as.factor(month), y = pe, color = Alg, group = Alg)
    ) +
    ggplot2::facet_grid(year ~ data, labeller = ggplot2::label_wrap_gen()) +
    ggplot2::geom_text(
      data  = yearly,
      ggplot2::aes(
        x     = 8,
        y     = 0.65,
        label = paste(
          "%Err(n) =",     scales::percent(pn, accuracy = 0.1),
          "\n",
          "%Err(value) =", scales::percent(pe, accuracy = 1)
        )
      ),
      size = 3
    ) +
    ggplot2::theme_bw() +
    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::labs(
      title    = "Potential errors (outliers) among reported values",
      subtitle = "by year, data, and error detection algorithm",
      x        = "Month",
      y        = "Percent\n",
      caption  = paste(
        "%Err(n) = percent of values flagged as potential errors\n",
        "%Err(value) = percent of total value flagged as potential errors"
      )
    )
}

