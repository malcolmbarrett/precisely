#' Plot precisely
#'
#' Simple line plots for the output of [map_precisely()]. Use
#' [dplyr::group_by()] to create multiple lines on the plot.
#'
#' @param .df a data frame with values to plot, possibly from [map_precisely()].
#' @param xlab Label for the x-axis.
#' @param ylab Label for the y-axis.
#' @param line_size The width of the line. Default is 1.
#'
#' @return a ggplot
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' map_precisely(
#'   n_risk_difference,
#'   precision = seq(from = .02, to = .20, by = .005),
#'   exposed = .4,
#'   unexposed = .3,
#'   group_ratio = 1
#'  ) %>%
#'   plot_sample_size()
#'
#' map_precisely(
#'   precision_odds_ratio,
#'   n_cases = seq(from = 500, to = 1000, by = 10),
#'   exposed_cases = .6,
#'   exposed_controls = .4,
#'   group_ratio = 1:4
#' ) %>%
#'   group_by("Control/Case Ratio" = factor(group_ratio)) %>%
#'   plot_precision()
#'
#' map_precisely(
#'   upper_rate_ratio,
#'   upper_limit = seq(1.5, 2.5, by = .1),
#'   prob = seq(.50, .95, by = .05),
#'   exposed = .01,
#'   unexposed = .01,
#'   group_ratio = 1:4
#'  ) %>%
#'   group_by("Probability" = factor(prob)) %>%
#'   plot_upper_limit(line_size = 1) +
#'     scale_color_viridis_d() +
#'     theme_precisely() +
#'     theme(legend.position = "right",
#'           strip.text = element_text(margin = margin(b = 5), hjust = 0)) +
#'     facet_wrap(~ group_ratio,
#'                labeller = as_labeller(function(x) paste("Unexposed/Exposed:", x)))
#'
#'
#' @importFrom  rlang !!
plot_sample_size <- function(.df, xlab = "Sample Size", ylab = "Precision", line_size = 1) {
  grouped <- dplyr::is_grouped_df(.df)
  if (grouped) {
    group <- dplyr::group_vars(.df)
    group <- rlang::sym(group)
    aesthetics <- ggplot2::aes(n_total, precision, col = !!group)
  } else {
    aesthetics <- ggplot2::aes(n_total, precision)
  }

  ggplot2::ggplot(.df, aesthetics) +
    ggplot2::geom_line(size = line_size) +
    ggplot2::labs(x = xlab, y = ylab)
}

#' @rdname plot_sample_size
#' @export
plot_precision <- function(.df, xlab = "Precision", ylab = "Sample Size", line_size = 1) {
  grouped <- dplyr::is_grouped_df(.df)
  if (grouped) {
    group <- dplyr::group_vars(.df)
    group <- rlang::sym(group)
    aesthetics <- ggplot2::aes(precision, n_total, col = !!group)
  } else {
    aesthetics <- ggplot2::aes(precision, n_total)
  }

  ggplot2::ggplot(.df, aesthetics) +
    ggplot2::geom_line(size = line_size) +
    ggplot2::labs(x = xlab, y = ylab)
}

#' @rdname plot_sample_size
#' @export
plot_upper_limit <- function(.df, xlab = "Sample Size", ylab = "Upper Limit", line_size = 1) {
  grouped <- dplyr::is_grouped_df(.df)
  if (grouped) {
    group <- dplyr::group_vars(.df)
    group <- rlang::sym(group)
    aesthetics <- ggplot2::aes(n_total, upper_limit, col = !!group)
  } else {
    aesthetics <- ggplot2::aes(n_total, upper_limit)
  }

  ggplot2::ggplot(.df, aesthetics) +
    ggplot2::geom_line(size = line_size) +
    ggplot2::labs(x = xlab, y = ylab)
}


#' Minimalist themes for precision plots
#'
#' @inheritParams ggplot2::theme_minimal
#' @param ... additional arguments passed to [ggplot2::theme()]
#'
#' @export
#' @importFrom ggplot2 %+replace%
theme_precisely <- function(base_size = 14, base_family = "", ...) {
    ggplot2::theme_minimal(
      base_size = base_size,
      base_family = base_family
     ) %+replace%
    ggplot2::theme(
      strip.text = ggplot2::element_text(
        face = "bold",
        margin = ggplot2::margin(b = 5),
        hjust = 0
       ),
      legend.position = "bottom",
      ...,
      complete = TRUE
    )
}

