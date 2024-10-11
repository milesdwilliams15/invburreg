#' Plot Pr(X > x) given x for an inverse Burr model
#' 
#' This function allows you to plot a 
#' log-log plot of Pr(X > x) given x by specifying values for the three
#' main parameters for an inverse Burr distribution (scale, shape 1, and shape 2).
#' 
#' @param mu The scale parameter for an inverse Burr model.
#' @param alpha The first shape parameter for an inverse Burr model.
#' @param theta The second shape parameter for an inverse Burr model.
#' @param xmin Default is 0. The minimum level of x to show in the plot.
#' @param xmax Default is 1000. The maximum level of x to show in the plot.
#' @param len Default is 1000. The level of granularity for x.
#' @param add Are you adding a new inverse Burr model to an existing plot? Default is `FALSE`.
#' @param legend Default is `NULL`. Do you want to include a legend if adding additional plots? This lets you select the category name for a particular model.
#' @param legend_title Default is `NULL`. Do you want to provide a custom name for your legend plot?
#' @export
ll_plot <- function(
    mu, alpha, theta, 
    xmin = 0,
    xmax = 1000, 
    len = 1000,
    add = F,
    legend = NULL,
    legend_title = NULL
) {
  
  ## X values to be plotted
  x <- exp(seq(ifelse(xmin <= 0, 0, log(xmin)), log(xmax), len = len))
  
  ## The probability that X > x
  p <- actuar::pinvburr(x, alpha, theta, scale = mu, lower.tail = F)
  
  ## Create the plot
  # ... if the plot is a new plot
  if(!add) {
    ggplot2::ggplot() +
      ggplot2::aes(x, p) +
      ggplot2::geom_line(
        ggplot2::aes(
          color = legend
        ),
        size = 1
      ) +
      ggplot2::scale_x_log10(
        labels = scales::comma
      ) +
      ggplot2::scale_y_log10(
        n.breaks = 9,
        labels = scales::percent
      ) +
      ggplot2::labs( 
        x = "x",
        y = "Pr(X > x)",
        color = legend_title
      ) +
      ggplot2::theme(
        panel.grid.major = 
          ggplot2::element_line(
            color = "black",
            linetype = 4,
            size = .5
          )
      ) 
  } else {
    # ... if making an addition to the plot
    ggplot2::last_plot() +
      ggplot2::geom_line(
        ggplot2::aes(x, p, color = legend),
        size = 1
      ) 
  }
}