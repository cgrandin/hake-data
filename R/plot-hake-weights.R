#' Plot a grid of weight frequencies
#'
#' @param lst A list of [gfplot::tidy_lengths_raw()] data frames, each with
#' a different name in the `survey_abbrev` column
#' @param yrs A vector of years to include in the plot. If `NULL`, all
#' years in the data will be plot
#' @param fill_col See `fill_col` parameter in [gfplot::plot_lengths()]
#' @param fn The filename to save the plot to. If `NULL`, return
#' the plot instead
#' @param ... Arguments passed to [gfplot::plot_lengths()]
#'
#' @return A [ggplot2::ggplot()] object
plot_hake_weights <- function(lst,
                              fn = here::here("figures-output",
                                              "can-weights.png"),
                              ...){

  mult_format <- function() {
    function(x) format(x / 100)
  }

  g <- plot_hake_lengths(lst, fn = NULL, ...) +
    xlab("Weight (kg)") +
    ylab("Relative weight frequency") +
    labs(title = "Weight frequencies") +
    scale_x_continuous(labels = mult_format())


  if(is.null(fn)){
    g
  }else{
    ggsave(fn, g)
    message("Created the figure in `", fn, "`.")
  }
}