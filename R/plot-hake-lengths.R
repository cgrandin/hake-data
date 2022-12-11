#' Plot a grid of length frequencies
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
plot_hake_lengths <- function(lst,
                              yrs = NULL,
                              fill_col = c(F = rgb(1, 0, 0, alpha = 0.3),
                                           M = rgb(0, 0, 1, alpha = 0.3)),
                              fn = here::here("figures-output",
                                              "can-lengths.png"),
                              ...){

  if(!is.null(yrs)){
    lst <- lst |>
      map(~{
        .x |>
          filter(year %in% yrs)
      })
  }

  tab <- lst |>
    map_df(~{.x})
  g <- gfplot::plot_lengths(tab,
                            fill_col = fill_col,
                            ...)


  if(is.null(fn)){
    g
  }else{
    ggsave(fn, g)
    message("Created the figure in `", fn, "`.")
  }
}