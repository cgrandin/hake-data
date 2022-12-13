plot_hake_ages <- function(lst,
                           yrs = NULL,
                           line_col = c(M = rgb(0, 0, 1, alpha = 0.3),
                                        F = rgb(1, 0, 0, alpha = 0.3),
                                        U = rgb(0, 0, 1, alpha = 0.3)),
                           fn = here::here("figures-output",
                                           "can-ages.png"),
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

  g <- gfplot::plot_ages(tab,
                         line_col = line_col,
                         ...) +
    theme(legend.position = "none")

  if(is.null(fn)){
    g
  }else{
    ggsave(fn, g)
    message("Created age proportions figure in `", fn, "`.")
  }
}