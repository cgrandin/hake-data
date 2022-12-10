#' Get depth data by year and calculate boxplot stats on it.
#' This writes a CSV file containing the output
#'
#' @param d Output from [load_spatial_catch_data()]
#' @param type One of "bottom" or "gear" for depth type
#' @param yrs A vector of years to include. If NULL, all years in the data will be included
#' @param min_depth_cutoff The depth for which to remove data. In fathoms. All data shallower than this
#' will be removed. This applies to both `type` 'bottom' and 'gear'
#'
#' @return Invisibly - A tibble containing year and depth record stats
#' @export
#' @importFrom dplyr do
#'
#' @examples
#' d_ss <- load_spatial_catch_data("ss")
#' gear_depth_ss <- get_depth_by_year(d_ss, "gear")
create_depth_by_year <- function(lst,
                                 yrs = NULL,
                                 min_depth_cutoff = 50 / 1.8288){

  depth_cols <- c("bottom", "gear")

  out <- imap(lst, function(fleet_df, fleet){
    map(depth_cols, function(depth_col){
      depth_col_sym <- sym(paste0(depth_col, "depth_fm"))
      dpth <- fleet_df |>
        filter(!is.na(!!depth_col_sym)) |>
        transmute(year = year(catchdate),
                  depth = !!depth_col_sym * 1.8288) |>
        filter(depth >= min_depth_cutoff) |>
        group_by(year) %>%
        do(as.data.frame(t(boxplot.stats(.$depth)$`stats`))) |>
        ungroup() |>
        transmute(year,
                  lower95 = V1,
                  lowerhinge = V2,
                  median = V3,
                  upperhinge = V4,
                  upper95 = V5)

      if(!is.null(yrs)){
        dpth <- dpth |>
          filter(year %in% yrs)
      }
      fn <- here::here("data-output", paste0("depth-can-", fleet, "-", depth_col,".csv"))
      write_csv(dpth, fn)
      message("Wrote file `", fn)
      dpth
    }) |>
      setNames(c("bottom", "gear"))
  })
  invisible(out)
}
