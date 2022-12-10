create_landings_data_files <- function(catches){

  hr <- here::here()
  r_dir <- file.path(hr, "R")

  flt_yr_month <- calc_catch_by_fleet_yr_month(catches)

  create_catch_fleet_df(flt_yr_month)

  create_catch_total_df(flt_yr_month)
}