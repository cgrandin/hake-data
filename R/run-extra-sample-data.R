#' Run the function [gfdata::get_commercial_samples] on the `gfbiosql` database
#'
#' @details
#' Provides additional fields in the samples data (namely sex)
#' The data only includes "outside hake" PFMC areas.
#' This is only used to extract the weight-at-age by sex, see [get_waa()]
#'
#' @param overwrite Logical. Overwrite the RDS file for sample data if it exists
#'
#' @export
#' @importFrom gfdata get_commercial_samples
run_extra_sample_data <- function(fn = here::here("data-samples", "samples-extra.rds")){

  major_areas <- c("02", "03", "04", "05", "06", "07", "08", "09")

  dir.create(here::here("data-samples"), showWarnings = FALSE)

  d <- gfdata::get_commercial_samples(225) %>%
    filter(major_stat_area_code %in% major_areas |
             (major_stat_area_code == "01" & minor_stat_area_code == "20"))

  saveRDS(d, fn)
}
