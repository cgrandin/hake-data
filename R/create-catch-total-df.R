#' Create (and write to csv file) a data frame with rows = years and
#' columns = fleet-specific total catches for the hake fleets
#'
#' @param d Output from [calc_catch_by_fleet_yr_month()]
#' @param conv_factor Value to divide catch by. e.g. if 1,000
#' the weights will be divided by 1,000 and come out as tonnes
#' @param digits The number of digits to round catches to
#' @param write_file If `TRUE`, write the output to the file.
#' If `FALSE`, return the data frame
#'
#' @return
#' @export
#'
#' @examples
create_catch_total_df <- function(d,
                                  conv_factor = 1000,
                                  digits = 3,
                                  write_file = TRUE){

  tab <- d |>
    group_by(Fishery, Year) |>
    summarize(weight = sum(catchKg) / conv_factor) |>
    ungroup() |>
    pivot_wider(names_from = "Fishery", values_from = "weight" )

  names(tab) <- gsub("FreezerTrawler", "ft_weight", names(tab))
  names(tab) <- gsub("Shoreside", "ss_weight", names(tab))
  names(tab) <- gsub("JV", "jv_weight", names(tab))
  tab[is.na(tab)] <- 0

  tab <- tab |>
    mutate(tot_weight = ft_weight + ss_weight + jv_weight)

  # Remove year rows where all values are zero, and re-order
  tab <- tab |>
    filter(!if_all(-Year, ~ .x == 0 )) |>
    select(Year, ss_weight, ft_weight, jv_weight, tot_weight)

  if(!write_file){
    return(tab)
  }
  # Need as.data.frame() here to avoid quoted headers
  write_csv(as.data.frame(tab),
            here::here("data-output",
                       "can-catch-by-year.csv"),
            quote = "none")
  invisible()
}