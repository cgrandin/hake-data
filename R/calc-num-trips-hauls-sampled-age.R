#' Create a data frame containing the number of sample_id for each year
#' that have fish with ages
#'
#' @param lst The output of [load_sample_data()], a list of data frames
#'
#' @return a list of data frames
#' @export
calc_num_trips_hauls_sampled_age <- function(lst){

  map(lst, ~{
    .x |>
      filter(!is.na(age)) |>
      group_by(year) |>
      summarize(num_trips_hauls = length(unique(sample_id))) |>
      ungroup()
  })
}
