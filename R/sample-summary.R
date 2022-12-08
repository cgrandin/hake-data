#' Summarize an array of interesting columns in the sample data
#'
#' @param lst A list of data frames as output from [load_sample_data()]
#'
#' @return A list of two lists, each with three dataframes,
#' these data frames in the two lists are summarized by year and
#' year + sample_id
#' @export
#' @importFrom dplyr n_distinct
sample_summary <- function(lst){

  out <- list()
  out$by_sample_id <- map(lst, ~{
    .x |>
      group_by(year, sample_id) |>
      summarize(ages = sum(!is.na(age)),
                na_ages = sum(is.na(age)),
                lengths = sum(!is.na(length)),
                na_lengths = sum(is.na(length)),
                weights = sum(!is.na(weight)),
                na_weights = sum(is.na(weight)),
                sample_weights = sum(!is.na(sample_weight)),
                na_sample_weights = sum(is.na(sample_weight)),
                catch_weights = sum(!is.na(catch_weight)),
                na_catch_weights = sum(is.na(catch_weight)),
                num_fish = n_distinct(specimen_id)) |>
      ungroup() %>%
      arrange(desc(year), sample_id)
  })

  out$by_yr <- map(out$by_sample_id, ~{
    .x |>
      group_by(year) |>
      summarize(ages = sum(ages),
                na_ages = sum(na_ages),
                lengths = sum(lengths),
                na_lengths = sum(na_lengths),
                weights = sum(weights),
                na_weights = sum(na_weights),
                sample_weights = sum(sample_weights),
                na_sample_weights = sum(na_sample_weights),
                catch_weights = sum(catch_weights),
                na_catch_weights = sum(na_catch_weights),
                num_fish = sum(num_fish)) |>
      ungroup() %>%
      arrange(desc(year))
  })

  out
}