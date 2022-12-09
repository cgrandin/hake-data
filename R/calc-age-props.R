#' Calculate the weighted age proportions for the input data
#'
#' @details
#' Each record will have an `lw_alpha` and `lw_beta` assigned to it. To get these:
#' Estimate them for `sample_id`s with enough (`lw_cutoff`) length samples in them.
#' Next, group the data by `year` and coalesce those into the same columns.
#' To fill in the remaining ones, use an overall (all years) estimate (local variable
#' `all_yrs_lw`). At this point, the `lw_alpha` and `lw_beta` columns will be fully
#' populated for every specimen (no NAs).
#' Hake sampling for length has been very good, so specimen weights are calculated using the
#' length/weight parameters estimated for each specimen (for records in which weights
#' haven't been recorded as data).
#'
#' Calculate missing sample weights, by summing individual specimen weights in each sample
#' They are divided by 1,000 because the specimen samples are in grams and sample weights in kilograms.
#' If there is no catch weight for a sample, both `catch_weight` and `sample_weight` are
#' set to 1 so that the ratio multiplied later for weighting is 1 and raw proportions are used
#' instead for these samples.
#'
#' Counts of ages by `sample_id` are made, and missing ages are filled in using [tidyr::complete()].
#' Missing `year`, `sample_weight`, and `catch_weight` for cases added with [tidyr::complete()] are added.
#' Numbers-at-age are weighted by `catch_weight` / `sample_weight`.
#' Numbers for each `year` and `age` are summed.
#' Weighted proportions by `year` and `age` are produced.
#'
#' @param d A data frame as created by [load_sample_data()]
#' @param min_date Earliest date to include
#' @param plus_grp Age plus group for maximum grouping
#' @param lw_tol See [fit_lw()]
#' @param lw_maxiter See [fit_lw()]
#' @param by_month Logical. If TRUE the return dataframe with have a `month` column
#'
#' @return Age proportion dataframe with ages as columns and years as rows
#' @export
#' @importFrom dplyr first group_by_at vars one_of
#' @importFrom reshape2 dcast
#' @importFrom lubridate month mdy
calc_age_props <- function(d,
                           min_date = NULL, #as.Date("1972-01-01"),
                           plus_grp = 15,
                           lw_tol = 0.1,
                           lw_maxiter = 1000,
                           by_month = FALSE){

  temporal_grouping <- if(by_month) c("year", "month") else "year"

  # Remove ageless fish, put fish ages into plus group, remove time from
  # trip end date string, and change date format
  d <- d |>
    filter(!is.na(age)) |>
    mutate(age = ifelse(age > plus_grp, plus_grp, age)) |>
    mutate(trip_end_date = gsub(" +[0-9]+:[0-9]{2}:[0-9]{2}$", "", trip_end_date)) |>
    mutate(trip_end_date = lubridate::mdy(trip_end_date))

  # Filter the date to be greater than the minimum asked for, or the minimum
  # in the data
  if(is.null(min_date)){
    min_date <- min(d$trip_end_date)
  }
  d <- d |>
    filter(trip_end_date >= min_date)

  # In case there are errors where the age was set to zero (JV has one)
  d <- d |>
    filter(age > 0)

  if(by_month){
    d <- d |>
      mutate(month = month(trip_end_date)) |>
      select(year, month, sample_id, length, weight, age, sample_weight, catch_weight)
  }else{
    d <- d |>
      select(year, sample_id, length, weight, age, sample_weight, catch_weight)
  }

  # LW parameter estimation
  all_yrs_lw <- fit_lw(d, lw_tol, lw_maxiter)

  # Fill in missing `weight` and `sample_weight`
  ds <- d |>
    mutate(lw_alpha = all_yrs_lw[1],
           lw_beta = all_yrs_lw[2]) |>
    split(~sample_id) |>
    map(~{
      .x |> mutate(weight = ifelse(is.na(weight) & !is.na(length),
                               lw_alpha * length ^ lw_beta,
                               weight))
      }) |>
    map(~{
      # Fill in sample weights for those that have individual weights but no sample weights
      if(is.na(first(.x$sample_weight))){
        .x$sample_weight <- sum(.x$weight, na.rm = TRUE) / 1000
      }
      if(first(.x$sample_weight) == 0){
        .x$sample_weight <- sum(.x$weight, na.rm = TRUE) / 1000
      }
      .x
    }) |>
    map_df(~{.x})

  ap <- ds |>
    group_by(sample_id, age) %>%
    summarize(year = first(year),
              # This allows month to be present or not
              month = {if("month" %in% names(.)) month else NULL},
              num_ages = n(),
              sample_weight = first(sample_weight),
              catch_weight = first(catch_weight)) |>
    ungroup() |>
    group_by(sample_id) %>%
    mutate(num_ages = ifelse(is.na(num_ages),
                             0,
                             num_ages),
           year = max(year, na.rm = TRUE),
           # This allows month to be present or not
           month = {if("month" %in% names(.)) month else NULL},
           sample_weight = max(sample_weight, na.rm = TRUE),
           catch_weight = max(catch_weight, na.rm = TRUE)) |>
    mutate(num_ages_weighted = num_ages * catch_weight / sample_weight) |>
    ungroup()

  if(by_month){
    ap <- ap |>
      group_by(year, month, age) |>
      summarize(num_ages_weighted = sum(num_ages_weighted)) |>
      mutate(age_prop = num_ages_weighted / sum(num_ages_weighted)) |>
      ungroup() |>
      select(-num_ages_weighted)
    prop_lst <- ap |>
      split(~year) |>
      map(~{
        tmp <- complete(.x, year, month, age = 1:plus_grp, fill = list(age_prop = 0))
        tmp <- reshape2::dcast(tmp, month ~ age, value.var = "age_prop")
        tmp[is.na(tmp)] <- 0
        tmp
      })
  }else{
    ap <- ap |>
      group_by(year, age) |>
      summarize(num_ages_weighted = sum(num_ages_weighted)) |>
      mutate(age_prop = num_ages_weighted / sum(num_ages_weighted)) |>
      ungroup() |>
      select(-num_ages_weighted)
    prop_tab <- reshape2::dcast(ap, year ~ age, value.var = "age_prop")
    prop_tab[is.na(prop_tab)] <- 0
    prop_lst <- list(prop_tab)
  }

  prop_lst
}
