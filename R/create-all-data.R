create_all_data <- function(){

  source(here::here("R", "constants.R"))

  # `run_spatial_catch_sql()` and `run_extra_sample_data()` must be run while
  # on the VPN or the DFO intranet. You can run it on a DFO machine, and
  # transfer the three CSV files they generate to the data-catch and
  # data-sample directories respectively
  # on your non-DFO machine.

  # run_spatial_catch_sql()
  # After running `run_spatial_catch_sql()`, place three output files,
  # catch-locations-ft.csv, catch-locations-ss.csv, and catch-locations-jv.csv
  # in the data-catch directory

  # run_extra_sample_data()
  # After running `run_extra_sample_data()`, place output file,
  # sample-extra.rds in the data-sample directory

  catches <- load_catch_data()
  catches_lst <- load_spatial_catch_data()

  # ----- Samples
  samples <- load_sample_data(ft_vessels_lu = ft_vessels, rebuild_rds = FALSE)
  samples_extra <- samples |>
    map_df(~{.x}) |>
    mutate(sex = 1,
           usability_code = 1,
           species_category_code = 1,
           major_stat_area_code = areas_lu[area, ]$code,
           species_common_name = "225")

  # ----- Weight-at-age
  create_waa(samples_extra |> mutate(sex = 0), ft_vessels_lu = ft_vessels, type = "wa")
  create_waa(samples_extra |> mutate(sex = 0), ft_vessels_lu = ft_vessels, type = "wal")

  # ----- Age proportions
  calc_num_fish_aged(samples)
  create_age_proportion_files(samples)
  create_landings_data_files(catches)
  create_spatial_catch_plots(catches_lst)

  # ----- Depths
  plot_hake_depths(create_depth_by_year(catches_lst))


  # ----- Lengths
  samples_extra_ft <- samples$ft |>
    mutate(sex = 1,
           usability_code = 1,
           species_category_code = 1,
           major_stat_area_code = areas_lu[area, ]$code,
           species_common_name = "225")
  samples_extra_ss <- samples$ss |>
    mutate(sex = 1,
           usability_code = 1,
           species_category_code = 1,
           major_stat_area_code = areas_lu[area, ]$code,
           species_common_name = "225")
  samples_extra_lst <- list(samples_extra,
                            samples_extra_ft,
                            samples_extra_ss)
  fleet_names_lst <- c("All vessels",
                       "Freezer trawler",
                       "Shoreside")
  tidy_samples_extra_lst <- map2(samples_extra_lst, fleet_names_lst, ~{
    gfplot::tidy_lengths_raw(.x, sample_type = "commercial") |>
      mutate(survey_abbrev = .y)
  })
  plot_hake_lengths(tidy_samples_extra_lst, yrs = 2018:2022, show_year = "all")

  # ----- Weights
  # Use a trick, replace length column with weights and call
  # `gfplot::plot_lengths()` from within `plot_hake_weights()`
  replaced_lw_tidy_samples_extra_lst <- map2(samples_extra_lst, fleet_names_lst, ~{
    .x <- .x |> mutate(length = weight / 10)
    gfplot::tidy_lengths_raw(.x, sample_type = "commercial") |>
      mutate(survey_abbrev = .y)
  })
  plot_hake_weights(replaced_lw_tidy_samples_extra_lst, yrs = 2018:2022, show_year = "all")

  # ----- Ages
  tidy_age_samples_extra_lst <- map2(samples_extra_lst, fleet_names_lst, ~{
    gfplot::tidy_ages_raw(.x, sample_type = "commercial") |>
      mutate(survey_abbrev = .y)
  })
  plot_hake_ages(tidy_age_samples_extra_lst,
                 yrs = 2012:2022,
                 diagonal_lines = c(-1999, -2010, -2014, -2016),
                 year_increment = 1)
}
