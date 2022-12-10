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

  samples <- load_sample_data(ft_vessels_lu = ft_vessels, rebuild_rds = FALSE)
  samples_extra <- readRDS(here::here("data-sample", "samples-extra.rds")) |> as_tibble()
  catches <- load_catch_data()
  catches_lst <- load_spatial_catch_data()

  create_waa(samples_extra, ft_vessels_lu = ft_vessels, type = "wa")
  create_waa(samples_extra, ft_vessels_lu = ft_vessels, type = "wal")
  calc_num_fish_aged(samples)
  create_age_proportion_files(samples)
  create_landings_data_files(catches)
  create_spatial_catch_plots(catches_lst)
  plot_depths(create_depth_by_year(catches_lst))
}