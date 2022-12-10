create_all_data <- function(){

  source(here::here("R", "constants.R"))

  # `run_spatial_catch_sql()` and `run_extra_sample_data()` must be run while
  # on the VPN or the DFO intranet. You can run it on a DFO machine, and
  # transfer the three CSV files they generate to the data-output directory
  # on your non-DFO machine.

  # run_spatial_catch_sql()
  # run_extra_sample_data()

  samples <- load_sample_data(ft_vessels_lu = ft_vessels, rebuild_rds = FALSE)
  samples_extra <- read.csv(here::here("data-sample", "samples-extra.csv"))
  catches <- load_catch_data()
  catches_lst <- load_spatial_catch_data()

  get_waa(samples)
  calc_num_fish_aged(samples)
  create_age_proportion_files(samples)
  create_landings_data_files(catches)
  create_spatial_catch_plots(catches_lst)
  create_depth_by_year(catches_lst)
  plot_depths(x)
}