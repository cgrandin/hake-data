library(here)
hr <- here()
landings_dir <- file.path(hr, "landings")
dat_out_dir <- file.path(hr, "data-output")
r_dir <- file.path(hr, "R")

source(file.path(r_dir, "constants.R"))
source(file.path(r_dir, "load-catch-data.R"))
source(file.path(r_dir, "calc-landings-by-fleet.R"))
source(file.path(r_dir, "create-catch-fleet-df.R"))
source(file.path(r_dir, "create-catch-total-df.R"))

catches <- load_catch_data()
flt_yr_month <- calc_catch_by_fleet_yr_month(catches)

create_catch_fleet_df(flt_yr_month, write_file = TRUE)

create_catch_total_df(flt_yr_month, write_file = TRUE)
