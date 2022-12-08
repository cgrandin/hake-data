r_dir <- file.path(here::here(), "R")

source(file.path(r_dir, "constants.R"))
source(file.path(r_dir, "load-sample-data.R"))
source(file.path(r_dir, "sample-summary.R"))

samples <- load_sample_data(ft_vessels_lu = ft_vessels, rebuild_rds = FALSE)

# Look at this to get a summary of sampling by fleet
s_summary <- sample_summary(samples)


