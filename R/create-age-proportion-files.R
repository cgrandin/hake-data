create_age_proportion_files <- function(){
  r_dir <- file.path(here::here(), "R")

  source(file.path(r_dir, "constants.R"))
  source(file.path(r_dir, "load-sample-data.R"))
  source(file.path(r_dir, "sample-summary.R"))
  source(file.path(r_dir, "fit-lw.R"))
  source(file.path(r_dir, "calc-age-props.R"))
  source(file.path(r_dir, "calc-num-fish-aged.R"))
  source(file.path(r_dir, "calc-num-trips-hauls-sampled-age.R"))

  samples <- load_sample_data(ft_vessels_lu = ft_vessels, rebuild_rds = FALSE)

  # Look at this to get a summary of sampling by fleet
  s_summary <- sample_summary(samples)


  age_fn <- here::here("data-output", "can-age-data.csv")

  dir.create(here::here("data-output"), showWarnings = FALSE)
  if(file.exists(age_fn)){
    unlink(age_fn, force = TRUE)
  }

  ages_ss <- calc_age_props(samples$ss)[[1]]


  write_csv_append <- function(...){
    write.table(..., append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE, sep = ",")
  }

  write("Shoreside age comps", age_fn)
  # Write ages as header
  ages <- names(ages_ss) |> as.numeric()
  ages <- ages[!is.na(ages)]
  write(paste0(",", paste(ages, collapse = ",")), age_fn, append = TRUE)
  write_csv_append(x = ages_ss, file = age_fn)

  ages_ft <- calc_age_props(samples$ft)[[1]]
  write("Freezer Trawler age comps", age_fn, append = TRUE)
  write_csv_append(ages_ft, age_fn)

  ages_jv <- calc_age_props(samples$jv)[[1]]
  write("Joint Venture age comps", age_fn, append = TRUE)
  write_csv_append(ages_jv, age_fn)

  num_trips <- calc_num_trips_hauls_sampled_age(samples)
  write("Shoreside number of trips sampled for age", age_fn, append = TRUE)
  write_csv_append(num_trips$ss, age_fn)

  write("Freezer Trawler number of hauls sampled for age", age_fn, append = TRUE)
  write_csv_append(num_trips$ft, age_fn)

  write("Joint Venture number of hauls sampled for age", age_fn, append = TRUE)
  write_csv_append(num_trips$jv, age_fn)


  ages_bymonth_ft <- calc_age_props(samples$ft, by_month = TRUE)

}