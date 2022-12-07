#' Load the catch data in from files extracted from FOS website
#'
#' @param catch_data_dir The directory the catch data files are in
#' @param dmp_fn The filename for the DMP CSV file
#' @param log_fn_regex The regular expression defining filenames for the
#' LOGS CSV files
#' @param dmp_skip_head The number of lines to skip at the beginning of the
#' DMP file
#' @param log_skip_head The number of lines to skip at the beginning of the
#' LOGS files
#' @param keep_all If `TREU`, keep all data. If `FALSE`, filter out catch
#' from area 4B and catch landed in French Creek prior to June as it was likely
#' caught in the strait
#'
#' @return A list of two [tibble::tibble()], the DMP data and the LOGS data
#' @export
load_catch_data <- function(catch_data_dir = here("data-catch"),
                            dmp_fn = "LandingsSpeciesDateDMP.csv",
                            log_fn_regex = "LogCatchReport",
                            dmp_skip_head = 1,
                            log_skip_head = 7,
                            keep_all = FALSE){

  out <- list()
  out$dmp_data <- read.csv(file.path(catch_data_dir, dmp_fn),
                           skip = dmp_skip_head,
                           header = TRUE) |>
    as_tibble() |>
    mutate(LANDING.DATE = gsub(" [0-9]{2}:[0-9]{2}", "", LANDING.DATE)) |>
    mutate(LANDING.DATE = as.Date(LANDING.DATE, "%B %d %Y")) |>
    mutate(Year = year(LANDING.DATE),
           Month = month(LANDING.DATE),
           Day = day(LANDING.DATE))

  files <- list.files(catch_data_dir, full.names = TRUE)
  files <- files[grep(log_fn_regex, files)]
  out$log_data <- map(files,~{
    read.csv(.x, header = TRUE, skip = log_skip_head) |>
      filter(SOURCE == "ASOP")
  }) |>
    map_df(~{.x}) |>
    as_tibble() |>
    mutate(LANDING.DATE = as.Date(LANDING.DATE, "%B %d %Y")) |>
    mutate(Year = year(LANDING.DATE),
           Month = month(LANDING.DATE),
           Day = day(LANDING.DATE))

  # Remove GULF catch from the DMP data
  if(any(grepl("GULF", out$dmp_data$LICENCE.TRIP.TYPE))){
    if(keep_all){
      message("Some of the DMP landings are labeled with 'LICENCE.TRIP.TYPE' ",
              "= 'GULF'. They were not removed.")
    }else{
      message("Some of the DMP landings are labeled with 'LICENCE.TRIP.TYPE' ",
              "= 'GULF'. They were removed.")
      out$dmp_data <- out$dmp_data |>
        filter(!grepl("GULF", LICENCE.TRIP.TYPE))
    }
  }

  if(any(grepl("OPT B", out$dmp_data$LICENCE.TRIP.TYPE))){
    if(keep_all){
      message("Some of the DMP landings are labeled with 'LICENCE.TRIP.TYPE' ",
              "= 'OPT B'. They were not removed.")
    }else{
      message("Some of the DMP landings are labeled with 'LICENCE.TRIP.TYPE' ",
              "= 'OPT B'. They were removed.")
      out$dmp_data <- out$dmp_data |>
        filter(!grepl("OPT B", LICENCE.TRIP.TYPE))
    }
  }

  # Remove GULF catch from the LOGS data
  if(any(out$log_data$AREA %in% c("4B", "Trawl Gulf"))){
    if(keep_all){
      message("Some of the LOGS discards are in the '4B' or 'Trawl Gulf' ",
              "area. They were not removed.")
    }else{
      message("Some of the LOGS discards are in the '4B' or 'Trawl Gulf' ",
              "area. They were removed.")
      out$log_data <- out$log_data |>
        filter(!AREA %in% c("4B", "Trawl Gulf"))
    }
  }

  if(any(grepl("GULF", out$log_data$TRIP.TYPE))){
    if(keep_all){
      message("Some of the LOGS discards are labeled with 'TRIP.TYPE' = 'GULF'. ",
              "They were not removed.")
    }else{
      message("Some of the LOGS discards are labeled with 'TRIP.TYPE' = 'GULF'. ",
              "They were removed.")
      out$log_data <- out$log_data |>
        filter(!grepl("GULF", TRIP.TYPE))
    }
  }

  if(any(grepl("OPT B", out$log_data$TRIP.TYPE))){
    if(keep_all){
      message("Some of the LOGS discards are labeled with 'TRIP.TYPE' = 'OPT B'. ",
              "They were not removed.")
    }else{
      message("Some of the LOGS discards are labeled with 'TRIP.TYPE' = 'OPT B'. ",
              "They were removed.")
      out$log_data <- out$log_data |>
        filter(!grepl("OPT B", TRIP.TYPE))
    }
  }

  out
}
