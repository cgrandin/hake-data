#' Get the weight-at-age data frame for commercial data.
#'
#' @param lst Output from [load_sample_data()], a list of three data frames,
#' one for each fleet, containing sample data
#'
#' @return a data frame with Source set to `CAN_shoreside` and `CAN_freezer`
#' for the two fishery types
#' @export
get_waa <- function(fns = here::here("data-sample",
                                     c("hake_domestic_obs_len_wt_age.csv",
                                       "hake_jv_obs_len_wt_age.csv")),
                    fn_rds = here::here("data-sample",
                                        "samples_df.rds"),
                    ft_vessels_lu = NULL,
                    rebuild_rds = FALSE,

                    major_areas = 3:9){

  if(file.exists(fn_rds) && !rebuild_rds){
    message("Loading pre-made samples RDS file. To rebuild it from the CSVs, ",
            "set argument `rebuild_rds` to `TRUE`. File:\n", fn_rds, "\n")
    d <- readRDS(fn_rds)
  }
  if(is.null(ft_vessels_lu)){
    stop("`ft_vessels_lu` data frame is missing. It needs to have at least ",
         "two columns: name and id")
  }
  if(file.exists(fn_rds) && rebuild_rds){
    message("Rebuilding the samples RDS file:\n", fn_rds, "\n")
  }
  if(!file.exists(fn_rds)){
    message("Creating the samples RDS file:\n", fn_rds, "\n")
    dir.create(dirname(fn_rds), showWarnings = FALSE)
    a <- read.csv(fns[1]) |> as_tibble()
    b <- read.csv(fns[2]) |> as_tibble()
    d <- a |> bind_rows(b)
    saveRDS(d, fn_rds)
  }

  names(d) <- tolower(names(d))
  nms <- c("CAN_shoreside", "CAN_freezer", "CAN_jv", "CAN_polish")

  map(nms, ~{
    k <- d |>
      filter(major_stat_area_code %in% major_areas |
               (major_stat_area_code == 1 & minor_stat_area_code == 20))
    if(.x == "CAN_shoreside"){
      k <- k |>
        filter(trip_sub_type_desc %in% c("OBSERVED DOMESTIC", "NON - OBSERVED DOMESTIC")) |>
        filter(!vessel_id %in% ft_vessels_lu$id)
    }else if(.x == "CAN_freezer"){
      k <- k |>
        filter(trip_sub_type_desc %in% c("OBSERVED DOMESTIC", "NON - OBSERVED DOMESTIC")) |>
        filter(vessel_id %in% ft_vessels_lu$id)
    }else if(.x == "CAN_jv"){
      k <- k |>
        filter(trip_sub_type_desc == "OBSERVED J-V")
    }else{
      k <- k |>
        filter(trip_sub_type_desc %in% c("POLISH COMM NATIONAL", "POLISH COMMERCIAL SUPPLEMENTAL"))
    }
    browser()

    k |> transmute(Source = .x,
                    Weight_kg = weight_g * 1000,
                    Sex = ifelse(is.na(sex), NA_character_, ifelse(sex == 1, "M", "F")),
                    Age_yrs = age,
                    Length_cm = length,
                    Month = month(trip_start_date),
                    Year = year(trip_start_date)) |>
      filter(!is.na(Weight_kg),
             !is.na(Sex),
             !is.na(Age_yrs),
             !is.na(Length_cm),
             !is.na(Weight_kg),
             !is.na(Month),
             !is.na(Year))
  }) |>
    bind_rows()
}