#' Get the weight-at-age data frame for commercial data.
#'
#' @param d Output from [run_extra_samples_data()]
#' @param ft_vessels_lu The Freezer trawler lookup table. Must have columns
#' `name` and `id`
#' @param major_areas The PFMC major areas codes to include (as integers)
#' @param type If "wal", filter for records with non-NA weights, ages, and
#' length. If "wa", filter for records with non-NA weights and ages
#' @param fn The filename for the CSV file to write the output to
#'
#' @return a data frame with Source set to `CAN_shoreside` and `CAN_freezer`
#' for the two fishery types
#' @export
create_waa <- function(d,
                       ft_vessels_lu = NULL,
                       major_areas = 3:9,
                       type = c("wal", "wa"),
                       fn = here::here("data-output",
                                       ifelse(type == "wal",
                                              "can-weight-at-age-and-length.csv",
                                              "can-weight-at-age.csv"))){

  type <- match.arg(type)

  names(d) <- tolower(names(d))
  nms <- c("CAN_shoreside", "CAN_freezer", "CAN_jv", "CAN_polish")

  out <- map(nms, ~{
    k <- d
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

    if(type == "wal"){
      k |> transmute(Source = .x,
                     Weight_kg = weight / 1000,
                     Sex = ifelse(is.na(sex),
                                  NA_character_,
                                  ifelse(sex == 1,
                                         "M",
                                         ifelse(sex == 2,
                                                "F",
                                                "U"))),
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
    }else if(type == "wa"){
      k |> transmute(Source = .x,
                     Weight_kg = weight / 1000,
                     Sex = ifelse(is.na(sex),
                                  NA_character_,
                                  ifelse(sex == 1,
                                         "M",
                                         ifelse(sex == 2,
                                                "F",
                                                "U"))),
                     Age_yrs = age,
                     Month = month(trip_start_date),
                     Year = year(trip_start_date)) |>
        filter(!is.na(Weight_kg),
               !is.na(Sex),
               !is.na(Age_yrs),
               !is.na(Weight_kg),
               !is.na(Month),
               !is.na(Year))
    }
  }) |>
    rev() |>
    map_df(~{.x})

  write.csv(out, fn, quote = FALSE)
  invisible()
}
