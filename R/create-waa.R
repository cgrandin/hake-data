#' Get the weight-at-age data frame for commercial data.
#'
#' @param d Output from [run_extra_samples_data()]
#' @param ft_vessels_lu The Freezer trawler lookup table. Must have columns
#' `name` and `id`
#' @param major_areas The PFMC major areas codes to include (as integers)
#' @param fn The filename for the CSV file to write the output to
#'
#' @return a data frame with Source set to `CAN_shoreside` and `CAN_freezer`
#' for the two fishery types
#' @export
create_waa <- function(d,
                       ft_vessels_lu = NULL,
                       major_areas = 3:9,
                       fn = here::here("data-output",
                                       "can-weight-at-age.csv")){

  names(d) <- tolower(names(d))
  nms <- c("CAN_shoreside", "CAN_freezer", "CAN_jv",
           "CAN_polish", "CAN_russian", "CAN_japanese")

  out <- map(nms, ~{
    if(.x == "CAN_shoreside"){
      k <- d |> filter(trip_sub_type_desc %in% c("OBSERVED DOMESTIC",
                                                 "NON - OBSERVED DOMESTIC")) |>
        filter(!vessel_id %in% ft_vessels_lu$id)
    }else if(.x == "CAN_freezer"){
      k <- d |> filter(trip_sub_type_desc %in% c("OBSERVED DOMESTIC",
                                                 "NON - OBSERVED DOMESTIC")) |>
        filter(vessel_id %in% ft_vessels_lu$id)
      # In 2019, the weights were just made up to be 1lb, and then 0.5
      # lb increments by eye. We remove them due to this bias.
      # This was discovered by looking at the gfplot::plot_lengths()
      # histograms for weight
      k <- k |>
        filter(!sample_id %in% c(542227, 542228, 542229, 542279,
                                 542280, 542281, 542282, 542283))
    }else if(.x == "CAN_jv"){
      k <- d |>
        filter(trip_sub_type_desc == "OBSERVED J-V")
    }else if(.x == "CAN_polish"){
      k <- d |>
        filter(trip_sub_type_desc %in% c("POLISH COMM NATIONAL",
                                         "POLISH COMMERCIAL SUPPLEMENTAL"))
    }else if(.x == "CAN_russian"){
      k <- d |>
        filter(trip_sub_type_desc %in% c("RUSSIAN COMMERCIAL NATIONAL",
                                         "RUSSIAN COMMERCIAL SUPPLEMENTAL"))
    }else if(.x == "CAN_japanese"){
      k <- d |>
        filter(trip_sub_type_desc %in% "JAPANESE OBSERVED NATIONAL")
    }
    j <- k |>
      filter(!is.na(weight),
             !is.na(age),
             !is.na(trip_start_date)) |>
      transmute(sample_id = sample_id,
                Source = .x,
                Weight_kg = weight / 1000,
                Sex = "U",
                Age_yrs = age,
                Month = month(trip_start_date),
                Year = year(trip_start_date)) |>
      arrange(Year, Month)

    j
  }) |>
    rev() |>
    map_df(~{.x})

  write.csv(out, fn, quote = FALSE)
  message("Wrote weight-at-age file `", fn, "`")
  invisible()
}
