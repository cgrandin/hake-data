#' Create (and write to csv file) data frames with rows = years and
#' columns = months for the hake fleets
#'
#' @param d Output from [calc_catch_by_fleet_yr_month()]
#' @param conv_factor Value to divide catch by. e.g. if 1,000
#' the weights will be divided by 1,000 and come out as tonnes
#' @param digits The number of digits to round catches to
#' @param write_file If `TRUE`, write the output to the file.
#' If `FALSE`, return the data frame
#'
#' @return A [tibble::tibble()]
#' @export
create_catch_fleet_df <- function(d,
                                  conv_factor = 1000,
                                  digits = 3,
                                  write_file = TRUE){

  fleet = c("JV",
            "FreezerTrawler",
            "Shoreside")
  fleet_short <- c("jv",
                   "ft",
                   "ss")

  if(write_file){
    map_func <- walk2
  }else{
    map_func <- map2
  }

  out <- map_func(fleet, fleet_short, ~{
    tmp <- d |>
      filter(Fishery == .x) |>
      select(-Fishery, -numLandings) |>
      mutate(catchKg = catchKg / conv_factor) |>
      mutate(catchKg = round(catchKg, digits))

    months_numeric <- 1:12
    months <- as.character(months_numeric)
    months_avail <- sort(unique(tmp$Month))
    months_not_avail <- months_numeric[!months_numeric %in% months_avail]

    if(length(months_not_avail)){
      # copy last row and add a row for each missing month
      row <- tmp[1, ]
      for(mnth in months_not_avail){
        row$Month <- mnth
        row$catchKg <- 0
        tmp <- tmp |> rbind(row)
      }
    }

    tab <- tmp |>
      pivot_wider(names_from = "Month", values_from = "catchKg") |>
      select(Year, all_of(months))
    tab[is.na(tab)] <- 0

    # Remove year rows where all values are zero
    tab <- tab |>
      filter(!if_all(-Year, ~ .x == 0 ))

    if(write_file){
      # Need as.data.frame() here to avoid quoted headers
      write_csv(as.data.frame(tab),
                here::here("data-output",
                           paste0("can-", .y, "-catch-by-month.csv")),
                quote = "none")
    }
    tab
  })

  if(write_file){
    invisible()
  }else{
    out
  }
}
