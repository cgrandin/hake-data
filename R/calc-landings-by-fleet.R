#' Sum and count catch (landings or discards)
#'
#' @details
#' Aggregate by Fishery, Year, and Month.
#' Returns a data frame with the following five columns:
#' Fishery, Year, Month, weightKg, numLandings
#' Where Fishery will be the same for all records (fishery string)
#'
#' @param lst A list of two data frames, one for landings and one for
#' discards. Output from [load_catch_data()]
#' @param fleet The fleet to use, either 'ft' or 'ss'
#' @param convert_weight If `TRUE`, convert pounds to kilos
#'
#' @return A [tibble::tibble()] with the catch data by Fishery, Year,
#' and Month, and catch in kg. Also the number of landings,
#' but not the number of discard events. If NU
#' @export
#'
#' @examples
calc_catch_by_fleet_yr_month <- function(lst,
                                         convert_weight = TRUE){

  if(!"list" %in% class(lst)){
    stop("`lst` is not a list.")
  }
  if(length(lst) != 2){
    stop("`lst` is not length 2.")
  }
  if(is.null(lst[[1]])){
    stop("The landings data frame is `NULL` (lst[[1]]).")
  }
  if(is.null(lst[[2]])){
    stop("The discards data frame is `NULL` (lst[[2]]).")
  }
  if(is.null(nrow(lst[[1]])) || nrow(lst[[1]]) == 0){
    stop("The landings data frame has no rows (lst[[1]]).")
  }
  if(is.null(nrow(lst[[2]])) || nrow(lst[[2]]) == 0){
    stop("The discrads data frame has no rows (lst[[2]]).")
  }

  landings_col <- "CONVERTED.WGHT.LBS."
  discards_col <- "RELEASED.WT"
  if(!landings_col %in% names(lst[[1]])){
    stop("The '", landings_col, "' column was not found ",
         " in the input data frame `d[[1]]`.")
  }
  if(!discards_col %in% names(lst[[2]])){
    stop("The '", discards_col, "' column was not found ",
         " in the input data frame `d[[2]]`.")
  }

  calc_catch <- function(d, catch_col_sym, trip_type_sym){
    j <- map(c("JV",
               "Shoreside",
               "FreezerTrawler"),
             function(fleet){
               if(fleet == "JV"){
                 d_fleet <- d |>
                   filter(grepl("JV", !!trip_type_sym))
               }else if(fleet == "FreezerTrawler"){
                 d_fleet <- d |>
                   filter(VRN %in% ft_vessels$cfv_num)
               }else if(fleet == "Shoreside"){
                 d_fleet <- d |>
                   filter(!grepl("JV", !!trip_type_sym)) |>
                   filter(!VRN %in% ft_vessels$cfv_num)
               }
               d_simp <- d_fleet |>
                 select(Year, Month, !!catch_col_sym)

               if(convert_weight){
                 d_simp <- d_simp |>
                   mutate(!!catch_col_sym := !!catch_col_sym / CONV_LBS_KILOS)
               }

               d_simp <- d_simp |>
                 group_by(Year, Month) |>
                 summarize(catchKg = sum(!!catch_col_sym, na.rm = TRUE),
                           numLandings = n()) |>
                 ungroup() |>
                 mutate(Fishery = !!fleet) |>
                 select(Fishery, everything())
             })
  }

  # Landings
  d <- lst[[1]]
  catch_col_sym <- sym(landings_col)
  trip_type_sym <- sym("LICENCE.TRIP.TYPE")
  x <- calc_catch(d, catch_col_sym, trip_type_sym)

  # Discards
  d <- lst[[2]]
  catch_col_sym <- sym(discards_col)
  trip_type_sym <- sym("TRIP.TYPE")
  y <- calc_catch(d, catch_col_sym, trip_type_sym)
  # Set all JV discards to zero (they all are except one which is an error)
  y[[1]]$catchKg <- 0

  # Sum landings and discards for each fleet
  map2(x, y, ~{
    joined <- .x |> full_join(.y, by = c("Fishery", "Year", "Month"))
    joined[is.na(joined)] <- 0
    joined <- joined |>
      mutate(catchKg = catchKg.x+ catchKg.y) |>
      select(Fishery, Year, Month, catchKg, numLandings.x) |>
      rename(numLandings = numLandings.x)
  }) |>
    map_df(~{.x})
}

