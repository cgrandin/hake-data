#' Load spatial catch data in
#'
#' @param fns A vector of filenames of the CSV files
#' @param inc_areas A vector of areas to include
#'
#' @return A list of [tibble::tibble()], one for each file
#' @export
#'
#' @examples
load_spatial_catch_data <- function(fns = here::here("data-catch",
                                                     c("catch-locations-ft.csv",
                                                       "catch-locations-ss.csv",
                                                       "catch-locations-jv.csv")),
                                    inc_areas = 3:9){

  depth_fn <- here::here("data-catch", "catch-depths.csv")
  if(!file.exists(depth_fn)){
    stop("File `", depth_fn, "` does not exist. You must run the SQL query ",
         "`sql/catch-depths.sql` to make this", call. = FALSE)
  }
  depths <- read.csv(depth_fn) |>
    as_tibble()

  map(fns, ~{
    d <- read.csv(.x) |>
      as_tibble() |>
      full_join(depths, by = "FID") |>
      rename(catchdate = catchdate.x) |>
      select(-c(vessel.y,
                catch.y,
                X.y,
                Y.y,
                PFMC.y,
                catchdate.y,
                X.1)) |>
      rename(vessel = vessel.x,
             catch = catch.x,
             X = X.x,
             Y = Y.x,
             PFMC = PFMC.x) |>
      mutate(years = year(catchdate),
             months = month(catchdate),
             days = day(catchdate),
             PFMC = as.numeric(PFMC)) |>
      filter(PFMC %in% inc_areas)
  }) |>
    setNames(c("ft", "ss", "jv"))
}