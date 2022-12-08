#' Load spatial catch data in
#'
#' @param fns A vector of filenames of the CSV files
#' @param inc_areas A vector of areas to include
#'
#' @return A list of [tibble::tibble()], one for each file
#' @export
#'
#' @examples
load_spatial_catch_data <- function(fns = here::here("data-output",
                                                     c("catch-locations-ft.csv",
                                                       "catch-locations-ss.csv",
                                                       "catch-locations-jv.csv")),
                                    inc_areas = 3:9){

  map(fns, ~{
    d <- read.csv(.x) |>
      as_tibble() |>
      mutate(years = year(catchdate),
             months = month(catchdate),
             days = day(catchdate),
             PFMC = as.numeric(PFMC)) |>
      filter(PFMC %in% inc_areas)
  }) |>
    setNames(c("ft", "ss", "jv"))
}
