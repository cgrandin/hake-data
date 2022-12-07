#' Load spatial catch data in
#'
#' @param fns A vector of filenames of the CSV files
#'
#' @return A list of [tibble::tibble()], one for each file
#' @export
#'
#' @examples
load_spatial_catch_data <- function(fns = here::here("data-output",
                                                     c("catch-locations-ft.csv",
                                                       "catch-locations-ss.csv",
                                                       "catch-locations-jv.csv"))){

  map(fns, ~{
    d <- read.csv(.x) |>
      as_tibble() |>
      mutate(years = year(catchdate),
             months = month(catchdate),
             days = day(catchdate),
             PFMC = as.numeric(PFMC)) |>
      filter(PFMC %in% 2:9)
  }) |>
    setNames(c("ft", "ss", "jv"))
}
