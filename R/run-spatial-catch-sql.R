#' Run SQL queries on the gffos database
#'
#' @param fns A vector of filenames of the SQL files continaing
#' SQL code in plaintext
#'
#' @return A list of [tibble::tibble()], one for each query
#' @export
run_spatial_catch_sql <- function(fns = here::here("sql",
                                                   c("catch-locations-ft.sql",
                                                     "catch-locations-ss.sql",
                                                     "catch-locations-jv.sql"))){

  purrr::map(fns, ~{
    d <- gfdata::run_sql("gffos", readr::read_file(.x)) |>
      tibble::as_tibble()
    names(d) <- c("ft", "ss", "jv")
    d
  })
}