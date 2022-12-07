#' Run SQL queries on the gffos database
#'
#' @param fns A vector of filenames of the SQL files containing
#' SQL code in plaintext
#' @param write_files If `TRUE`, write the output to DAT files
#' If `FALSE`, return the list of tibbles
#'
#' @return A list of [tibble::tibble()], one for each query
#' @export
run_spatial_catch_sql <- function(fns = here::here("sql",
                                                   c("catch-locations-ft.sql",
                                                     "catch-locations-ss.sql",
                                                     "catch-locations-jv.sql")),
                                  write_files = TRUE){

  lst <- purrr::map(fns, ~{
    gfdata::run_sql("gffos", readr::read_file(.x)) |>
      tibble::as_tibble()
  }) |>
    setNames(c("ft", "ss", "jv"))


  if(!write_files){
    return(lst)
  }

  dir.create(here::here("data-output"), showWarnings = FALSE)
  out_fns <- gsub("\\.sql", ".csv", basename(fns))
  walk2(lst, out_fns, ~{
    # Need as.data.frame() here to avoid quoted headers
    write_csv(as.data.frame(.x),
              here::here("data-output",
                         .y),
              quote = "none")
  })
  invisible()
}