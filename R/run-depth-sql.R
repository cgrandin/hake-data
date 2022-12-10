#'Run the catch depth SQL query on the gffos database
#'
#' @param fn  Filenames of the SQL file containing the SQL code in plaintext
#' @param write_file If `TRUE`, write the output to a CSV file.  If `FALSE`,
#' return the tibble
#'
#' @return A [tibble::tibble()] or Nothing
#' @export
run_depth_sql <- function(fn = here::here("sql", "catch-depths.sql"),
                          write_file = TRUE){

  d <- gfdata::run_sql("gffos", readr::read_file(fn)) |>
    tibble::as_tibble()

  if(!write_file){
    return(lst)
  }

  out_fn <- gsub("\\.sql", ".csv", basename(fn))
  write_csv(as.data.frame(d),
            here::here("data-catch",
                       out_fn),
            quote = "none")
  invisible()
}