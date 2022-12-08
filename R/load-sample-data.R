#' Load the samples data into an RDS file
#'
#' @param fns A vector length 2 for sample data filenames to load
#' @param fn_rds The name of the RDS file to create/load
#' @param ft_vessels_lu The Freezer trawler lookup table. Must have columns
#' `name` and `id`
#' @param rebuild_rds If `TRUE` and the RDS file exists, rebuild it from the
#' raw CSV files given by `fns`. If `FALSE`, do not rebuild it, just load and
#' return the RDS file given by `fn_rds`
#' @param inc_areas A vector of areas to include
#' @param inc_major_minors A vector of length two, consisting of a major area
#' and minor area. This major/minor area combo will be included in the data
#' along with the data from the major areas defined in `inc_areas`
#'
#' @return A list of 3 [tibble::tibble()] containing Freezer trawlers,
#' Shoreside, and JV sample data. All column names are lower case
#' @export
load_sample_data <- function(fns = here::here("data-sample",
                                              c("hake_domestic_obs_len_wt_age.csv",
                                                "hake_jv_obs_len_wt_age.csv")),
                             fn_rds = here::here("data-sample",
                                                 "samples.rds"),
                             ft_vessels_lu = NULL,
                             rebuild_rds = FALSE,
                             inc_areas = 3:9,
                             inc_major_minor = c(1, 20)){

  if(file.exists(fn_rds) && !rebuild_rds){
    message("Loading pre-made samples RDS file. To rebuild it from the CSVs, ",
            "set argument `rebuild_rds` to `TRUE`. File:\n", fn_rds, "\n")
    return(readRDS(fn_rds))
  }

  if(is.null(ft_vessels_lu)){
    stop("`ft_vessels_lu` data frame is missing. It needs to have at least ",
         "two columns: name and id")
  }

  if(file.exists(fn_rds) && rebuild_rds){
    message("Rebuilding the samples RDS file:\n", fn_rds, "\n")
  }

  if(!file.exists(fn_rds)){
    message("Creating the samples RDS file:\n", fn_rds, "\n")
  }

  dir.create(dirname(fn_rds), showWarnings = FALSE)

  d <- map(fns, ~{
    if(!file.exists(.x)){
      stop("The following data file does not exist:\n",
           .x, call. = FALSE)
    }
    tmp <- read.csv(.x) |>
      as_tibble() |>
      rename(year = Year,
             area = MAJOR_STAT_AREA_CODE,
             length = Length_cm,
             weight =Weight_g,
             age = SPECIMEN_AGE)
    tmp_majors <- tmp |>
      filter(area %in% inc_areas)
    tmp_major_minors <- tmp |>
      filter(area %in% inc_major_minor[1] &
               MINOR_STAT_AREA_CODE %in% inc_major_minor[2])
    out <- bind_rows(tmp_majors, tmp_major_minors)
    names(out) <- tolower(names(out))
    out
  }) |>
    setNames(c("domestic", "jv"))

  # Extract Freezer Trawlers and Shoreside from 'domestic'
  ft <- d$domestic |>
    filter(vessel_id %in% ft_vessels_lu$id)
  ss <- d$domestic |>
    filter(!vessel_id %in% ft_vessels_lu$id)

  d <- list(ft = ft,
            ss = ss,
            jv = d$jv)
  saveRDS(d, fn_rds)

  d
}