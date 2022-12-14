library(tidyverse)
library(lubridate)
library(RColorBrewer)
library(gfdata)
library(gfplot)

r_dir <- here::here("R")
source(file.path(r_dir, "calc-age-props.R"))
source(file.path(r_dir, "calc-landings-by-fleet.R"))
source(file.path(r_dir, "calc-num-fish-aged.R"))
source(file.path(r_dir, "calc-num-trips-hauls-sampled-age.R"))
source(file.path(r_dir, "create-age-proportion-files.R"))
source(file.path(r_dir, "create-catch-fleet-df.R"))
source(file.path(r_dir, "create-catch-total-df.R"))
source(file.path(r_dir, "create-depths-by-year.R"))
source(file.path(r_dir, "create-landings-data-files.R"))
source(file.path(r_dir, "create-spatial-catch-plots.R"))
source(file.path(r_dir, "create-waa.R"))
source(file.path(r_dir, "fit-lw.R"))
source(file.path(r_dir, "load-catch-data.R"))
source(file.path(r_dir, "load-sample-data.R"))
source(file.path(r_dir, "load-spatial-catch-data.R"))
source(file.path(r_dir, "plot-area-dist-catch.R"))
source(file.path(r_dir, "plot-catch.R"))
source(file.path(r_dir, "plotcolour.R"))
source(file.path(r_dir, "plot-hake-depths.R"))
source(file.path(r_dir, "plot-hake-lengths.R"))
source(file.path(r_dir, "plot-hake-weights.R"))
source(file.path(r_dir, "plot-hake-ages.R"))
source(file.path(r_dir, "run-spatial-catch-sql.R"))
source(file.path(r_dir, "sample-summary.R"))
source(file.path(r_dir, "theme.R"))

theme_set(hake_theme())

# Turn off the following type messages (they're unnecessary):
# `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.
options(dplyr.summarise.inform = FALSE)
options(scipen = 999)

# Conversion factor since FOS records catch in pounds
CONV_LBS_KILOS   <- 2.20462262

fns <- c()

# vessel ids for freezer trawlers
ft_vessels <- tibble(name = c("Viking Enterprise", "Northern Alliance",
                              "Osprey #1", "Raw Spirit",
                              "Pacific Legacy No. 1", "Sunderoey",
                              "Viking Alliance"),
                     id = c(568, 592, 569, 595,
                            608, 609, 1727),
                     cfv_num = c(310913, 312275, 310988, 312405,
                                 313334, 313464, 313224))

areas_lu <- tibble(code = c("00", "01", "02",
                            "03", "04", "05",
                            "06", "07", "08",
                            "09", "10", "11",
                            "22", "23", "68",
                            "99"),
                   short_name = c("UK", "4B", "3B",
                                  "3C", "3D", "5A",
                                  "5B", "5C", "5D",
                                  "5E", "AL", "OS",
                                  "OR", "CA", "4A",
                                  "UD"),
                   desc = c("Unknown", "SOG", "Cape Flattery",
                            "Southwest WCVI", "Northwest WCVI", "South QC Sound",
                            "North QC Sound", "South Hecate", "North Hecate",
                            "WC Haida Gwaii", "Alaska", "British Columbia Offshore",
                            "Oregon", "California", "Puget Sound",
                            "Undefined"))

areas_names_lu <- c("0" = areas_lu$desc[1],
                    "1" = areas_lu$desc[2],
                    "2" = areas_lu$desc[3],
                    "3" = areas_lu$desc[4],
                    "4" = areas_lu$desc[5],
                    "5" = areas_lu$desc[6],
                    "6" = areas_lu$desc[7],
                    "7" = areas_lu$desc[8],
                    "8" = areas_lu$desc[9],
                    "9" = areas_lu$desc[10],
                    "10" = areas_lu$desc[11],
                    "11" = areas_lu$desc[12],
                    "22" = areas_lu$desc[13],
                    "23" = areas_lu$desc[14],
                    "68" = areas_lu$desc[15],
                    "99" = areas_lu$desc[16])

months_lu <- c("1" = "Jan",
               "2" = "Feb",
               "3" = "Mar",
               "4" = "Apr",
               "5" = "May",
               "6" = "Jun",
               "7" = "Jul",
               "8" = "Aug",
               "9" = "Sep",
               "10" = "Oct",
               "11" = "Nov",
               "12" = "Dec")
