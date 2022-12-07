library(tidyverse)
require(lubridate)
require(RColorBrewer)
require(plyr)

# Turn off the following type messages (they're unnecessary):
# `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.
options(dplyr.summarise.inform = FALSE)
options(scipen = 999)

# Conversion factor since FOS records catch in pounds
CONV_LBS_KILOS   <- 2.20462262

# vessel ids for freezer trawlers
ft_vessels <- tibble(name = c("Viking Enterprise", "Northern Alliance",
                              "Osprey #1", "Raw Spirit",
                              "Pacific Legacy No. 1", "Sunderoey",
                              "Viking Alliance"),
                     id = c(568, 592, 569, 595,
                            608, 609, 1727),
                     cfv_num = c(310913, 312275, 310988, 312405,
                                 313334, 313464, 313224))

areas_lu <- tibble(code = c("00", "01", "02", "03",
                            "04", "05", "06", "07",
                            "08", "09", "10", "11",
                            "22", "23", "68", "99"),
                   short_name = c("UK", "4B", "3B", "3C",
                                  "3D", "5A", "5B", "5C",
                                  "5D", "5E", "AL", "OS",
                                  "OR", "CA", "4A", "UD"),
                   desc = c("Unknown",
                            "SOG", "Cape Flattery", "Southwest WCVI",
                            "Northwest WCVI", "South QC Sound", "North QC Sound",
                            "South Hecate", "North Hecate", "WC Haida Gwaii",
                            "Alaska", "British Columbia Offshore", "Oregon",
                            "California", "Puget Sound", "Undefined"))

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
