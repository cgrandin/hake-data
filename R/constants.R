library(tidyverse)
require(lubridate)

# Turn off the following type messages (they're unnecessary):
# `summarise()` has grouped output by 'x'. You can override using the `.groups` argument.
options(dplyr.summarise.inform = FALSE)

# Conversion factor since FOS records catch in pounds
CONV_LBS_KILOS   <- 2.20462262

# vessel ids for freezer trawlers
ft_vessels <- tibble(name = c("Viking Enterprise",
                              "Northern Alliance",
                              "Osprey #1",
                              "Raw Spirit",
                              "Pacific Legacy No. 1",
                              "Sunderoey",
                              "Viking Alliance"),
                     id = c(568,
                            592,
                            569,
                            595,
                            608,
                            609,
                            1727),
                     cfv_num = c(310913,
                                 312275,
                                 310988,
                                 312405,
                                 313334,
                                 313464,
                                 313224))
