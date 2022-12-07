library(tidyverse)
library(lubridate)
data <- read.csv("LandingsSpeciesDateDMP.csv", skip = 1, header = TRUE, stringsAsFactors = FALSE)
## Strip the time off the LANDING.DATE field and convert to Date object
data$LANDING.DATE <- gsub(" 00:00","",data$LANDING.DATE)
data$LANDING.DATE <- as.Date(data$LANDING.DATE, "%B %d %Y")

VIKING.ENTERPRISE <- 310913
NORTHERN.ALLIANCE <- 312275
OSPREY.NO.1 <- 310988
RAW.SPIRIT <- 312405
PACIFIC.LEGACY.NO.1 <- 313334
SUNDEROEY <-  313464
VIKING.ALLIANCE <- 313224

ft_vessels <- c(VIKING.ENTERPRISE,
                NORTHERN.ALLIANCE,
                OSPREY.NO.1,
                RAW.SPIRIT,
                PACIFIC.LEGACY.NO.1,
                SUNDEROEY,
                VIKING.ALLIANCE)

fresh_frozen <- data %>%
  mutate(yr = year(LANDING.DATE)) %>%
  filter(yr == 2020) %>%
  filter(LANDING.PORT != "FRENCH CREEK") %>%
  filter(CATCH.STATE != "LIVE") %>%
  select(VESSEL, CATCH.STATE, CONVERTED.WGHT.LBS.) %>%
  group_by(CATCH.STATE) %>%
  summarize(landings = sum(CONVERTED.WGHT.LBS.))

ft <- data %>%
  mutate(yr = year(LANDING.DATE)) %>%
  filter(yr == 2020) %>%
  filter(LANDING.PORT != "FRENCH CREEK") %>%
  filter(VRN %in% ft_vessels) %>%
  pull(CONVERTED.WGHT.LBS.) %>%
  sum()

ss <- data %>%
  mutate(yr = year(LANDING.DATE)) %>%
  filter(yr == 2020) %>%
  filter(LANDING.PORT != "FRENCH CREEK") %>%
  filter(!VRN %in% ft_vessels) %>%
  pull(CONVERTED.WGHT.LBS.) %>%
  sum()

frozen <- data %>%
  mutate(yr = year(LANDING.DATE)) %>%
  filter(yr == 2020) %>%
  filter(LANDING.PORT != "FRENCH CREEK") %>%
  filter(CATCH.STATE == "FROZEN") %>%
  select(VESSEL, VRN, CATCH.STATE, CONVERTED.WGHT.LBS.) %>%
  filter(!VRN %in% ft_vessels) %>%
  group_by(VESSEL) %>%
  summarize(landings = sum(CONVERTED.WGHT.LBS.))

