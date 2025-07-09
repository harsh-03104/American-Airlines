library(dplyr)
library(readr)

route_wide <- read_csv("route_final.csv")           # your KPI table
airports    <- read_csv("usa-airports.csv") %>%     # the coord file
  select(iata, latitude, longitude) %>% 
  mutate(iata = toupper(iata))

route_wide <- route_wide %>%                       # add lat/long once
  left_join(airports, by = c("ORIGIN" = "iata")) %>%
  rename(ORIGIN_LAT = latitude, ORIGIN_LONG = longitude) %>%
  left_join(airports, by = c("DEST" = "iata")) %>%
  rename(DEST_LAT   = latitude, DEST_LONG   = longitude) %>%
  filter(!is.na(ORIGIN_LAT) & !is.na(DEST_LAT))     # drop unmatched

write_csv(route_wide, "route_wide.csv")     
