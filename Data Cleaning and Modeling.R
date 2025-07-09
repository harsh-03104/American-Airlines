
library(tidyverse)

# 1 ── LOAD & BASIC FILTERS ──────────────────────────────────
ticket <- read_csv("T_DB1B_Ticket.csv",
                   col_types = cols(.default = "c")) %>%
  filter(REPORTING_CARRIER == "AA")  # Filter for American Airlines

# Load Coupon Data
coupon <- read_csv("T_DB1B_COUPON.csv",
                   col_types = cols(.default = "c")) %>%
  filter(TICKET_CARRIER == "AA")     # Filter for American Airlines

# Load T100 Market Data
t100 <- read_csv("T_T100D_SEGMENT_US_CARRIER_ONLY.csv",
                 col_types = cols(.default = "c")) %>%
  filter(UNIQUE_CARRIER == "AA")     # Filter for American Airlines




# Convert key columns to numeric
ticket <- ticket %>%
  mutate(across(c(YEAR, QUARTER, PASSENGERS, ITIN_FARE, DISTANCE, COUPONS), as.numeric),
         ITIN_ID = as.character(ITIN_ID))

coupon <- coupon %>%
  mutate(across(c(YEAR, QUARTER, SEQ_NUM, COUPONS, PASSENGERS, DISTANCE), as.numeric),
         ITIN_ID = as.character(ITIN_ID))

t100 <- t100 %>%
  mutate(across(c(YEAR, QUARTER, MONTH, PASSENGERS, DISTANCE, SEATS, DEPARTURES_PERFORMED), as.numeric))


# ── 2 ── JOIN COUPON ⬅️ TICKET (adds fare, passengers per itinerary) ─────────────

coupon <- coupon %>%
  left_join(
    ticket %>%
      select(
        ITIN_ID,
        ITIN_FARE,
        TICKET_COUPONS = COUPONS,      # rename to avoid collision
        PAX_TICKET     = PASSENGERS    # itinerary-level passenger count
      ),
    by = "ITIN_ID"
  ) %>%
  mutate(
    SEGMENT_FARE = ITIN_FARE / TICKET_COUPONS   # even split across legs
  )


t100_qtr <- t100 %>%
  mutate(OD = paste(ORIGIN, DEST, sep = "-")) %>%
  group_by(YEAR, QUARTER, OD) %>%
  summarise(
    ROUTE_PAX     = sum(PASSENGERS,   na.rm = TRUE),
    ROUTE_SEATS   = sum(SEATS,        na.rm = TRUE),
    ROUTE_FLIGHTS = sum(DEPARTURES_PERFORMED, na.rm = TRUE),   # <-- new column
    AVG_DIST      = mean(DISTANCE,    na.rm = TRUE),
    .groups       = "drop"
  )


coupon <- coupon %>%
  mutate(
    SEGMENT_REV = SEGMENT_FARE * PAX_TICKET   # revenue for this flight leg
  )



route_rev <- coupon %>%
  mutate(OD = paste(ORIGIN, DEST, sep = "-")) %>%
  group_by(YEAR, QUARTER, OD) %>%
  summarise(
    ROUTE_REVENUE = sum(SEGMENT_REV, na.rm = TRUE),
    ROUTE_PAX_TIX = sum(PAX_TICKET,  na.rm = TRUE),   # pax derived from tickets
    .groups       = "drop"
  )


route_final <- t100_qtr %>%                     # seats, pax, flights, distance
  left_join(route_rev, by = c("YEAR", "QUARTER", "OD"))

route_final <- route_final %>%
  mutate(
    ORIGIN = str_split_fixed(OD, "-", 2)[, 1],
    DEST   = str_split_fixed(OD, "-", 2)[, 2]
  )

route_map_data <- route_final %>%
  left_join(airport_coords, by = c("ORIGIN" = "IATA_CODE")) %>%
  rename(ORIGIN_LAT = LATITUDE, ORIGIN_LONG = LONGITUDE) %>%
  left_join(airport_coords, by = c("DEST" = "IATA_CODE")) %>%
  rename(DEST_LAT = LATITUDE, DEST_LONG = LONGITUDE) %>%
  select(YEAR, QUARTER, ORIGIN, DEST, OD, everything())

library(tidyr)

route_map_long <- route_map_data %>%
  pivot_longer(
    cols = c(ORIGIN_LAT, ORIGIN_LONG, DEST_LAT, DEST_LONG),
    names_to = c("POINT", ".value"),
    names_pattern = "(ORIGIN|DEST)_(LAT|LONG)"
  )


write_csv(route_final, "route_final.csv")
