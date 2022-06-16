## code to prepare data included in logitr package

library(dplyr)
library(tidyr)
library(readr)
library(fastDummies)
library(mlogit)

yogurt_raw <- read_csv(here::here("data-raw", "yogurt_raw.csv"))
cars_china <- read_csv(here::here('data-raw', 'cars_china.csv'))
cars_us    <- read_csv(here::here('data-raw', 'cars_us.csv'))
runtimes   <- read_csv(here::here('data-raw', 'runtimes.csv'))

# Yogurt data ----

# Raw data variables:
# id      = individuals identifiers
# choice  = one of yoplait, dannon, hiland, weight (weight watcher)
# feat.z  = is there a newspaper feature advertisement for brand z ?
# price.z = price of brand z

# Format the yogurt dataset for use in logitr
yogurt <- yogurt_raw %>%
  mutate(obsID = seq(nrow(yogurt_raw))) %>%
  gather(brand, attributeValue, feat.yoplait:price.weight) %>%
  separate(brand, into = c("attributeName", "brand"), sep = "\\.") %>%
  spread(attributeName, attributeValue) %>%
  mutate(choice = ifelse(choice == brand, 1, 0)) %>%
  arrange(obsID) %>%
  mutate(alt = rep(seq(4), max(obsID))) %>%
  select(id, obsID, alt, choice, price, feat, brand)

# Save the formatted dataset
usethis::use_data(yogurt, overwrite = TRUE)

# Cars data ----

# Raw data variables:

# id             = individual identifiers
# obsnum         = identifier for unique choice observation
# choice         = dummy code for choice (1 or 0)
# hev            = dummy code for HEV vehicle type (1 or 0)
# phev10         = dummy code for PHEV vehicle type w/10 mile electric driving range (1 or 0)
# phev20         = dummy code for PHEV vehicle type w/20 mile electric driving range (1 or 0)
# phev40         = dummy code for PHEV vehicle type w/40 mile electric driving range (1 or 0)
# bev75          = dummy code for BEV vehicle type w/75 mile electric driving range (1 or 0)
# bev100         = dummy code for BEV vehicle type w/100 mile electric driving range (1 or 0)
# bev150         = dummy code for BEV vehicle type w/150 mile electric driving range (1 or 0)
# phevFastcharge = dummy code for whether PHEV vehicle had fast charging capability (1 or 0)
# bevFastcharge  = dummy code for whether BEV vehicle had fast charging capability (1 or 0)
# price          = price of vehicle ($USD)
# opCost         = operating cost of vehicle (US cents / mile)
# accelTime      = 0-60 mph acceleration time (seconds)
# american       = dummy code for whether American brand (1 or 0)
# japanese       = dummy code for whether Japanese brand (1 or 0)
# chinese        = dummy code for whether Chinese brand (1 or 0)
# skorean        = dummy code for whether S. Korean brand (1 or 0)
# weights        = weights for each individual computed so that the sample age and income demographics matched with those of the general car-buying population

# Save the datasets
usethis::use_data(cars_us, overwrite = TRUE)
usethis::use_data(cars_china, overwrite = TRUE)

# Simulated SP mode choice data from {apollo} package ----

apolloModeChoiceData <- apollo::apollo_modeChoiceData %>%
  # Only use SP data
  filter(SP == 1) %>%
  # Reshape data into "long" format
  pivot_longer(names_to = "var", values_to = "val", av_car:service_rail) %>%
  separate(var, into = c("var", "mode"), sep = "_") %>%
  pivot_wider(names_from = var, values_from = val) %>%
  # Fill in created NA values for access and service
  mutate(
    access = ifelse(is.na(access), 0, access),
    service = ifelse(is.na(service), 0, service)
  ) %>%
  # Dummy-code the mode and service variables
  dummy_cols(c("mode", "service")) %>%
  # Drop service 0 level
  select(-service_0) %>%
  # Rename service variables
  rename(
      service_no_frills = service_1,
      service_wifi = service_2,
      service_food = service_3) %>%
  # Make mode-specific time variables
  mutate(
      time_car = time*mode_car,
      time_bus = time*mode_bus,
      time_air = time*mode_air,
      time_rail = time*mode_rail) %>%
  # Create obsID and altID variables
  mutate(
      obsID = rep(seq(n() / 4), each = 4),
      altID = rep(seq(1, 4), times = 14*500)) %>%
  # Define dummy-coded choice column
  mutate(choice = ifelse(choice == altID, 1, 0)) %>%
  # Drop rows where alternative wasn't available
  filter(av == 1) %>%
  # Re-define altID
  group_by(obsID) %>%
  mutate(altID = seq(n())) %>%
  ungroup() %>%
  # Drop RP and SP identifiers (all SP data), and av (all available)
  select(-RP, -SP, -RP_journey, -av) %>%
  # Reorder columns
  select(
    ID, obsID, altID, qID = SP_task, choice,
    mode:time_rail, female:income, everything())

# Save the dataset
usethis::use_data(apolloModeChoiceData, overwrite = TRUE)

# Electricity data ----

data("Electricity")
electricity <- data.frame(mlogit.data(
    Electricity, id.var = "id", choice = "choice",
    varying = 3:26, shape = "wide", sep = "")) %>%
    rename(obsID = chid) %>%
    select(-idx) %>%
    mutate(choice = ifelse(choice, 1, 0)) %>%
    # Reorder columns
    select(id, obsID, choice, everything())

# Save the dataset
usethis::use_data(electricity, overwrite = TRUE)

# runtimes ----

usethis::use_data(runtimes, overwrite = TRUE)
