## code to prepare data to include in logitr package

library(dplyr)
library(tidyr)
library(readr)

yogurt_raw <- read_csv(here::here("data-raw", "yogurt_raw.csv"))
cars_china <- read_csv(here::here('data-raw', 'cars_china.csv'))
cars_us    <- read_csv(here::here('data-raw', 'cars_us.csv'))

# Yogurt data ---------------------------------------------------------------

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
  left_join(data.frame(
    brand = as.character(c("dannon", "hiland", "weight", "yoplait")),
    dannon = c(1, 0, 0, 0),
    hiland = c(0, 1, 0, 0),
    weight = c(0, 0, 1, 0),
    yoplait = c(0, 0, 0, 1)
  )) %>%
  select(
    id, obsID, alt, choice, price, feat, brand, dannon, hiland, weight,
    yoplait
  )

# Save the formatted dataset
usethis::use_data(yogurt, overwrite = TRUE)

# Cars data -------------------------------------------------------------------

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

# Combine data into single data frame
cars_china$country <- "china"
cars_us$country <- "us"
cars <- rbind(cars_china, cars_us)
cars <- select(cars, country, everything())

# Save the formatted datasets
usethis::use_data(cars, overwrite = TRUE)




