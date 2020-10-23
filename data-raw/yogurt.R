## code to prepare `yogurt` for logitr package

library(dplyr)
library(tidyr)

load(here::here("data-raw", "yogurt_raw.Rda"))

# Raw data variables:
# id:      individuals identifiers
# choice:  one of yoplait, dannon, hiland, weight (weight watcher)
# feat.z:  is there a newspaper feature advertisement for brand z ?
# price.z: price of brand z

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

# Save the formatted yogurt dataset
usethis::use_data(yogurt, overwrite = TRUE)
