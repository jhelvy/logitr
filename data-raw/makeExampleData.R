# ============================================================================
# Add the mlogit yogurt dataset to the logitr package
# ============================================================================
setwd('/Users/jhelvy/Documents/github/logitr')

# Load libraries
library(devtools)
library(roxygen2)
library(mlogit)
library(dplyr)
library(tidyr)

# Create data-raw folder
devtools::use_data_raw()

# Load the 'Yogurt' dataset from the mlogit package
data(Yogurt)

# Format the Yogurt dataset for use in logitr
yogurt = Yogurt %>%
    mutate(obsID = seq(nrow(Yogurt))) %>%
    gather(brand, attributeValue, feat.yoplait:price.weight) %>%
    separate(brand, into=c('attributeName', 'brand'), sep='\\.') %>%
    spread(attributeName, attributeValue) %>%
    mutate(choice=ifelse(choice==brand, 1, 0)) %>%
    arrange(obsID) %>%
    mutate(alt = rep(seq(4), max(obsID))) %>%
    left_join(data.frame(
        brand   = as.character(c('dannon', 'hiland', 'weight', 'yoplait')),
        dannon  = c(1, 0, 0, 0),
        hiland  = c(0, 1, 0, 0),
        weight  = c(0, 0, 1, 0),
        yoplait = c(0, 0, 0, 1))) %>%
    select(id, obsID, alt, choice, price, feat, brand, dannon, hiland, weight,
           yoplait)

# Save the formatted yogurt dataset
usethis::use_data(yogurt, overwrite=TRUE)

# Description of 'Yogurt' dataset, from the mlogit package:
# ============================================================================
# The example data set is the 'Yogurt' dataset from the mlogit package
# This file reformats the dataset into a form appropriate for use in the
# logitr package.

# Description of the data:
# A cross-section dataset of choices of yogurt brands
# Number of observations: 2412
# Observation:            Individuals
# Country:                United States

# Variables:
# id:      individuals identifiers
# choice:  one of yoplait, dannon, hiland, weight (weight watcher)
# feat.z:  is there a newspaper feature advertisement for brand z ?
# price.z: price of brand z

# Source:
# Jain, Dipak C., Naufel J. Vilcassim and Pradeep K. Chintagunta (1994)
# “A random–coefficients logit brand–choice model applied to panel data”,
# Journal of Business and Economics Statistics, 12(3), 317.
# ============================================================================
