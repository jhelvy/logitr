# ============================================================================
# Add the mlogit yogurt dataset to the logitr package
# ============================================================================

# Load libraries and functions
library(devtools)
library(roxygen2)
library(mlogit)
library(dplyr)
library(tidyr)
setwd('/Users/jhelvy/Documents/github/logitr')

# Dummy codes a vector of characters into a matrix with one level dummied out
dummyCode = function(data, dummyName) {
    levels = levels(data)
    levels = levels[-which(levels==dummyName)]
    result = matrix(0, nrow=length(data), ncol=length(levels))
    colnames(result) = levels
    for (i in 1:nrow(result)) {
        for (j in 1:ncol(result)) {
            if (data[i]==levels[j]) {
                result[i,j] = 1
            }
        }
    }
    return(result)
}

# ============================================================================
# Create data-raw folder
devtools::use_data_raw()

# Import 'Yogurt' dataset from the mlogit package
data(Yogurt)

# Format the Yogurt dataset for use in logitr
Yogurt$obsID = 1:nrow(Yogurt)
yogurt = Yogurt %>%
    gather(brand, attributeValue, feat.yoplait:price.weight) %>%
    separate(brand, into=c('attributeName', 'brand'), sep='\\.') %>%
    spread(attributeName, attributeValue) %>%
    mutate(choice=ifelse(choice==brand, 1, 0)) %>%
    arrange(obsID)

# Dummy code the brand attribute using the 'weight' (weight watcher) brand as
# the dummy brand
brandsDummyCoded = dummyCode(as.factor(yogurt$brand), 'weight')
yogurt = cbind(yogurt, brandsDummyCoded)

# Save the formatted yogurt dataset
devtools::use_data(yogurt)


# Description of 'yogurt' dataset:
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
# price.z:  price of brand z

# Source:
# Jain, Dipak C., Naufel J. Vilcassim and Pradeep K. Chintagunta (1994)
# “A random–coefficients logit brand–choice model applied to panel data”,
# Journal of Business and Economics Statistics, 12(3), 317.
# ============================================================================
