# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the reformatted 'Yogurt' data set from the mlogit package
head(yogurt)

# ============================================================================
# Assess design of experiment sample size

# If you are designing a conjoint survey, you probably would like to know
# the sample size required to produce an informative model. In the example
# below, assume that the yogurt data was not a completed survey but rather a
# blank design of experiment with no observed choices. The sampleSizer function
# fills out the survey with random choices and estimates a model. The function
# does this multiple times with an increasing number of observations, set by
# the "nbreaks" argument. While the coefficients in those models are
# meaningless, the standard errors on the coefficients are informative. The
# example below estimates 10 separate models and then plots the standard errors
# against the number of observations@.

test <- sampleSizer(
  data       = yogurt,
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
  nbreaks    = 10,
  plot       = TRUE)
