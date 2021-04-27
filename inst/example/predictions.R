# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# ============================================================================
# Predict Choices Using Estimated Models

# Read in saved estimated models
mnl_pref <- readRDS(here::here('inst', 'extdata', 'mnl_pref.Rds'))
mnl_wtp  <- readRDS(here::here('inst', 'extdata', 'mnl_wtp.Rds'))
mxl_pref <- readRDS(here::here('inst', 'extdata', 'mxl_pref.Rds'))
mxl_wtp  <- readRDS(here::here('inst', 'extdata', 'mxl_wtp.Rds'))

# You can predict choices for any set of alternative, such as hold out
# samples or within-sample. For this example I will predict choices on
# the full yogurt data set, which was used to estimate the model.
head(yogurt)

# Run the simulation using the preference space MNL model:
predict_mnl_pref <- predictChoices(
  model      = mnl_pref,
  alts       = yogurt,
  choiceName = "choice",
  obsIDName  = "obsID"
)
head(predict_mnl_pref)

# The results show the expected shares for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha = 0.05).

# Run the simulation using the WTP space MNL model:
predict_mnl_wtp <- predictChoices(
  model      = mnl_wtp,
  alts       = yogurt,
  choiceName = "choice",
  obsIDName  = "obsID",
  priceName  = 'price'
)
head(predict_mnl_wtp)

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the alts argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
predict_mxl_pref <- predictChoices(
  model      = mxl_pref,
  alts       = yogurt,
  choiceName = "choice",
  obsIDName  = "obsID"
)
head(predict_mxl_pref)

predict_mxl_wtp <- predictChoices(
  model      = mxl_wtp,
  alts       = yogurt,
  choiceName = "choice",
  obsIDName  = "obsID",
  priceName  = 'price'
)
head(predict_mxl_wtp)

# Save results
saveRDS(predict_mnl_pref,
        here::here('inst', 'extdata', 'predict_mnl_pref.Rds'))
saveRDS(predict_mnl_wtp,
        here::here('inst', 'extdata', 'predict_mnl_wtp.Rds'))
saveRDS(predict_mxl_pref,
        here::here('inst', 'extdata', 'predict_mxl_pref.Rds'))
saveRDS(predict_mxl_wtp,
        here::here('inst', 'extdata', 'predict_mxl_wtp.Rds'))

# Compare prediction accuracy across models
library(dplyr)

# Combine models into one data frame
predictions <- rbind(
  predict_mnl_pref, predict_mnl_wtp, predict_mxl_pref, predict_mxl_wtp)
predictions$model <- c(
  rep("mnl_pref", nrow(predict_mnl_pref)),
  rep("mnl_wtp",  nrow(predict_mnl_wtp)),
  rep("mxl_pref", nrow(predict_mxl_pref)),
  rep("mxl_wtp",  nrow(predict_mxl_wtp)))

# Compute prediction accuracy by model
predictions %>%
  filter(choice == 1) %>%
  mutate(predict_correct = (choice_predict == choice)) %>%
  group_by(model) %>%
  summarise(p_correct = sum(predict_correct) / n())

# The models all perform about the same with ~38% correct predictions
# This is significantly better than random predictions, which should be 25%
