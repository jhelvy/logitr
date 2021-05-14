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
choices_mnl_pref <- predictChoices(
  model      = mnl_pref,
  alts       = yogurt,
  obsIDName  = "obsID"
)

head(choices_mnl_pref)

# Run the simulation using the WTP space MNL model:
choices_mnl_wtp <- predictChoices(
  model      = mnl_wtp,
  alts       = yogurt,
  obsIDName  = "obsID"
)

head(choices_mnl_wtp)

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same.

# Simulations can also be run using MXL models in either space:
choices_mxl_pref <- predictChoices(
  model      = mxl_pref,
  alts       = yogurt,
  obsIDName  = "obsID"
)

head(choices_mxl_pref)

choices_mxl_wtp <- predictChoices(
  model      = mxl_wtp,
  alts       = yogurt,
  obsIDName  = "obsID"
)

head(choices_mxl_wtp)

# Save results
saveRDS(choices_mnl_pref,
        here::here('inst', 'extdata', 'choices_mnl_pref.Rds'))
saveRDS(choices_mnl_wtp,
        here::here('inst', 'extdata', 'choices_mnl_wtp.Rds'))
saveRDS(choices_mxl_pref,
        here::here('inst', 'extdata', 'choices_mxl_pref.Rds'))
saveRDS(choices_mxl_wtp,
        here::here('inst', 'extdata', 'choices_mxl_wtp.Rds'))

# Compare prediction accuracy across models
library(dplyr)

# Combine models into one data frame
choices <- rbind(
  choices_mnl_pref, choices_mnl_wtp, choices_mxl_pref, choices_mxl_wtp)
choices$model <- c(
  rep("mnl_pref", nrow(choices_mnl_pref)),
  rep("mnl_wtp",  nrow(choices_mnl_wtp)),
  rep("mxl_pref", nrow(choices_mxl_pref)),
  rep("mxl_wtp",  nrow(choices_mxl_wtp)))

# Compute prediction accuracy by model
choices %>%
  filter(choice == 1) %>%
  mutate(predict_correct = (choice_predict == choice)) %>%
  group_by(model) %>%
  summarise(p_correct = sum(predict_correct) / n())

# The models all perform about the same with ~38% correct predictions
# This is significantly better than random predictions, which should be 25%
