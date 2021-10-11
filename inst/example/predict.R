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

# You can make predictions for any set of alternatives, such as hold out
# samples or within-sample. By default, if no new data are provided via the
# newdata argument, then predictions will be made for the original data.
predictions <- predict(mnl_pref)
head(predictions)

# If you only want the predictions to be returned without the data, set
# returnData = FALSE
predictions <- predict(mnl_pref, returnData = FALSE)
head(predictions)

# To make predictions for a new set of alternatives, use the newdata argument.
# In this example, I just use two of the choice observations from the
# yogurt dataset:
alts <- subset(
  yogurt, obsID %in% c(42, 13),
  select = c('obsID', 'alt', 'price', 'feat', 'brand'))

alts

# Compute choice probabilities for the alts data frame using the preference
# space MNL model:
probs_mnl_pref <- predict(
  mnl_pref,
  newdata  = alts,
  obsID = "obsID",
  ci = 0.95
)

probs_mnl_pref

# The results show the expected choice probabilities for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting ci to a different
# value (e.g. a 90% CI is obtained with ci = 0.90).

# Compute choice probabilities using the WTP space MNL model:
probs_mnl_wtp <- predict(
  mnl_wtp,
  newdata = alts,
  obsID = "obsID",
  ci = 0.95
)

probs_mnl_wtp

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'price' is the name
# of the price attribute in the alts argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
probs_mxl_pref <- predict(
  mxl_pref,
  newdata = alts,
  obsID = "obsID"
)

probs_mxl_pref

probs_mxl_wtp <- predict(
  mxl_wtp,
  newdata = alts,
  obsID = "obsID"
)

probs_mxl_wtp

# Save results
saveRDS(probs_mnl_pref, here::here('inst', 'extdata', 'probs_mnl_pref.Rds'))
saveRDS(probs_mnl_wtp,  here::here('inst', 'extdata', 'probs_mnl_wtp.Rds'))
saveRDS(probs_mxl_pref, here::here('inst', 'extdata', 'probs_mxl_pref.Rds'))
saveRDS(probs_mxl_wtp,  here::here('inst', 'extdata', 'probs_mxl_wtp.Rds'))

# Plot simulation results from each model:
library(ggplot2)
library(dplyr)

probs <- rbind(
  probs_mnl_pref, probs_mnl_wtp, probs_mxl_pref, probs_mxl_wtp) %>%
  mutate(
    model = c(rep("mnl_pref", 8), rep("mnl_wtp", 8),
              rep("mxl_pref", 8), rep("mxl_wtp", 8)),
    alt = rep(c("dannon", "hiland", "weight", "yoplait"), 8),
    obs = paste0("Observation ID: ", obsID)
  )

probs <- ggplot(probs, aes(x = alt, y = prob_mean, fill = model)) +
    geom_bar(stat = 'identity', width = 0.7, position = "dodge") +
    geom_errorbar(aes(ymin = prob_low, ymax = prob_high),
                  width = 0.2, position = position_dodge(width = 0.7)) +
    facet_wrap(vars(obs)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = 'Alternative', y = 'Expected Choice Probabilities') +
    theme_bw()

ggsave(here::here('vignettes', 'probs.png'),
       probs, width = 7, height = 4, dpi = 300)





# You can also predict choices using the type argument. For this example, I
# will predict choices for every alternative in the data used to estimate the
# model using the preference space MNL model:
choices_mnl_pref <- predict(mnl_pref, type = "choices")
head(choices_mnl_pref)

# You can also make predictions using WTP space models:
choices_mnl_wtp <- predict(mnl_wtp, type = "choices")
head(choices_mnl_wtp)

# Since these two models are equivalent except in different spaces, the
# simulation results should be approximately the same.

# Simulations can also be run using MXL models in either space:
choices_mxl_pref <- predict(mxl_pref, type = "choices")
head(choices_mxl_pref)

choices_mxl_wtp <- predict(mxl_wtp, type = "choices")
head(choices_mxl_wtp)

# Save results
saveRDS(choices_mnl_pref, here::here('inst', 'extdata', 'choices_mnl_pref.Rds'))
saveRDS(choices_mnl_wtp,  here::here('inst', 'extdata', 'choices_mnl_wtp.Rds'))
saveRDS(choices_mxl_pref, here::here('inst', 'extdata', 'choices_mxl_pref.Rds'))
saveRDS(choices_mxl_wtp,  here::here('inst', 'extdata', 'choices_mxl_wtp.Rds'))

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
  dplyr::filter(choice == 1) %>%
  mutate(predict_correct = (choice_predict == choice)) %>%
  group_by(model) %>%
  summarise(p_correct = sum(predict_correct) / n())

# The models all perform about the same with ~38% correct predictions
# This is significantly better than random predictions, which should be 25%
