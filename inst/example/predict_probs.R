# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# ============================================================================
# Predict Expected Choice Probabilities Using Estimated Models

# Read in saved estimated models
mnl_pref <- readRDS(here::here('inst', 'extdata', 'mnl_pref.Rds'))
mnl_wtp  <- readRDS(here::here('inst', 'extdata', 'mnl_wtp.Rds'))
mxl_pref <- readRDS(here::here('inst', 'extdata', 'mxl_pref.Rds'))
mxl_wtp  <- readRDS(here::here('inst', 'extdata', 'mxl_wtp.Rds'))

# Create a set of alternatives for which to predict choice probabilities.
# Each row is an alternative and each column an attribute.
# In this example, I just use two of the choice observations from the
# yogurt dataset:
alts <- subset(yogurt, obsID %in% c(42, 13),
               select = c('obsID', 'price', 'feat', 'brand'))
alts

# Compute choice probabilities using the preference space MNL model:
probs_mnl_pref <- predictProbs(
  model     = mnl_pref,
  alts      = alts,
  obsIDName = "obsID"
)

probs_mnl_pref

# The results show the expected choice probabilities for each alternative.
# The low and high values show a 95% confidence interval, estimated using
# simulation. You can change the CI level by setting alpha to a different
# value (e.g. a 90% CI is obtained with alpha = 0.05).

# Compute choice probabilities using the WTP space MNL model:
probs_mnl_wtp <- predictProbs(
  model     = mnl_wtp,
  alts      = alts,
  obsIDName = "obsID"
)

probs_mnl_wtp

# Since these two models are equivalent except in different spaces, the
# simulation results should be the same. Note that 'priceName' is the name
# of the price attribute in the alts argument and must be included for
# WTP space models.

# Simulations can also be run using MXL models in either space:
probs_mxl_pref <- predictProbs(
  model     = mxl_pref,
  alts      = alts,
  obsIDName = "obsID"
)

probs_mxl_pref

probs_mxl_wtp <- predictProbs(
  model     = mxl_wtp,
  alts      = alts,
  obsIDName = "obsID"
)

probs_mxl_wtp

# Save results
saveRDS(probs_mnl_pref,
        here::here('inst', 'extdata', 'probs_mnl_pref.Rds'))
saveRDS(probs_mnl_wtp,
        here::here('inst', 'extdata', 'probs_mnl_wtp.Rds'))
saveRDS(probs_mxl_pref,
        here::here('inst', 'extdata', 'probs_mxl_pref.Rds'))
saveRDS(probs_mxl_wtp,
        here::here('inst', 'extdata', 'probs_mxl_wtp.Rds'))

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

ggsave(here::here('man', 'figures', 'probs.png'),
       probs, width = 7, height = 4, dpi = 300)
