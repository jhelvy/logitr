# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the reformatted 'Yogurt' data set from the mlogit package
head(yogurt)

# ============================================================================
# Estimate weighted homogeneous MNL model

# Often times, researchers oversample or undersample specific groups of
# people, resulting in a dataset that is not proportionally consistent with
# a desired sample population. To account for this, you can add weights to the
# model that proportionally increase the "weight" of specific choice
# observations. Weight values are evaluated relative to 1. For example, a
# value of 0.2 would weight the choice observation to be 1/5 of that of other
# observations, and a value of 5 would weight the choice observation to be
# 5 times of that of other observations. Weights spanning between 0.2 and 5
# would result in some choice observations being weighted as much as 10 times
# those of others.

# To enable weighting, your data should have a column variable storing the
# weight value to use for each observation. Note that the same weight value
# should be repeated across rows of alternatives from the same choice
# observation.

# To include these weights in the model estimation, provide the column name
# for the "weightsName" argument to the logitr() function. Here is an example:

# First, define the weights. Here is an example with different random weights
# for different ids
w <- data.frame(id = seq(max(yogurt$id)))
w$w <- sample(c(0.25, 0.5, 1, 2, 4), nrow(w), replace = TRUE)
yogurt_w <- merge(yogurt, w, by = "id")

# Run a MNL model in the Preference Space with weights:
mnl_pref_w = logitr(
  data       = yogurt_w,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
  weightsName = 'w') # Including this parameter enables weighting

# Print a summary of the results:
summary(mnl_pref_w)

# Compare the coefficients and log-likelihood from the weighted model to
# those of the unweighted model:
mnl_pref <- readRDS(here::here('examples', 'results', 'mnl_pref_w.Rds'))
coef_compare <- data.frame(
  Unweighted = coef(mnl_pref),
  Weighted   = coef(mnl_pref_w))
logLik_compare <- c("Unweighted" = mnl_pref$logLik, "Weighted" = mnl_pref_w$logLik)
coef_compare
logLik_compare

# Save results
saveRDS(mnl_pref_w,
        here::here('examples', 'results', 'mnl_pref_w.Rds'))
