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

# By default, the predict function returns probabilities:
probs <- predict(mnl_pref)
head(probs)

# If you want the original data to also be returned in the data frame, set
# returnData = TRUE
probs <- predict(mnl_pref, returnData = TRUE)
head(probs)

# To make predictions for a new set of alternatives, use the newdata argument.
# In this example, I just use two of the choice observations from the
# yogurt dataset:
data <- subset(
  yogurt, obsID %in% c(42, 13),
  select = c('obsID', 'alt', 'price', 'feat', 'brand'))

data

# Compute choice probabilities using the preference space MNL model:
probs_mnl_pref <- predict(
  mnl_pref,
  newdata = data,
  obsID = "obsID",
)

probs_mnl_pref

# You can also get a confidence interval of predicted probabilities by including
# the `ci` argument. For example, a 95% CI is obtained with `ci = 0.95`:
probs_mnl_pref <- predict(
  mnl_pref,
  newdata = data,
  obsID = "obsID",
  ci = 0.95
)

probs_mnl_pref

# Probabilities can also be computed using WTP space models, but you have to
# specify the column in "newdata" that corresponds to "price":
probs_mnl_wtp <- predict(
  mnl_wtp,
  newdata = data,
  obsID = "obsID",
  price = "price",
  ci = 0.95
)

probs_mnl_wtp

# Simulations can also be run using MXL models in either space:
probs_mxl_pref <- predict(
  mxl_pref,
  newdata = data,
  obsID = "obsID",
  ci = 0.95
)

probs_mxl_pref

probs_mxl_wtp <- predict(
  mxl_wtp,
  newdata = data,
  obsID = "obsID",
  price = "price",
  ci = 0.95
)

probs_mxl_wtp

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

probs <- ggplot(probs, aes(x = alt, y = prob_predict, fill = model)) +
    geom_bar(stat = 'identity', width = 0.7, position = "dodge") +
    geom_errorbar(aes(ymin = prob_predict_lower, ymax = prob_predict_upper),
                  width = 0.2, position = position_dodge(width = 0.7)) +
    facet_wrap(vars(obs)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(x = 'Alternative', y = 'Expected Choice Probabilities') +
    theme_bw()

ggsave(here::here('vignettes', 'probs.png'),
       probs, width = 7, height = 4, dpi = 300)




# You can also predict choices using the type argument. For this example, I
# will predict choices for every alternative in the data used to estimate the
# model using the preference space MNL model. I am also setting
# returnData = TRUE so compare the predicted choice with the actual
# choices
choices_mnl_pref <- predict(mnl_pref, type = "choices", returnData = TRUE)
head(choices_mnl_pref)

# You can also make predictions using WTP space models as well as MXL models
# in either space
choices_mnl_wtp <- predict(mnl_wtp, type = "choices", returnData = TRUE)
head(choices_mnl_wtp)

choices_mxl_pref <- predict(mxl_pref, type = "choices", returnData = TRUE)
head(choices_mxl_pref)

choices_mxl_wtp <- predict(mxl_wtp, type = "choices", returnData = TRUE)
head(choices_mxl_wtp)

# Compare prediction accuracy across models
library(dplyr)

# Add actual choices to every choice prediction data frame
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
