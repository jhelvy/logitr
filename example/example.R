# Install logitr package from github
library('devtools')
install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Import the choice data. Example data is the 'Yogurt' data set from the
# mlogit package, reformatted for usage with the logitr package
choiceData = read.csv(
    file   = 'https://raw.github.com/jhelvy/logitr/master/example/yogurt.csv',
    header = TRUE)

# ============================================================================
# Homogeneous MNL models

# Run a MNL model in the Preference Space:
mnl.pref = logitr(
  data       = choiceData,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))

# Print a summary of the results:
summary(mnl.pref)

# Get the coefficients from the model:
coef(mnl.pref)

# Get the WTP implied from the preference space model
mnl.pref.wtp = wtp.logitr(mnl.pref, priceName='price')
mnl.pref.wtp

# Run a MNL model in the WTP Space using a multistart:
mnl.wtp = logitr(
  data       = choiceData,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  modelSpace = 'wtp',
  options = list(
    # You should run a multistart for WTP models since they are non-convex:
    numMultiStarts = 10,
    # You can review the results of each multistart run with keepAllRuns=T:
    keepAllRuns = TRUE,
    # It can be useful to use the WTP from the preference space model as the
    # starting values for the first run:
    startVals = mnl.pref.wtp$Estimate,
    # Because the prefSpaceWtp has values as large as 8, I increase the
    # boundaries of the random starting values:
    startParBounds = c(-5,5)))

# Print a summary of all multistart runs and a summary of the best model:
summary(mnl.wtp)

# Print a summary of only the third model run:
summary(mnl.wtp$models[[3]])

# Print a summary of the best model:
summary(mnl.wtp$bestModel)

# NOTE ON LOCAL MINIMA IN WTP SPACE MODELS:
# Comparing the WTP and log-likelihood values between the equivalent models in
# the preference space and WTP space is a helpful check for whether you have
# reached a global solution in WTP space models, which have non-convex
# log-likelihoods functions. This can be done using the wtpCompare function:

wtpCompare(mnl.pref, mnl.wtp, priceName='price')

# ============================================================================
# Heterogeneous MXL models

# Multistart MXL model in the Preference Space:
mxl.pref = logitr(
  data       = choiceData,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
  randPars   = c(price='n', feat='n'),
  options    = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
      numMultiStarts = 1,
      keepAllRuns    = TRUE,
      numDraws       = 200))

# View summary of model:
summary(mxl.pref)

# Get the WTP implied from the preference space model
mxl.pref.wtp = wtp.logitr(mxl.pref, priceName='price.mu')
mxl.pref.wtp

# Multistart MXL model in the WTP Space:
mxl.wtp = logitr(
  data       = choiceData,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  randPars   = c(feat='n'),
  randPrice  = 'n',
  modelSpace = 'wtp',
  options = list(
  # You should run a multistart for MXL models since they are non-convex,
  # but it can take a long time. Here I just use 1 for brevity:
    numMultiStarts = 1,
    keepAllRuns    = TRUE,
    startVals      = mxl.pref.wtp$Estimate,
    startParBounds = c(-5,5),
    numDraws       = 200))

# View summary of model:
summary(mxl.wtp)

# Compare WTP from each space:
wtpCompare(mxl.pref, mxl.wtp, priceName='price.mu')

# Note that the WTP will not be the same between preference space and WTP
# space MXL models. This is because the distributional assumptions
# in MXL models imply different distributions on WTP depending on the model
# space. See Train and Weeks (2005) and Sonnier, Ainslie, and Otter (2007) for
# details on this topic:

# Train K, Weeks M (2005). “Discrete Choice Models in Preference Space and
# Willingness- to-Pay Space.” In R Scarpa, A Alberini (eds.), Applications of
# Simulation Methods in Environmental and Resource Economics, volume 6 of The
# Economics of Non-Market Goods and Resources, pp. 1-16. Springer-Verlag.

# Sonnier G, Ainslie A, Otter T (2007). “Heterogeneity Distributions of
# Willingness-to-Pay in Choice Models.” Quantitative Marketing and Economics,
# 5(3), 313–331.
