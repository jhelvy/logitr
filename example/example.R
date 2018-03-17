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

# Multistart MNL model in the Preference Space:
mnl.pref = logitr(
  data       = choiceData,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))

# Print a summary of the results:
summary(mnl.pref)

# Multistart MNL model in the WTP Space:
mnl.wtp = logitr(
  data       = choiceData,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  modelSpace = 'wtp',
  options = list(
    # You should run a multistart for WTP models since they are non-convex
    numMultiStarts = 10,
    # You can review the results of each multistart run with keepAllRuns=T
    keepAllRuns    = TRUE,
    # Include the preference space model as an input to 1) use the computed
    # WTP as the starting parameters for the first multistart run, and
    # 2) compare the WTP between the two spaces.
    prefSpaceModel = mnl.pref))

# Print a summary of all multistart runs and a summary of the best model:
summary(mnl.wtp)

# Print a summary of only the third model run:
summary(mnl.wtp$models[[3]])

# Print a summary of the best model:
summary(mnl.wtp$bestModel)

# CAUTION ON LOCAL MINIMA:
# To check whether you have reached a global solution in WTP space models,
# try running the equivalent model in the preference space and compare the
# log-likelihood values at the solution as well as the computed mean WTP
# values with those from the WTP space model. If they do not agree, then the
# WTP space model might have reached a local minimum (the solution for the
# preference space mnl model should be global since the log-likelihood in the
# preference space is convex). Increase numMultiStarts and run the model again
# to search the solution space for the global solution. By including the
# prefSpaceModel argument in a WTP space model, this comparison is
# automatically done for you.

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
  # but it can take a long time.
      numMultiStarts = 1,
      keepAllRuns    = TRUE,
      numDraws       = 200))

# View summary of model:
summary(mxl.pref)

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
  # but it can take a long time.
    numMultiStarts = 1,
    keepAllRuns    = TRUE,
    prefSpaceModel = mxl.pref,
    numDraws       = 200))

# View summary of model:
summary(mxl.wtp)

