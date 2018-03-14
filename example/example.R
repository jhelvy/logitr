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
  parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
  options    = list(
    numMultiStarts = 5,     # Since the log-likelihood is convex for this model
                            # a multistart isn't really necessary
    keepAllRuns    = TRUE)) # By keeping all the runs, you can review the
                            # results of each multistart run

# Print a summary of all multistart runs and a summary of the best model:
summary(mnl.pref)

# Print a summary of the third model run:
summary(mnl.pref$models[[3]])

# Print a summary of the best model:
summary(mnl.pref$bestModel)

# Multistart MNL model in the WTP Space:
mnl.wtp = logitr(
  data       = choiceData,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  modelSpace = 'wtp',
  options = list(
    # You should run a multistart for WTP models since they are non-convex,
    numMultiStarts = 10,
    keepAllRuns    = TRUE,
    prefSpaceModel = mnl.pref, # If keepAllRuns=T for the prefSpaceModel,
                               # the best model from the multistart
                               # will be used for comparison.
    scaleInputs    = TRUE)) # Here I scale the inputs because it helps with
                            # stability in this case.

# Print a summary of all multistart runs and a summary of the best model:
# Note that because the prefSpaceModel argument was included in the options
# the summary will also print a comparison of the WTP between the two spaces.
summary(mnl.wtp)

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
  data       = logitr.data,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  randPars   = c(feat='n'),
  randPrice  = 'n',
  modelSpace = 'wtp',
  options = list(
    numMultiStarts = 1,
    keepAllRuns    = TRUE,
    prefSpaceModel = mxl.pref,
    numDraws       = 200))

# View summary of model:
summary(mxl.wtp)

