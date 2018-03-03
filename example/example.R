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
        numMultiStarts = 5,
        keepAllRuns    = TRUE)) # By keeping all the runs, you can review the
                                # results of each multistart run

# Print a summary of all multistart runs and a summary of the best model:
logitr.summary(mnl.pref)
# Print a summary of the third model run:
logitr.summary(mnl.pref$models[[3]])
# Print a summary of the best model:
logitr.summary(mnl.pref$bestModel)

# Multistart MNL model in the WTP Space:
mnl.wtp = logitr(
    data       = choiceData,
    choiceName = 'choice',
    obsIDName  = 'obsID',
    parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
    priceName  = 'price',
    prefSpaceModel = mnl.pref, # By default the best model from the mnl.pref
                               # multistart will be used for comparison of WTP
    options = list(
        wtpSpace       = TRUE,
        numMultiStarts = 10,
        keepAllRuns    = TRUE,
        scaleInputs    = TRUE)) # Here I scale the inputs because everything
                                # will be divided by price for WTP, and price
                                # is in currently between 0 < 20. Scaling
                                # helps with stability in this case.

# Print a summary of all multistart runs and a summary of the best model:
# Note that because the prefSpaceModel argument was included, the summary
# also prints a comparison of the WTP between the two spaces.
logitr.summary(mnl.wtp)

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
    parDist    = c(1, 1, 1, 1, 1),
    options    = list(
    # You should run a multistart for MXL models since they are non-convex,
    # but it can take a long time.
        numMultiStarts = 1,
        keepAllRuns    = TRUE,
        numDraws       = 200))

# Multistart MXL model in the WTP Space:
mxl.wtp = logitr(
    data       = logitr.data,
    choiceName = 'choice',
    obsIDName  = 'obsID',
    parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
    priceName  = 'price',
    parDist    = c(1, 1, 1, 1),
    priceDist  = 1,
    prefSpaceModel = mxl.pref,
    options = list(
        wtpSpace        = TRUE,
        numMultiStarts  = 1,
        keepAllRuns     = TRUE,
        numDraws        = 200))

# Compare model summaries
logitr.summary(mxl.pref)
logitr.summary(mxl.wtp)

