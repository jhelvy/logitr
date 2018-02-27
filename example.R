# Install from github
# library('devtools')
# install_github('jhelvy/logitr')
library('logitr')

# Import the choice data. Example data is the 'Yogurt' data set from the
# mlogit package, reformatted for usage with the logitr package
choiceData = read.csv(
    file   = 'https://raw.github.com/jhelvy/logitr/master/yogurt.csv',
    header = TRUE)

# ============================================================================
# Homogeneous MNL models

# Multistart MNL model in the Preference Space:
mnl.pref = logitr(
    data       = choiceData,
    choiceName = 'choice',
    obsIDName  = 'obsID',
    betaNames  = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
    options = list(
        numMultiStarts = 5,
        keepAllRuns    = TRUE))

# Multistart MNL model in the WTP Space:
mnl.wtp = logitr(
    data       = choiceData,
    choiceName = 'choice',
    obsIDName  = 'obsID',
    betaNames  = c('feat', 'dannon', 'hiland', 'yoplait'),
    priceName  = 'price',
    prefSpaceModel = mnl.pref$bestModel,
    options = list(
        wtpSpace       = TRUE,
        numMultiStarts = 5,
        keepAllRuns    = TRUE))

# View model summaries
logitr.summary(mnl.pref)
logitr.summary(mnl.wtp)

# NOTE:
# To be check whether you have reached a global solution in WTP space models,
# try running the equivalent model in the preference space and compare the
# log-likelihood values at the solution as well as the computed mean WTP
# values with those from the WTP space model. If they do not agree, then the
# WTP space model might have reached a local minimum (the solution for the
# preference space mnl model should be global since the log-likelihood in the
# preference space is convex). Increase numMultiStarts and run the model again
# to search the solution space for the global solution.

# ============================================================================
# Heterogeneous MXL models

# Multistart MXL model in the Preference Space:
mxl.pref = logitr(
    data       = choiceData,
    choiceName = 'choice',
    obsIDName  = 'obsID',
    betaNames  = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
    betaDist   = c(1, 1, 1, 1, 1),
    options    = list(
        numMultiStarts = 1,
        keepAllRuns    = TRUE,
        numDraws       = 200))

# Multistart MXL model in the WTP Space:
mxl.wtp = logitr(
    data       = logitr.data,
    choiceName = 'choice',
    obsIDName  = 'obsID',
    betaNames  = c('feat', 'dannon', 'hiland', 'yoplait'),
    priceName  = 'price',
    betaDist   = c(1, 1, 1, 1),
    priceDist  = 1,
    prefSpaceModel = mxl.pref$bestModel,
    options = list(
        wtpSpace        = TRUE,
        numMultiStarts  = 1,
        keepAllRuns     = TRUE,
        numDraws        = 200))

# Compare model summaries
logitr.summary(mxl.pref)
logitr.summary(mxl.wtp)

