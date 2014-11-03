# Model Run Name: model1
# Author:         John Doe
# Date:           March 10, 2000

# MAIN SETTINGS
# =============
# modelType          = Choose the type of model: 'mnl' or 'mxl', for a
#                      multinomial or mixed logit. Defaults to 'mnl'.
# modelSpace         = Choose the model space: 'pref' or 'wtp', for a
#                      preference or WTP space models. Defaults to 'pref'.
# useWeights         = Choose whether to use weights or not: TRUE or FALSE.
#                      Defaults to FALSE.
# useAnalyticGrad    = Choose whether to use the analytic gradient or not
#                      in the optimization loop: TRUE or FALSE .
#                      Defaults to TRUE.
# scaleParams        = Choose whether to scale the parameters for the
#                      optimization or not: TRUE or FALSE .
#                      Defaults to FALSE.
# numMultiStarts     = Set the number of model runs you want to complete
#                      for a multistart model estimation. By default, the
#                      algorithm uses a different random starting point
#                      between the user provided lower and upper bounds for
#                      each run. If a custom starting point is provided, it
#                      is used only for the first run, followed by random
#                      starting points. Defaults to 1.
# maxMultiStartIters = The maximum iterations to use in each run of the
#                      multistart optimization loop. Defaults to 100.
# maxFinalIters      = The maximum iterations to use in the optimization
#                      loop for the final optimization run, which uses the
#                      results from the best multistart run as it's
#                      starting point. Defaults to 1000.
# numDraws           = Set the number of draws to use for mixed logit models.
#                      If the modelType is set to 'mnl', this is ignored.
#                      Defaults to 125.
# useOptimx          = The default optimizor is optim, but you can use
#                      optimx if desired. Note that optim is usually a lot
#                      faster, though optimx is sometimes more robust. If
#                      running a large multistart, I optim is recommended
#                      for speed, but if only running one iteration optimx
#                      may give better results. Defaults to FALSE.
# optAlgorithm       = The method to use when running the optimization.
#                      Choices include 'BFGS', 'CG', 'L-BFGS-B', 'SANN',
#                      and 'Brent'. See ?optim for more details about each
#                      algorithm. Defaults to 'BFGS'.
#
# An example of the main settings:
modelType          = 'mnl'
modelSpace         = 'pref'
useWeights         = FALSE
useAnalyticGrad    = TRUE
scaleParams        = FALSE
numMultiStarts     = 1
maxMultiStartIters = 100
maxFinalIters      = 1000
numDraws           = 125
useOptimx          = FALSE
optAlgorithm       = 'BFGS'



# DATA & PARAMETER SETUP SETTINGS
# ===============================
# choiceData    = The choice data - must have a header for the first row!
# Each of the following variables must be names of columns in choiceData:
# respondentID  = Identifies each individual choice maker.
# choiceID      = Identifies each individual choice situation.
# choice        = Identifies which alternative was chosen (1 or 0).
# weights       = Identifies the weighting vector.
# priceVar      = Identifies the price attribute. Defaults to NULL.
# priceDist     = The distributional assumption on the price parameters:
#                 0=fixed, 1=normal, 2=log-normal. Defaults to 0.
# fixedVars     = Identifies all the fixed (non-heterogeneous) covariates
#                 other than price.
# normalVars    = Identifies all the normally distributed covariates other
#                 than price.
# logNormalVars = Identifies all the log-normally distributed covariates other
#                 than price.
#
# A note on covariate distributions: If the modelType is 'mnl' (multinomial
# logit), then all parameters are assumed fixed and the model will include all
# covariates listed in fixedVars, normalVars, and logNormalVars. Likewise, if
# the modelType is 'mxl' (mixed logit) and the only covariates listed are in
# fixedVars, then this is the same as running a 'mnl' model.

# An example of the parameter setup settings:
choiceData     = read.csv(file='./choiceData.csv', header=TRUE)
respondentID   = 'id'
choiceID       = 'obsnum'
choice         = 'choice'
weights        = 'weights'
priceVar       = 'price'
priceDist      = 0
fixedVars      = c('brand')
normalVars     = c('fuelEconomy')
logNormalVars  = c('acceleration')



# START POINT SETTINGS
# ====================
# lowerBound        = The lower bound to use when choosing a random starting
#                     point. This setting is used in every model run,
#                     including those in every iteration of a multistart.
#                     Defaults to -1.
# upperBound        = The lower bound to use when choosing a random starting
#                     point. This setting is used in every model run,
#                     including those in every iteration of a multistart.
#                     Defaults to 1.
# customStartPoints = Will be used as the starting points in the first model
#                     run only. All subsequent runs (in a multistart)
#                     will be randomly chosen between provided lower and upper
#                     bounds. Be sure that the length of customStartPoints is
#                     equal to the number of parameters being estimated.
#                     Defaults to NULL.
# customLowerBound  = A vector of lower bounds to use for each parameter
#                     being estimated when choosing a random starting
#                     point. This setting is used in every model run,
#                     including those in every iteration of a multistart.
#                     Be sure that the length of customStartPoints is equal
#                     to the number of parameters being estimated. Defaults
#                     to NULL.
# customUpperBound  = A vector of upper bounds to use for each parameter
#                     being estimated when choosing a random starting
#                     point. This setting is used in every model run,
#                     including those in every iteration of a multistart.
#                     Be sure that the length of customStartPoints is equal
#                     to the number of parameters being estimated. Defaults
#                     to NULL.
#
# An example of the start point settings:
lowerBound        = -1
upperBound        = 1
customStartPoints = NULL
customLowerBound  = NULL
customUpperBound  = NULL



# MODEL RUN SETTINGS
# ==================
# runOptimizor = Set to TRUE or FAlSE based on if you want the optimization
#                loop to begin after this file is run. Setting to FALSE can
#                allow the user an opportunity to review and examine inputs
#                for errors before the optimization loop begins. Defaults
#                to TRUE.
# saveResults  = Set to TRUE or FALSE based on if you want the results of
#                the model run to be saved. Setting to FALSE gives the user
#                the opportunity to avoid accidentally overwriting previous
#                model runs. Defaults to TRUE.
# savePath     = Set the path to where you want the model results to be
#                saved from . Must be a .Rds ending.
#
# An example of the model run settings:
runOptimizor = TRUE
saveResults  = TRUE
savePath     = './modelResults.Rds'



# RUN THE MODEL
# =============
source(file='./code/launch.R') # Runs everything - do not change this line!
