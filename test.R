setwd('/Users/jhelvy/Documents/github/logitr/')
source('./R/functions.R')

# Import the choice data. Example data is the 'Yogurt' data set from the
# mlogit package, reformatted for usage with the logitr package
choiceData = read.csv(file='./yogurt.csv', header=T)

# ============================================================================

model = logitr(
    data       = choiceData,
    choiceName = 'choice',
    obsIDName  = 'obsID',
    betaNames  = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
    options = list(
        wtpSpace        = F,
        numMultiStarts  = 1,
        useAnalyticGrad = T,
        keepAllRuns     = T,
        scaleInputs     = F))



data       = choiceData
choiceName = 'choice'
obsIDName  = 'obsID'
betaNames  = c('price', 'feat', 'dannon', 'hiland', 'yoplait')
options = list(
    wtpSpace        = F,
    numMultiStarts  = 1,
    useAnalyticGrad = T,
    keepAllRuns     = T,
    scaleInputs     = F,
    printLevel      = 1)



betaDist = NULL
priceDist = NULL
priceName = NULL
standardDraws = NULL
prefSpaceModel = NULL



options = runOptionsChecks(options)
# Prepare the modelInputs list
modelInputs = getModelInputs(data, choiceName, obsIDName, betaNames,
    betaDist, priceName, priceDist, prefSpaceModel, standardDraws, options)

startPars = getRandomStartPars(modelInputs)

model  = runModel(modelInputs, startPars)
model = appendModelInfo(model, modelInputs)


my.pars = model$coef
mxl.pars = as.numeric(M.mxl$coefficients)

negLL     = modelInputs$evalFuncs$negLL(my.pars, modelInputs)
negGradLL = modelInputs$evalFuncs$negGradLL(my.pars, modelInputs)
negGradLL
negLL
negLL     = modelInputs$evalFuncs$negLL(mxl.pars, modelInputs)
negGradLL = modelInputs$evalFuncs$negGradLL(mxl.pars, modelInputs)
negGradLL
negLL

# hessLL = modelInputs$evalFuncs$hessLL(pars, modelInputs)
# negGradLL %*% solve(hessLL) %*% t(negGradLL)


pars = pars + 0.1*negGradLL %*% solve(-1*hessLL)

model = appendModelInfo(model, modelInputs)
logitr.summary(model)


# mxlNegGradLL.pref(X, parSetup, obsID, choice, standardDraws,
#     betaDraws, VDraws, logitDraws, pHat)







    logitFuncs = modelInputs$logitFuncs
    obsID      = modelInputs$obsID
    choice     = modelInputs$choice
    betaDraws  = makeBetaDraws(pars, modelInputs)
    VDraws     = logitFuncs$getMxlV(betaDraws, modelInputs)
    logitDraws = logitFuncs$getMxlLogit(VDraws, obsID)
    pHat       = rowMeans(logitDraws)
