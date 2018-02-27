setwd('/Users/jhelvy/Documents/github/logitr/')
source('./R/additionalInfo.R')
source('./R/logit.R')
source('./R/main.R')
source('./R/modelInputs.R')
source('./R/optimLoop.R')
source('./R/other.R')
source('./R/printFunctions.R')
source('./R/simulation.R')

library('nloptr')
library('randtoolbox')

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
betaDist   = c(1,1,0,0,0)
options = list(
    wtpSpace        = F,
    numMultiStarts  = 1,
    useAnalyticGrad = T,
    keepAllRuns     = T,
    scaleInputs     = F,
    printLevel      = 1)



# betaDist = NULL
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
logitr.summary(model)

negLL     = modelInputs$evalFuncs$negLL(model$coef, modelInputs)
negGradLL = modelInputs$evalFuncs$negGradLL(model$coef, modelInputs)
negGradLL
negLL
# hessLL = modelInputs$evalFuncs$hessLL(model$coef, modelInputs)
# negGradLL %*% solve(hessLL) %*% t(negGradLL)
