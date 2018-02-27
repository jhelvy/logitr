# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs = function(data, choiceName, obsIDName, betaNames, betaDist,
                         priceName, priceDist, prefSpaceModel, standardDraws,
                         options) {
    modelSpace = 'pref'
    if (options$wtpSpace) {modelSpace = 'wtp'}
    # Setup pars
    parSetup = getParSetup(betaNames, betaDist, priceDist, modelSpace)
    parNames = getParNames(parSetup)
    numBetas = nrow(parSetup)
    # Separate data elements
    data   = removeNAs(data, choiceName, obsIDName, betaNames, priceName,
             modelSpace)
    X      = as.matrix(data[betaNames])
    obsID  = data[,which(names(data)==obsIDName)]
    choice = data[,which(names(data)==choiceName)]
    price  = NA
    if (modelSpace == 'wtp') {price = -1*data[,which(names(data)==priceName)]}
    # Create the modelInputs list
    modelInputs = list(
        price=price, X=X, choice=choice, obsID=obsID, priceName=priceName,
        parNames=parNames, parSetup=parSetup, scaleFactors=NA,
        numBetas=numBetas, modelSpace=modelSpace, modelType='mnl',
        options=options)
    if (options$scaleInputs) {modelInputs = scaleInputs(modelInputs)}
    if (sum(parSetup$dist != 0)) {
        modelInputs$modelType = 'mxl'
        if (is.null(standardDraws)) {
            modelInputs$standardDraws = getStandardDraws(parSetup, options)
        } else {
            modelInputs$standardDraws = standardDraws
        }
    }
    modelInputs$logitFuncs = setLogitFunctions(modelInputs)
    modelInputs$evalFuncs  = setEvalFunctions(modelInputs)
    if (is.null(prefSpaceModel)==F) {
        prefSpaceModel$wtp = getPrefSpaceModelWtp(prefSpaceModel, modelInputs)
        modelInputs$prefSpaceModel = prefSpaceModel

    }
    return(modelInputs)
}

removeNAs = function(data, choiceName, obsIDName, betaNames, priceName,
                     modelSpace) {
    colsToSelect = c(choiceName, obsIDName, betaNames)
    if (modelSpace == 'wtp') {colsToSelect = c(colsToSelect, priceName)}
    return(na.omit(data[colsToSelect]))
}

# Function that scales all the variables in X to be between 0 and 1.
scaleInputs = function(modelInputs) {
    price       = modelInputs$price
    X           = modelInputs$X
    scaledX     = X
    scaledPrice = price
    # Scale X data
    scaleFactorsX = rep(0, ncol(scaledX))
    for (col in 1:ncol(scaledX)) {
        scalingResultsX    = scaleVar(X[,col], scalingFactor=1)
        scaledX[,col]      = scalingResultsX$scaledVar
        scaleFactorsX[col] = scalingResultsX$scalingFactor
    }
    scaleFactors = scaleFactorsX
    names(scaleFactors) = colnames(scaledX)
    # Scale price if WTP space model
    if (modelInputs$modelSpace == 'wtp') {
        scalingResultsPrice = scaleVar(price, scalingFactor=1)
        scaledPrice         = scalingResultsPrice$scaledVar
        scaleFactorPrice    = scalingResultsPrice$scalingFactor
        scaleFactors        = c(scaleFactorPrice, scaleFactorsX)
        names(scaleFactors) = c('lambda', colnames(scaledX))
    }
    modelInputs$X            = scaledX
    modelInputs$price        = scaledPrice
    modelInputs$scaleFactors = scaleFactors
    return(modelInputs)
}

scaleVar = function(var, scalingFactor) {
    scaledVar = var/scalingFactor
    test = abs(mean(scaledVar[which(scaledVar!=0)]))
    if (test > 1) {
        while (test > 1) {
            scalingFactor = scalingFactor*10
            scaledVar = var/scalingFactor
            test = abs(mean(scaledVar[which(scaledVar!=0)]))
        }
    } else if (test < 0.1) {
        while (test < 0.1) {
            scalingFactor = scalingFactor/10
            scaledVar = var/scalingFactor
            test = abs(mean(scaledVar[which(scaledVar!=0)]))
        }
    }
    return(list(scaledVar=scaledVar, scalingFactor=scalingFactor))
}

getParSetup = function(betaNames, betaDist, priceDist, modelSpace) {
    if (is.null(betaDist)) {betaDist = rep(0, length(betaNames))}
    if (is.null(priceDist)) {priceDist = 0}
    parNames = betaNames
    parDist  = betaDist
    if (modelSpace == 'wtp') {
        parNames = c('lambda', parNames)
        parDist  = c(priceDist, betaDist)
    }
    parSetup = data.frame(par=parNames, dist=parDist)
    return(parSetup)
}

getParNames = function(parSetup) {
    # For mxl models, need both '.mu' and '.sigma' parameters
    fixedParIDs = which(parSetup$dist == 0)
    randParIDs  = which(parSetup$dist != 0)
    parNames    = as.character(parSetup$par)
    parNames.mu = parNames
    parNames.sigma = parNames[randParIDs]
    parNames.mu[randParIDs] = paste(parNames[randParIDs], 'mu', sep='.')
    if (length(randParIDs) > 0) {
        parNames.sigma = paste(parNames.sigma, 'sigma', sep='.')
    }
    return(list(mu=parNames.mu, sigma=parNames.sigma))
}

setLogitFunctions = function(modelInputs) {
    logitFuncs = list(
        getMnlLogit  = getMnlLogit,
        mnlNegLL     = mnlNegLL,
        getMnlV      = getMnlV.pref,
        mnlNegGradLL = mnlNegGradLL.pref,
        mnlHessLL    = mnlHessLL.pref,
        getMxlLogit  = getMxlLogit,
        mxlNegLL     = mxlNegLL,
        getMxlV      = getMxlV.pref,
        mxlNegGradLL = mxlNegGradLL.pref)
    if (modelInputs$modelSpace == 'wtp') {
        logitFuncs$getMnlV      = getMnlV.wtp
        logitFuncs$mnlNegGradLL = mnlNegGradLL.wtp
        logitFuncs$mnlHessLL    = mnlHessLL.wtp
        logitFuncs$getMxlV      = getMxlV.wtp
        logitFuncs$mxlNegGradLL = mxlNegGradLL.wtp
    }
    return(logitFuncs)
}

setEvalFunctions = function(modelInputs) {
    evalFuncs = list(
        objective = mnlNegLLAndNumericGradLL,
        negLL     = getMnlNegLL,
        negGradLL = getNumericNegGradLL,
        hessLL    = getNumericHessLL)
    if (modelInputs$options$useAnalyticGrad) {
        evalFuncs$objective = mnlNegLLAndGradLL
        evalFuncs$negGradLL = getMnlNegGradLL
        evalFuncs$hessLL    = getMnlHessLL
    }
    if (modelInputs$modelType == 'mxl') {
        evalFuncs$objective = mxlNegLLAndNumericGradLL
        evalFuncs$negLL     = getMxlNegLL
        evalFuncs$negGradLL = getNumericNegGradLL
        evalFuncs$hessLL    = getNumericHessLL
        # mxlNegGradLL is still not yet correct for WTP space models,
        # so still running numeric approximations for now regardless of
        # settings for MXL WTP space models
        if (modelInputs$options$useAnalyticGrad &
            modelInputs$modelSpace == 'pref') {
            evalFuncs$objective = mxlNegLLAndGradLL
            evalFuncs$negGradLL = getMxlNegGradLL
        }
    }
    return(evalFuncs)
}

getPrefSpaceModelWtp = function(model, modelInputs) {
    priceName           = modelInputs$priceName
    prefCoefs           = model$coef
    prefPriceIndex      = which(grepl(priceName, names(prefCoefs)))[1]
    prefPriceCoef       = prefCoefs[prefPriceIndex]
    wtp                 = prefCoefs / (-1*prefPriceCoef)
    wtp[prefPriceIndex] = -1*prefCoefs[prefPriceIndex]
    names(wtp)          = gsub(priceName, 'lambda', names(wtp))
    return(wtp)
}
