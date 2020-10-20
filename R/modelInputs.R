# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs = function(data, choiceName, obsIDName, parNames, randPars,
                          priceName, randPrice, modelSpace, weightsName,
                          options) {
    # Setup weights
    weights = matrix(1, nrow(X))
    weightsUsed = FALSE
    if (! is.null(weightsName)) {
        weights = as.matrix(data[weightsName])
        weightsUsed = TRUE
    }
    # Setup pars
    parSetup    = getParSetup(parNames, priceName, randPars, randPrice)
    parNameList = getParNameList(parSetup)
    options     = runOptionsChecks(options, parNameList)
    # Separate data elements
    data   = removeNAs(data, choiceName, obsIDName, parNames, priceName,
             modelSpace)
    X      = as.matrix(data[parNames])
    obsID  = data[, which(names(data) == obsIDName)]
    choice = data[, which(names(data) == choiceName)]
    price  = NA
    if (modelSpace == 'wtp') {
        price = -1*data[, which(names(data) == priceName)]
    }
    # Create the modelInputs list
    modelInputs = list(
        price = price, X = X, choice = choice, obsID = obsID,
        weights = weights, priceName = priceName, parNameList = parNameList,
        parSetup = parSetup, scaleFactors = NA, modelSpace = modelSpace,
        modelType = 'mnl', weightsUsed = weightsUsed, options = options)
    if (options$scaleInputs) {
        modelInputs = scaleInputs(modelInputs)
    }
    if (isMxlModel(parSetup)) {
        modelInputs$modelType     = 'mxl'
        modelInputs$standardDraws = options$standardDraws
        if (is.null(options$standardDraws)) {
            modelInputs$standardDraws = getStandardDraws(parSetup,
                options$numDraws, options$drawType)
        }
    }
    modelInputs$logitFuncs = setLogitFunctions(modelSpace)
    modelInputs$evalFuncs  = setEvalFunctions(
        modelInputs$modelType, options$useAnalyticGrad)
    return(modelInputs)
}

getParSetup = function(parNames, priceName, randPars, randPrice) {
    parSetup = rep('f', length(parNames))
    for (i in 1:length(parNames)) {
        name = parNames[i]
        if (name %in% names(randPars)) {
            parSetup[i] = randPars[name]
        }
    }
    names(parSetup) = parNames
    if (is.null(priceName)==F) {
        if (is.null(randPrice)) {randPrice = 'f'}
        parSetup = c(randPrice, parSetup)
        names(parSetup)[1] = 'lambda'
    }
    return(parSetup)
}

getParNameList = function(parSetup) {
    # For mxl models, need both '.mu' and '.sigma' parameters
    randParIDs  = getRandParIDs(parSetup)
    names       = names(parSetup)
    names.mu    = names
    names.sigma = names[randParIDs]
    if (length(randParIDs) > 0) {
        names.mu[randParIDs] = paste(names[randParIDs],'mu',sep='.')
        names.sigma          = paste(names.sigma, 'sigma', sep='.')
    }
    names.all = c(names.mu, names.sigma)
    return(list(mu=names.mu, sigma=names.sigma, all=names.all))
}

runOptionsChecks = function(options, parNameList) {
    # Run checks for all inputs
    if (is.null(options$numMultiStarts))  {options$numMultiStarts  = 1}
    if (options$numMultiStarts < 1)       {options$numMultiStarts  = 1}
    if (is.null(options$keepAllRuns))     {options$keepAllRuns     = F}
    if (is.null(options$useAnalyticGrad)) {options$useAnalyticGrad = T}
    if (is.null(options$scaleInputs))     {options$scaleInputs     = T}
    if (is.null(options$startParBounds))  {options$startParBounds  = c(-1, 1)}
    if (is.null(options$standardDraws))   {options$standardDraws   = NULL}
    if (is.null(options$numDraws))        {options$numDraws        = 200}
    if (is.null(options$drawType))        {options$drawType        = 'halton'}
    if (is.null(options$printLevel))      {options$printLevel      = 0}
    if (is.null(options$xtol_rel))        {options$xtol_rel        = 1.0e-8}
    if (is.null(options$xtol_abs))        {options$xtol_abs        = 1.0e-8}
    if (is.null(options$ftol_rel))        {options$ftol_rel        = 1.0e-8}
    if (is.null(options$ftol_abs))        {options$ftol_abs        = 1.0e-8}
    if (is.null(options$maxeval))         {options$maxeval         = 1000}
    if (is.null(options$startVals)) {
        options$startVals = NULL
    } else {
        names(options$startVals) = parNameList$all
    }
    return(options)
}

removeNAs = function(data, choiceName, obsIDName, parNames, priceName,
                     modelSpace) {
    colsToSelect = c(choiceName, obsIDName, parNames)
    if (modelSpace == 'wtp') {
        colsToSelect = c(colsToSelect, priceName)
    }
    return(na.omit(data[colsToSelect]))
}

# Function that scales all the variables in X to be between 0 and 1:
scaleInputs = function(modelInputs) {
    price       = modelInputs$price
    X           = modelInputs$X
    scaledX     = X
    scaledPrice = price
    # Scale X data
    scaleFactorsX = rep(0, ncol(scaledX))
    for (col in 1:ncol(scaledX)) {
        var                = X[,col]
        vals               = unique(var)
        scalingFactor      = abs(max(vals) - min(vals))
        scaledX[,col]      = var / scalingFactor
        scaleFactorsX[col] = scalingFactor
    }
    scaleFactors = scaleFactorsX
    names(scaleFactors) = colnames(scaledX)
    # Scale price if WTP space model
    if (modelInputs$modelSpace=='wtp') {
        vals                = unique(price)
        scaleFactorPrice    = abs(max(vals) - min(vals))
        scaledPrice         = price / scaleFactorPrice
        scaleFactors        = c(scaleFactorPrice, scaleFactorsX)
        names(scaleFactors) = c('lambda', colnames(scaledX))
    }
    modelInputs$X            = scaledX
    modelInputs$price        = scaledPrice
    modelInputs$scaleFactors = scaleFactors
    return(modelInputs)
}

setLogitFunctions = function(modelSpace) {
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
    if (modelSpace=='wtp') {
        logitFuncs$getMnlV      = getMnlV.wtp
        logitFuncs$mnlNegGradLL = mnlNegGradLL.wtp
        logitFuncs$mnlHessLL    = mnlHessLL.wtp
        logitFuncs$getMxlV      = getMxlV.wtp
        logitFuncs$mxlNegGradLL = mxlNegGradLL.wtp
    }
    return(logitFuncs)
}

setEvalFunctions = function(modelType, useAnalyticGrad) {
    evalFuncs = list(
        objective = mnlNegLLAndNumericGradLL,
        negLL     = getMnlNegLL,
        negGradLL = getNumericNegGradLL,
        hessLL    = getNumericHessLL)
    if (useAnalyticGrad) {
        evalFuncs$objective = mnlNegLLAndGradLL
        evalFuncs$negGradLL = getMnlNegGradLL
        # evalFuncs$hessLL    = getMnlHessLL # Numeric approx is faster
    }
    if (modelType=='mxl') {
        evalFuncs$objective = mxlNegLLAndNumericGradLL
        evalFuncs$negLL     = getMxlNegLL
        evalFuncs$negGradLL = getNumericNegGradLL
        evalFuncs$hessLL    = getNumericHessLL
        if (useAnalyticGrad) {
            evalFuncs$objective = mxlNegLLAndGradLL
            evalFuncs$negGradLL = getMxlNegGradLL
        }
    }
    return(evalFuncs)
}

