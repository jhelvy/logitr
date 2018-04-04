# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs = function(data, choiceName, obsIDName, parNames, randPars,
                          priceName, randPrice, modelSpace, options) {
    # Setup pars
    parSetup    = getParSetup(parNames, priceName, randPars, randPrice)
    parNameList = getParNameList(parSetup)
    # Separate data elements
    data   = removeNAs(data, choiceName, obsIDName, parNames, priceName,
             modelSpace)
    X      = as.matrix(data[parNames])
    obsID  = data[,which(names(data)==obsIDName)]
    choice = data[,which(names(data)==choiceName)]
    price  = NA
    if (modelSpace=='wtp') {price = -1*data[,which(names(data)==priceName)]}
    # Create the modelInputs list
    modelInputs = list(
        price=price, X=X, choice=choice, obsID=obsID, priceName=priceName,
        parNameList=parNameList, parSetup=parSetup, scaleFactors=NA,
        modelSpace=modelSpace, modelType='mnl', options=options)
    if (options$scaleInputs) {modelInputs = scaleInputs(modelInputs)}
    if (length(unique(parSetup)) > 1) {
        modelInputs$modelType     = 'mxl'
        modelInputs$standardDraws = options$standardDraws
        if (is.null(options$standardDraws)) {
            modelInputs$standardDraws = getStandardDraws(parSetup,
                options$numDraws, options$drawType)
        }
    }
    modelInputs$logitFuncs = setLogitFunctions(modelInputs)
    modelInputs$evalFuncs  = setEvalFunctions(modelInputs)
    return(modelInputs)
}

removeNAs = function(data, choiceName, obsIDName, parNames, priceName,
                     modelSpace) {
    colsToSelect = c(choiceName, obsIDName, parNames)
    if (modelSpace=='wtp') {colsToSelect = c(colsToSelect, priceName)}
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
    if (modelInputs$modelSpace=='wtp') {
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
    if (abs(max(scaledVar)) > 1) {
        while (abs(max(scaledVar)) > 1) {
            scalingFactor = scalingFactor*10
            scaledVar     = var/scalingFactor
        }
    } else if (abs(max(scaledVar)) < 0.1) {
        while (abs(max(scaledVar)) < 0.1) {
            scalingFactor = scalingFactor/10
            scaledVar     = var/scalingFactor
        }
    }
    return(list(scaledVar=scaledVar, scalingFactor=scalingFactor))
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
    randParIDs        = getRandParIDs(parSetup)
    parNameList       = names(parSetup)
    parNameList.mu    = parNameList
    parNameList.sigma = parNameList[randParIDs]
    if (length(randParIDs) > 0) {
        parNameList.mu[randParIDs] =paste(parNameList[randParIDs],'mu',sep='.')
        parNameList.sigma = paste(parNameList.sigma, 'sigma', sep='.')
    }
    return(list(mu=parNameList.mu, sigma=parNameList.sigma))
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
    if (modelInputs$modelSpace=='wtp') {
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
        # evalFuncs$hessLL    = getMnlHessLL # Numeric approx is faster
    }
    if (modelInputs$modelType=='mxl') {
        evalFuncs$objective = mxlNegLLAndNumericGradLL
        evalFuncs$negLL     = getMxlNegLL
        evalFuncs$negGradLL = getNumericNegGradLL
        evalFuncs$hessLL    = getNumericHessLL
        if (modelInputs$options$useAnalyticGrad) {
            evalFuncs$objective = mxlNegLLAndGradLL
            evalFuncs$negGradLL = getMxlNegGradLL
        }
    }
    return(evalFuncs)
}

getFixedParIDs = function(parSetup) {
    return(which(parSetup == 'f'))
}

getRandParIDs = function(parSetup) {
    return(which(parSetup != 'f'))
}

getNormParIDs = function(parSetup) {
    return(which(parSetup == 'n'))
}

getLogNormParIDs = function(parSetup) {
    return(which(parSetup == 'ln'))
}
