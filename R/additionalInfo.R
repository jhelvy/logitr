# ============================================================================
# Functions for computing other information about model after estimation
# ============================================================================

appendModelInfo = function(model, modelInputs) {
    # Compute variables
    coef       = getModelPars(model, modelInputs)
    gradient   = -1*modelInputs$evalFuncs$negGradLL(coef, modelInputs)
    hessian    = getModelHessian(model, modelInputs)
    se         = getModelStandErrs(coef, hessian)
    logLik     = as.numeric(model$logLik)
    nullLogLik = -1*modelInputs$evalFuncs$negLL(coef*0, modelInputs)
    numObs     = sum(modelInputs$choice)
    options    = append(modelInputs$options, model$options)
    result = structure(list(
        coef=coef, standErrs=se, logLik=logLik, nullLogLik=nullLogLik,
        gradient=gradient, hessian=hessian, numObs=numObs,
        numParams=length(coef), startPars=model$startPars,
        multistartNumber=model$multistartNumber, time=model$time,
        iterations=model$iterations, message=model$message,
        status=model$status, modelSpace=modelInputs$modelSpace,
        standardDraws=NA, randParSummary=NA, options=options),
        class='logitr')
    # If MXL model, attached draws and summary of parameter distributions
    if (modelInputs$modelType =='mxl') {
        result$standardDraws  = modelInputs$standardDraws
        result$randParSummary = getRandParSummary(coef, modelInputs)
    }
    return(result)
}

getModelPars= function(model, modelInputs) {
    pars        = model$solution
    muNames     = modelInputs$parNameList$mu
    sigmaNames  = modelInputs$parNameList$sigma
    names(pars) = c(muNames, sigmaNames)
    if (modelInputs$options$scaleInputs) {
        scaleFactors = getModelScaleFactors(model, modelInputs)
        pars = pars / scaleFactors
        if (modelInputs$modelSpace=='wtp') {
            priceScaleFactor = scaleFactors[1]
            pars             = pars*priceScaleFactor
            pars[1]          = pars[1]/priceScaleFactor
        }
    }
    # Make sigmas positive
    pars[sigmaNames] = abs(pars[sigmaNames])
    return(pars)
}

getModelHessian = function(model, modelInputs) {
    hessian = modelInputs$evalFuncs$hessLL(model$solution, modelInputs)
    if (modelInputs$options$scaleInputs) {
        scaleFactors = getModelScaleFactors(model, modelInputs)
        sf           = matrix(scaleFactors, ncol=1)
        sfMat        = sf %*% t(sf)
        hessian      = hessian*sfMat
        if (modelInputs$modelSpace=='wtp') {
            priceScaleFactor = scaleFactors[1]
            hessian          = hessian/priceScaleFactor^2
            hessian[1,]      = hessian[1,]*priceScaleFactor
            hessian[,1]      = hessian[,1]*priceScaleFactor
        }
    }
    parNames = c(modelInputs$parNameList$mu, modelInputs$parNameList$sigma)
    colnames(hessian)  = parNames
    row.names(hessian) = parNames
    return(hessian)
}

getModelScaleFactors = function(model, modelInputs) {
    if (modelInputs$modelType=='mnl') {
        return(modelInputs$scaleFactors)
    } else {
        parNames = c(modelInputs$parNameList$mu, modelInputs$parNameList$sigma)
        scaleFactors = modelInputs$scaleFactors
        mxlScaleFactors = rep(0, length(parNames))
        for (i in 1:length(scaleFactors)) {
            scaleFactor = scaleFactors[i]
            factorIDs   = which(grepl(names(scaleFactor), parNames))
            mxlScaleFactors[factorIDs] = scaleFactor
        }
        return(mxlScaleFactors)
    }
}

getModelStandErrs = function(coef, hessian) {
    se = rep(NA, length(coef))
    tryCatch({
        se = diag(sqrt(abs(solve(hessian))))
    }, error=function(e){})
    names(se) = names(coef)
    return(se)
}

getRandParSummary = function(coef, modelInputs) {
    temp       = modelInputs
    parSetup   = modelInputs$parSetup
    randParIDs = which(parSetup$dist != 0)
    normIDs    = which(parSetup$dist==1)
    logNormIDs = which(parSetup$dist==2)
    temp$options$numDraws = 10^4
    temp$standardDraws    = getStandardDraws(parSetup, temp$options)
    betaDraws             = makeBetaDraws(coef, temp)
    randParSummary        = apply(betaDraws, 2, summary)
    # Add names to summary
    distName             = rep('', nrow(parSetup))
    distName[normIDs]    = 'normal'
    distName[logNormIDs] = 'log-normal'
    parSetup$distName    = distName
    summaryNames = paste(parSetup$par, ' (', parSetup$distName, ')', sep='')
    colnames(randParSummary) = summaryNames
    randParSummary           = t(randParSummary[,randParIDs])
    return(as.data.frame(randParSummary))
}
