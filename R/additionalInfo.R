# ============================================================================
# Functions for computing other information about model after estimation
# ============================================================================

appendModelInfo = function(model, modelInputs) {
    # Compute variables
    coef         = getModelPars(model, modelInputs)
    gradient     = -1*modelInputs$evalFuncs$negGradLL(coef, modelInputs)
    hessian      = getModelHessian(model, modelInputs)
    se           = getModelStandErrs(coef, hessian)
    numObs       = sum(modelInputs$choice)
    numParams    = length(coef)
    logLik       = as.numeric(model$logLik)
    nullLogLik   = -1*modelInputs$evalFuncs$negLL(coef*0, modelInputs)
    options      = append(modelInputs$options, model$options)
    summaryTable = getSummaryTable(coef, se, numObs, numParams, logLik,
                   nullLogLik)
    result = list(
        summaryTable=summaryTable, coef=coef, standErrs=se, logLik=logLik,
        nullLogLik=nullLogLik, gradient=gradient, hessian=hessian,
        startPars=model$startPars, iterations=model$iterations,
        message=model$message, status=model$status,
        multistartNumber=model$multistartNumber,
        modelSpace=modelInputs$modelSpace, standardDraws=NA,
        randParSummary=NA, wtpComparison=NA, options=options)
    # If MXL model, attached draws and summary of parameter distributions
    if (modelInputs$modelType =='mxl') {
        result$standardDraws  = modelInputs$standardDraws
        result$randParSummary = getRandParSummary(coef, modelInputs)
    }
    # If prefSpaceModel exists, attach comparison of WTP between the two spaces
    prefSpaceModel = modelInputs$prefSpaceModel
    if (is.null(prefSpaceModel)==F & modelInputs$modelSpace=='wtp') {
        result$wtpComparison = getWtpComparison(coef, logLik, prefSpaceModel)
    }
    return(result)
}

getModelPars= function(model, modelInputs) {
    pars = model$solution
    names(pars) = c(modelInputs$parNameList$mu, modelInputs$parNameList$sigma)
    if (modelInputs$options$scaleInputs) {
        scaleFactors = getModelScaleFactors(model, modelInputs)
        pars = pars / scaleFactors
        if (modelInputs$modelSpace == 'wtp') {
            priceScaleFactor = scaleFactors[1]
            pars             = pars*priceScaleFactor
            pars[1]          = pars[1]/priceScaleFactor
        }
    }
    return(pars)
}

getModelHessian = function(model, modelInputs) {
    hessian = modelInputs$evalFuncs$hessLL(model$solution, modelInputs)
    if (modelInputs$options$scaleInputs) {
        scaleFactors = getModelScaleFactors(model, modelInputs)
        sf           = matrix(scaleFactors, ncol=1)
        sfMat        = sf %*% t(sf)
        hessian      = hessian*sfMat
        if (modelInputs$modelSpace == 'wtp') {
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
    if (modelInputs$modelType == 'mnl') {
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

getSummaryTable = function(coef, se, numObs, numParams, logLik,
                           nullLogLik) {
    coefMat      = getCoefMat(coef, se, numObs, numParams)
    statMat      = getStatMat(numObs, numParams, logLik, nullLogLik)
    summaryTable = rbind(coefMat, statMat)
    colnames(summaryTable)[1:2] = c('Estimate', 'StdError')
    return(summaryTable)
}

getCoefMat = function(coef, se, numObs, numParams) {
    tStat  = rep(NA, length(coef))
    pVal   = rep(NA, length(coef))
    signif = rep('', length(coef))
    if (sum(is.na(se)) == 0) {
        tStat  = as.numeric(coef / se)
        dof    = numObs - numParams
        pVal   = 2*(1-pt(abs(tStat), dof))
        signif = rep('', length(pVal))
        signif[which(pVal <= 0.001)] <- '***'
        signif[which(pVal >  0.001 & pVal <= 0.01)] <- '**'
        signif[which(pVal >  0.01  & pVal <= 0.05)] <- '*'
        signif[which(pVal >  0.05  & pVal <= 0.1)] <- '.'
    }
    coefMat = data.frame(
        coef=round(coef, 6), se=round(se, 6), tStat=round(tStat, 4),
        pVal=round(pVal, 4), signif=signif)
    row.names(coefMat) = names(coef)
    return(coefMat)
}

getStatMat = function(numObs, numParams, logLik, nullLogLik) {
    aic           = round(2*numParams - 2*logLik, 4)
    bic           = round(log(numObs)*numParams - 2*logLik, 4)
    mcFaddenR2    = 1 - (logLik / nullLogLik)
    adjMcFaddenR2 = 1 - ((logLik - numParams) / nullLogLik)
    results       = c(logLik, nullLogLik, aic, bic, mcFaddenR2, adjMcFaddenR2,
                    numObs)
    statMat = data.frame(
        coef   = results,
        se     = rep('', length(results)),
        tStat  = rep('', length(results)),
        pVal   = rep('', length(results)),
        signif = rep('', length(results)))
    row.names(statMat) = c('Log-Likelihood at Convergence:',
        'Null Log-Likelihood:', 'AIC:', 'BIC:', 'McFadden R2:',
        'Adj. McFadden R2', 'Number of Observations:')
    return(statMat)
}

getRandParSummary = function(coef, modelInputs) {
    temp       = modelInputs
    parSetup   = modelInputs$parSetup
    randParIDs = which(parSetup$dist != 0)
    normIDs    = which(parSetup$dist == 1)
    logNormIDs = which(parSetup$dist == 2)
    temp$options$numDraws = 10^4
    temp$standardDraws    = getStandardDraws(parSetup, temp$options)
    betaDraws             = makeBetaDraws(coef, temp)
    randParSummary        = apply(betaDraws, 2, summary)
    colnames(randParSummary)      = as.character(parSetup$par)
    randParSummary                = t(randParSummary[,randParIDs])
    randParSummary[normIDs, 1]    = -Inf
    randParSummary[normIDs, 6]    = Inf
    randParSummary[logNormIDs, 6] = Inf
    return(as.data.frame(randParSummary))
}

getWtpComparison = function(coef, logLik, prefSpaceModel) {
    wtpComparison = data.frame(
        prefSpace.wtp = prefSpaceModel$wtp,
        wtpSpace.wtp  = coef)
    # Add logLik values
    logLikCompare     = c(prefSpaceModel$logLik, logLik)
    wtpComparison = rbind(wtpComparison, logLikCompare)
    row.names(wtpComparison)[nrow(wtpComparison)] = 'logLik'
    wtpComparison$difference = round(wtpComparison$wtpSpace.wtp -
        wtpComparison$prefSpace.wtp, 8)
    return(wtpComparison)
}
