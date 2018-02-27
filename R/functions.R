# ============================================================================
# Main functions
# ============================================================================

#' The main function for running the logitr program
#'
#' logitr estimates multinomial (MNL) and mixed logit (MXL) models in R with
#' options to estimate in the preference space or willingness-to-pay (WTP)
#' spaces. The program includes an option to run a multistart optimization
#' loop with random starting points to search for a global solution, which
#' should be done for WTP space models as well as mixed logit models since
#' they have nonlinear-in-parameters utility functions that may result in
#' multiple local maxima. The main optimization loop uses the nloptr function
#' to minimize the negative log-likelihood function.
#' @keywords logitr, mnl, mxl
#' @export
#' @examples
#' # Put example code here
logitr = function(data, choiceName, obsIDName, betaNames, priceName=NULL,
                  betaDist=NULL, priceDist=NULL, prefSpaceModel=NULL,
                  standardDraws=NULL, options=list()) {
    require(randtoolbox) # Required for taking Halton draws
    require(nloptr)      # Required for optimization
    options = runOptionsChecks(options)
    # Prepare the modelInputs list
    modelInputs = getModelInputs(data, choiceName, obsIDName, betaNames,
        betaDist, priceName, priceDist, prefSpaceModel, standardDraws, options)
    # Run the models
    allModels = runMultistart(modelInputs)
    if (options$keepAllRuns) {
        models = appendAllModelsInfo(allModels, modelInputs)
        logitr.summary(models)
        return(models)
    } else {
        model = getBestModel(allModels, modelInputs)
        printModelSummary(model)
        return(model)
    }
}

runOptionsChecks = function(options) {
    # Run checks for all inputs
    if(is.null(options$wtpSpace))        {options$wtpSpace        = F}
    if(is.null(options$numMultiStarts))  {options$numMultiStarts  = 1}
    if(options$numMultiStarts < 1)       {options$numMultiStarts  = 1}
    if(is.null(options$keepAllRuns))     {options$keepAllRuns     = F}
    if(is.null(options$useAnalyticGrad)) {options$useAnalyticGrad = T}
    if(is.null(options$scaleInputs))     {options$scaleInputs     = F}
    if(is.null(options$numDraws))        {options$numDraws        = 200}
    if(is.null(options$drawType))        {options$drawType        = 'halton'}
    if(is.null(options$printLevel))      {options$printLevel      = 0}
    if(is.null(options$xtol_rel))        {options$xtol_rel        = 1.0e-9}
    if(is.null(options$xtol_abs))        {options$xtol_abs        = 1.0e-9}
    if(is.null(options$ftol_rel))        {options$ftol_rel        = 1.0e-9}
    if(is.null(options$ftol_abs))        {options$ftol_abs        = 1.0e-9}
    if(is.null(options$maxeval))         {options$maxeval         = 1000}
    return(options)
}

getBestModel = function(allModels, modelInputs) {
    logLikVals     = getMulitstartSummary(allModels)$logLik
    bestModelIndex = which(logLikVals==max(logLikVals))[1]
    bestModel      = allModels[[bestModelIndex]]
    bestModel      = appendModelInfo(bestModel, modelInputs)
    return(bestModel)
}

appendAllModelsInfo = function(allModels, modelInputs) {
    result = list()
    result$models = list()
    for (i in 1:length(allModels)) {
        result$models[[i]] = appendModelInfo(allModels[[i]], modelInputs)
    }
    result$multistartSummary = getMulitstartSummary(allModels)
    result$bestModel         = getBestModel(allModels, modelInputs)
    return(result)
}

getMulitstartSummary = function(allModels) {
    summary = as.data.frame(matrix(0, ncol=4, nrow=length(allModels)))
    colnames(summary) = c('run', 'logLik', 'iterations', 'status')
    for (i in 1:length(allModels)) {
        summary[i, 1] = i
        summary[i, 2] = round(allModels[[i]]$logLik, 5)
        summary[i, 3] = allModels[[i]]$iterations
        summary[i, 4] = allModels[[i]]$status
    }
    return(summary)
}











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









# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart = function(modelInputs) {
    # Setup lists for storing results
    numMultiStarts = modelInputs$options$numMultiStarts
    models = list()
    for (i in 1:numMultiStarts) {
        cat('Running Multistart', i, 'of', numMultiStarts, '\n', sep=' ')
        logLik        = NA
        noFirstRunErr = TRUE
        # Keep trying until a solution is reached for each multistart iteration
        while (is.na(logLik)) {
        tryCatch({
            startPars = getRandomStartPars(modelInputs)
            if (is.null(modelInputs$prefSpaceModel)==F & i==1 &
                noFirstRunErr & modelInputs$modelSpace=='wtp') {
                cat('**Using Preference Space Model WTP Results as Starting ',
                    'Point For This Run**', '\n', sep='')
                startPars = modelInputs$prefSpaceModel$wtp
            }
            model  = runModel(modelInputs, startPars)
            logLik = model$logLik
            model$multistartNumber = i
                   # -1 to get the positive rather than negative LL
        }, error=function(e){
            cat('ERROR: failed to converge...restarting search', '\n',
                sep='')})
            if (i==1 & is.na(logLik)) {noFirstRunErr = FALSE}
        }
        models[[i]] = model
    }
    # printLine()
    return(models)
}

# Returns randomly drawn starting parameters from a uniform distribution
# between -1 and 1
getRandomStartPars = function(modelInputs) {
    parNames = modelInputs$parNames
    # For mxl models, need both '.mu' and '.sigma' parameters
    pars.mu    = runif(length(parNames$mu), -10, 10)
    pars.sigma = runif(length(parNames$sigma), -10, 10)
    # if (modelInputs$options$wtpSpace) {
    #     # This is a WTP space model, so lambda must be positive
    #     pars.mu[1] = runif(1, 0, 1)
    # }
    startPars        = c(pars.mu, pars.sigma)
    names(startPars) = c(parNames$mu, parNames$sigma)
    return(startPars)
}

# Runs the MNL model
runModel = function(modelInputs, startPars) {
    model = nloptr(
        x0          = startPars,
        eval_f      = modelInputs$evalFuncs$objective,
        modelInputs = modelInputs,
        opts        = list(
            'algorithm' = 'NLOPT_LD_LBFGS',
            'xtol_rel'  = modelInputs$options$xtol_rel,
            'xtol_abs'  = modelInputs$options$xtol_abs,
            'ftol_rel'  = modelInputs$options$ftol_rel,
            'ftol_abs'  = modelInputs$options$ftol_abs,
            print_level = modelInputs$options$printLevel,
            maxeval     = modelInputs$options$maxeval))
    model$startPars = startPars
    model$logLik    = -1*model$objective
    return(model)
}










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
        multistartNumber=model$multistartNumber, standardDraws=NA,
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
    names(pars) = c(modelInputs$parNames$mu, modelInputs$parNames$sigma)
    if (modelInputs$options$scaleInputs) {
        scaleFactors = getModelScaleFactors(model, modelInputs)
        pars = pars / scaleFactors
        if (modelInputs$options$wtpSpace) {
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
        if (modelInputs$options$wtpSpace) {
            priceScaleFactor = scaleFactors[1]
            hessian          = hessian/priceScaleFactor^2
            hessian[1,]      = hessian[1,]*priceScaleFactor
            hessian[,1]      = hessian[,1]*priceScaleFactor
        }
    }
    parNames = c(modelInputs$parNames$mu, modelInputs$parNames$sigma)
    colnames(hessian)  = parNames
    row.names(hessian) = parNames
    return(hessian)
}

getModelScaleFactors = function(model, modelInputs) {
    if (modelInputs$modelType == 'mnl') {
        return(modelInputs$scaleFactors)
    } else {
        parNames = c(modelInputs$parNames$mu, modelInputs$parNames$sigma)
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








# ============================================================================
# Functions for printing results and summaries
# ============================================================================

#' Prints a summary of an estimated model using the logitr package.
#'
#' Prints a summary of an estimated model using the logitr package.
#' @keywords logitr, nloptr, status codes
#' @export
#' @examples
#' # View a summary of an estimate model:
#' logitr.summary(model)
logitr.summary = function(model) {
    if ('multistartSummary' %in% names(model)) {
        # This is a list of all models from a multistart, so print the
        # multistart summary first and then print the summary of the best model
        printLine()
        cat('SUMMARY OF ALL MULTISTART RUNS:', '\n', '\n', sep='')
        print(model$multistartSummary)
        cat('---', '\n', sep='')
        cat('To view meaning of status codes, use logitr.statusCodes()', '\n')
        printModelSummary(model$bestModel)
    } else {
        printModelSummary(model)
    }
}

printLine = function() {
    cat('=================================================', '\n', sep='')
}

#' Prints the status codes from the nloptr optimization routine.
#'
#' Prints the status codes from the nloptr optimization routine.
#' @keywords logitr, nloptr, status codes
#' @export
#' @examples
#' # View the status codes:
#' logitr.statusCodes()
logitr.statusCodes = function() {
    cat('Status codes:', '\n', sep='')
    cat('1:  Generic success return value.', '\n', sep='')
    cat('2:  Optimization stopped because stopval was reached.', '\n', sep='')
    cat('3:  Optimization stopped because ftol_rel or ftol_abs was reached.',
        '\n', sep='')
    cat('4:  Optimization stopped because xtol_rel or xtol_abs was reached.',
        '\n', sep='')
    cat('5:  Optimization stopped because maxeval was reached.', '\n', sep='')
    cat('6:  Optimization stopped because maxtime was reached.', '\n', sep='')
    cat('-1: Generic failure code.', '\n', sep='')
    cat('-2: Invalid arguments (e.g. lower bounds are bigger than upper ',
        'bounds, an unknown algorithm was specified, etc.).', '\n', sep='')
    cat('-3: Ran out of memory.', '\n', sep='')
    cat('-4: Halted because roundoff errors limited progress. (In this case, ',
        'the optimization still typically returns a useful result.)', '\n',
        sep='')
    cat("-5: Halted because of a forced termination: the user called ",
        "nlopt_force_stop(opt) on the optimization's nlopt_opt object opt ",
        "from the user's objective function or constraints.", '\n', sep='')
}

printModelSummary = function(model) {
    # Get the basic summary information of the best model
    modelSpace = 'Preference'
    if (model$options$wtpSpace) {modelSpace = 'Willingness-to-Pay'}
    bestModelSummary = data.frame(c(modelSpace,
        model$multistartNumber, round(model$logLik, 3), model$iterations))
    colnames(bestModelSummary) = ''
    row.names(bestModelSummary) = c('Model Space:', 'Best Model Run:',
        'Log-Likelihood:', 'Number of Iterations:')
    # Get the coef and stats tables
    summaryTable = model$summaryTable
    llRowID = which(row.names(summaryTable)=='Log-Likelihood at Convergence:')
    coefIDs = (1:(llRowID-1))
    statIDs = (llRowID:nrow(summaryTable))
    coefTable = summaryTable[coefIDs,]
    statTable = data.frame(round(summaryTable[statIDs,1], 4))
    colnames(statTable) = ''
    row.names(statTable) = row.names(summaryTable)[statIDs]
    # Print the summary
    printLine()
    cat('SUMMARY OF BEST MODEL:', '\n')
    print(bestModelSummary)
    cat('\n')
    cat('Model Coefficients:', '\n')
    print(coefTable)
    cat('---', '\n', sep='')
    cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", '\n',
        sep='')
    cat('\n')
    cat('Model Fit Values:', '\n')
    print(statTable)
    if (sum(grepl('.sigma', names(model$coef))) > 0) {
        cat('\n')
        cat('Random Coefficients:', '\n')
        print(model$randParSummary)
    }
    if (model$options$wtpSpace) {
        cat('\n')
        cat('Comparison of WTP Between Preference and WTP Space Models:', '\n')
        print(format(model$wtpComparison, scientific=F))
    }
}









# ============================================================================
# Logit and log-likelihood functions
# The log-likelihood function is given as the negative log-likelihood
# because the optimization performs a minimization
# ============================================================================

# ============================================================================
# MNL logit and log-likelihood functions
# ============================================================================

# # Logit fraction using data.table package (faster)
# # Returns the logit fraction for mnl (homogeneous) models
# getMnlLogit = function(V, obsID) {
#     data = data.table(V=V, obsID=obsID)
#     data[, expV:=exp(V.V1)]
#     data[, sumExpV:=sum(expV), by=obsID]
#     data[, logit:=expV/sumExpV]
#     return(data$logit)
# }

# Returns the logit fraction for mnl (homogeneous) models
getMnlLogit = function(V, obsID) {
    expV       = exp(V)
    sumExpV    = rowsum(expV, group=obsID)
    repTimes   = as.numeric(table(obsID))
    sumExpVMat = matrix(rep(sumExpV, times=repTimes), ncol=1)
    logit      = expV / sumExpVMat
    return(logit)
}

mnlNegLL = function(choice, logit) {
    negLL = -1*sum(choice*log(logit))
    return(negLL)
}

# Returns a list containing the negative log-likelihood and it's gradient
# Primary objective function for the nloptr optimizer.
mnlNegLLAndGradLL = function(pars, modelInputs) {
    logitFuncs = modelInputs$logitFuncs
    p          = modelInputs$price
    X          = modelInputs$X
    obsID      = modelInputs$obsID
    choice     = modelInputs$choice
    V          = logitFuncs$getMnlV(pars, modelInputs)
    logit      = logitFuncs$getMnlLogit(V, obsID)
    negLL      = logitFuncs$mnlNegLL(choice, logit)
    negGradLL  = logitFuncs$mnlNegGradLL(p, X, pars, choice, logit)
    return(list('objective'=negLL, 'gradient'=negGradLL))
}

getMnlNegLL = function(pars, modelInputs) {
    logitFuncs = modelInputs$logitFuncs
    obsID      = modelInputs$obsID
    choice     = modelInputs$choice
    V          = logitFuncs$getMnlV(pars, modelInputs)
    logit      = logitFuncs$getMnlLogit(V, obsID)
    negLL      = logitFuncs$mnlNegLL(choice, logit)
    return(negLL)
}

getMnlNegGradLL = function(pars, modelInputs) {
    logitFuncs = modelInputs$logitFuncs
    p          = modelInputs$price
    X          = modelInputs$X
    obsID      = modelInputs$obsID
    choice     = modelInputs$choice
    V          = logitFuncs$getMnlV(pars, modelInputs)
    logit      = logitFuncs$getMnlLogit(V, obsID)
    negGradLL  = logitFuncs$mnlNegGradLL(p, X, pars, choice, logit)
    return(negGradLL)
}

getMnlHessLL = function(pars, modelInputs) {
    hessLL = modelInputs$logitFuncs$mnlHessLL(pars, modelInputs)
    return(hessLL)
}

# ============================================================================
# MXL logit and log-likelihood functions
# ============================================================================
# The log-likelihood function is given here as the negative log-likelihood
# because the optim function performs a minimization

# Returns the logit fraction for all the draws in a mxl (heterogeneous) models
getMxlLogit = function(VDraws, obsID) {
    numDraws        = ncol(VDraws)
    expVDraws       = exp(VDraws)
    sumExpVDraws    = rowsum(expVDraws, group=obsID)
    repTimes        = rep(as.numeric(table(obsID)), each=numDraws)
    sumExpVDrawsMat = matrix(rep(sumExpVDraws, times=repTimes),
                      ncol=numDraws, byrow=F)
    logitDraws      = expVDraws / sumExpVDrawsMat
    return(logitDraws)
}

mxlNegLL = function(choice, pHat) {
    negLL = -1*sum(choice*log(pHat))
    return(negLL)
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
mxlNegLLAndGradLL = function(pars, modelInputs){
    logitFuncs    = modelInputs$logitFuncs
    X             = modelInputs$X
    obsID         = modelInputs$obsID
    choice        = modelInputs$choice
    parSetup      = modelInputs$parSetup
    standardDraws = modelInputs$standardDraws
    betaDraws     = makeBetaDraws(pars, modelInputs)
    VDraws        = logitFuncs$getMxlV(betaDraws, modelInputs)
    logitDraws    = logitFuncs$getMxlLogit(VDraws, obsID)
    pHat          = rowMeans(logitDraws)
    negLL         = logitFuncs$mxlNegLL(choice, pHat)
    negGradLL     = logitFuncs$mxlNegGradLL(X, parSetup, obsID, choice,
                    standardDraws, betaDraws, VDraws, logitDraws, pHat)
    return(list('objective'=negLL, 'gradient'=negGradLL))
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
getMxlNegLL = function(pars, modelInputs){
    logitFuncs = modelInputs$logitFuncs
    obsID      = modelInputs$obsID
    choice     = modelInputs$choice
    betaDraws  = makeBetaDraws(pars, modelInputs)
    VDraws     = logitFuncs$getMxlV(betaDraws, modelInputs)
    logitDraws = logitFuncs$getMxlLogit(VDraws, obsID)
    pHat       = rowMeans(logitDraws)
    negLL      = logitFuncs$mxlNegLL(choice, pHat)
    return(negLL)
}

getMxlNegGradLL = function(pars, modelInputs) {
    logitFuncs    = modelInputs$logitFuncs
    X             = modelInputs$X
    obsID         = modelInputs$obsID
    choice        = modelInputs$choice
    parSetup      = modelInputs$parSetup
    standardDraws = modelInputs$standardDraws
    betaDraws     = makeBetaDraws(pars, modelInputs)
    VDraws        = logitFuncs$getMxlV(betaDraws, modelInputs)
    logitDraws    = logitFuncs$getMxlLogit(VDraws, obsID)
    pHat          = rowMeans(logitDraws)
    negGradLL     = logitFuncs$mxlNegGradLL(X, parSetup, obsID, choice,
                    standardDraws, betaDraws, VDraws, logitDraws, pHat)
    return(negGradLL)
}

# ============================================================================
# Numerical log-likelihood functions
# ============================================================================

mnlNegLLAndNumericGradLL = function(pars, modelInputs) {
    negLL     = getMnlNegLL(pars, modelInputs)
    negGradLL = getNumericNegGradLL(pars, modelInputs)
    return(list('objective'=negLL, 'gradient'=negGradLL))
}

mxlNegLLAndNumericGradLL = function(pars, modelInputs) {
    negLL     = getMxlNegLL(pars, modelInputs)
    negGradLL = getNumericNegGradLL(pars, modelInputs)
    return(list('objective'=negLL, 'gradient'=negGradLL))
}

getNumericNegGradLL = function(pars, modelInputs) {
    return(nl.jacobian(
        x0          = pars,
        fn          = modelInputs$evalFuncs$negLL,
        modelInputs = modelInputs))
}

getNumericHessLL = function(pars, modelInputs) {
    return(-1*nl.jacobian(
        x0          = pars,
        fn          = modelInputs$evalFuncs$negGradLL,
        modelInputs = modelInputs))
}

# ============================================================================
# Preference Space Logit Functions - MNL models
# ============================================================================

getMnlV.pref = function(pars, modelInputs) {
    V = modelInputs$X %*% as.numeric(pars)
    return(V)
}

mnlNegGradLL.pref = function(p, X, pars, choice, logit) {
    negGradLL = -1*(t(X) %*% (choice - logit))
    return(negGradLL)
}

# Returns the hessian of the log-likelihood at the given pars
mnlHessLL.pref = function(pars, modelInputs) {
    X        = modelInputs$X
    obsID    = modelInputs$obsID
    choice   = modelInputs$choice
    V        = getMnlV.pref(pars, modelInputs)
    logit    = getMnlLogit(V, obsID)
    diffMat  = getDiffMatByObsID.pref(logit, X, obsID)
    logitMat = repmat(matrix(logit), 1, ncol(X))
    hessLL   = -1*(t(diffMat) %*% (logitMat*diffMat))
    return(hessLL)
}

# Used in computing the hessian
getDiffMatByObsID.pref = function(logit, X, obsID) {
    diffMat = matrix(0, nrow=length(obsID), ncol=ncol(X))
    for (id in sort(unique(obsID))) {
        indices   = which(obsID==id)
        tempX     = X[indices,]
        tempLogit = logit[indices]
        diffMat[indices,] = tempX - repmat(t(tempLogit)%*%tempX, nrow(tempX),1)
    }
    return(diffMat)
}

# ============================================================================
# Preference Space Logit Functions - MXL models
# ============================================================================

# Returns the observed utility
getMxlV.pref = function(betaDraws, modelInputs) {
    VDraws = modelInputs$X %*% t(betaDraws)
    return(VDraws)
}

# Computes the gradient of the negative likelihood for a mixed logit model.
mxlNegGradLL.pref = function(X, parSetup, obsID, choice, standardDraws,
    betaDraws, VDraws, logitDraws, pHat) {
    randParIDs = which(parSetup$dist != 0)
    numDraws   = nrow(standardDraws)
    numBetas   = ncol(standardDraws)
    logNormIDs = which(parSetup$dist == 2)
    repTimes   = rep(as.numeric(table(obsID)), each=2*numBetas)
    # Compute the gradient of V for all parameters
    grad = matrix(0, nrow=nrow(X), ncol=2*numBetas)
    for (i in 1:numDraws) {
        Xtemp    = X
        beta     = betaDraws[i,]
        draws    = standardDraws[i,]
        logit    = logitDraws[,i]
        betaMat  = matrix(rep(beta, nrow(X)), ncol=numBetas, byrow=T)
        drawsMat = matrix(rep(draws, nrow(X)), ncol=numBetas, byrow=T)
        logitMat = matrix(rep(logit, numBetas), ncol=numBetas, byrow=F)
        logitMat = cbind(logitMat, logitMat)
        if (length(logNormIDs) > 0) {
            Xtemp[,logNormIDs] = Xtemp[,logNormIDs]*betaMat[,logNormIDs]
        }
        partial.mu    = Xtemp
        partial.sigma = Xtemp*drawsMat
        partial       = cbind(partial.mu, partial.sigma)
        temp          = rowsum(logitMat*partial, group=obsID)
        tempMat       = matrix(rep(temp, times=repTimes), ncol=ncol(partial),
                        byrow=F)
        grad = grad + logitMat*(partial - tempMat)
    }
    grad           = grad / numDraws
    pHatInvChosen  = matrix(rep(choice*(1/pHat), 2*numBetas), ncol=2*numBetas,
                            byrow=F)
    grad      = colSums(pHatInvChosen*grad)
    negGradLL = -1*grad[c(1:numBetas, numBetas + randParIDs)]
    return(negGradLL)
}


# ============================================================================
# WTP Space Logit Functions - MNL models
# ============================================================================

# Returns the observed utility
getMnlV.wtp = function(pars, modelInputs) {
    lambda = as.numeric(pars[1])
    beta   = as.numeric(pars[2:length(pars)])
    X      = modelInputs$X
    p      = modelInputs$price
    V      = lambda*(p + (X %*% beta))
    return(V)
}

# Returns the negative gradient of the log-likelihood
mnlNegGradLL.wtp = function(p, X, pars, choice, logit) {
    lambda       = as.numeric(pars[1])
    beta         = as.numeric(pars[2:length(pars)])
    gradLLLambda = t(p + (X %*% beta)) %*% (choice - logit)
    gradLLBeta   = lambda*(t(X) %*% (choice - logit))
    negGradLL    = -1*c(gradLLLambda, gradLLBeta)
    return(negGradLL)
}

# Returns the negative hessian of the log-likelihood
mnlHessLL.wtp = function(pars, modelInputs) {
    lambda   = as.numeric(pars[1])
    beta     = as.numeric(pars[2:length(pars)])
    X        = modelInputs$X
    p        = modelInputs$price
    choice   = modelInputs$choice
    obsID    = modelInputs$obsID
    V        = getMnlV.wtp(pars, modelInputs)
    logit    = getMnlLogit(V, obsID)
    diffMat  = getDiffMatByObsID.wtp(lambda, beta, p, X, logit, obsID)
    logitMat = repmat(matrix(logit), 1, ncol(diffMat))
    hessLL   = -1*(t(diffMat) %*% (logitMat*diffMat))
    return(hessLL)
}

# Used in computing the hessian
getDiffMatByObsID.wtp = function(lambda, beta, p, X, logit, obsID) {
    diffMatLambda = matrix(0, nrow=length(obsID), ncol=1)
    diffMatBeta = matrix(0, nrow=length(obsID), ncol=ncol(X))
    for (id in sort(unique(obsID))) {
        indices   = which(obsID==id)
        tempP     = as.matrix(p[indices])
        tempX     = as.matrix(X[indices,])
        tempLogit = logit[indices]
        tempMat   = tempP + (tempX %*% beta)
        diffMatLambda[indices,] = tempMat - repmat(t(tempLogit) %*% tempMat,
            nrow(tempP), 1)
        diffMatBeta[indices,] = lambda*tempX - repmat(t(tempLogit) %*% (lambda*
            tempX), nrow(tempX), 1)
    }
    return(cbind(diffMatLambda, diffMatBeta))
}

# ============================================================================
# WTP Space Logit Functions - MXL models
# ============================================================================

# Returns the observed utility
getMxlV.wtp = function(betaDraws, modelInputs) {
    X           = modelInputs$X
    p           = modelInputs$price
    numDraws    = nrow(betaDraws)
    lambdaDraws = matrix(rep(betaDraws[,1], nrow(X)),ncol=numDraws,byrow=T)
    gammaDraws  = matrix(betaDraws[,2:ncol(betaDraws)], nrow=numDraws)
    pMat        = matrix(rep(p, numDraws), ncol=numDraws, byrow=F)
    return(lambdaDraws*(pMat + X%*%t(gammaDraws)))
}













# ********
# This function is still broken! Need to re-derive the gradient for WTP
# space MXL models
# Computes the gradient of the negative likelihood for a mixed logit model.
mxlNegGradLL.wtp = function(X, parSetup, obsID, choice, standardDraws,
    betaDraws, VDraws, logitDraws, pHat) {
    randParIDs     = which(parSetup$dist != 0)
    numDraws       = nrow(standardDraws)
    numBetas       = ncol(standardDraws)
    numRandom      = length(randParIDs)
    logNormIDs     = which(parSetup$dist == 2)
    lambdaDraws    = betaDraws[,1]
    lambdaDrawsMat = matrix(rep(lambdaDraws, nrow(X)), ncol=numDraws,byrow=T)
    gammaDraws     = matrix(betaDraws[,2:ncol(betaDraws)], nrow=numDraws)
    # Compute the inner gradient summation across the draws for all
    # parameters
    gradLambda      = matrix(0, nrow=nrow(X), ncol=1)
    gradMuGamma    = matrix(0, nrow=nrow(X), ncol=numBetas-1)
    gradSigmaGamma = gradMuGamma
    for (i in 1:numDraws) {
        V         = VDraws[,i]
        lambda    = lambdaDraws[i]
        gamma     = gammaDraws[i,]
        draws     = standardDraws[i,]
        logit     = logitDraws[,i]
        lambdaMat = lambdaDrawsMat[,i]
        gammaMat  = matrix(rep(gamma, nrow(X)), ncol=ncol(X), byrow=T)
        drawsMat  = matrix(rep(draws, nrow(X)), ncol=numBetas,byrow=T)
        logitMat  = matrix(rep(logit, ncol(X)), ncol=ncol(X), byrow=F)
        # get the inner gradient partial derivatives for lambda, muGamma, and sigmaGamma
        gradLambda     = gradLambda +
                         getGradLambda.wtp(lambda, V, logit, obsID)
        gradMuGamma    = gradMuGamma +
                         getGradGamma.wtp(X, lambda, logNormIDs, gammaMat,
                         drawsMat, logitMat, obsID, 'mu')
        gradSigmaGamma = gradSigmaGamma +
                         getGradGamma.wtp(X, lambda, logNormIDs, gammaMat,
                         drawsMat, logitMat, obsID, 'sigma')
    }
    gradLambda          = gradLambda / numDraws
    gradMuGamma        = gradMuGamma / numDraws
    gradMu             = c(gradLambda, gradMuGamma)
    gradSigmaGamma     = gradSigmaGamma / numDraws
    gradSigma          = gradSigmaGamma
    pHatInvChosenMu    = matrix(rep(choice*(1/pHat), numBetas),
                                ncol=numBetas,byrow=F)
    pHatInvChosenSigma = matrix(rep(choice*(1/pHat), numRandom),
                                ncol=numRandom, byrow=F)
    gradMu             = colSums(pHatInvChosenMu*gradMu)
    gradSigma          = colSums(pHatInvChosenSigma*gradSigma)
    negGradLL          = -1*c(gradMu, gradSigma)
    names(negGradLL)   = names(pars)
    return(negGradLL)
}

getGradLambda.wtp = function(lambda, V, logit, obsID) {
    partial = V / lambda
    temp = matrix(rep(rowsum(logit*partial, group=obsID), each=3),
                     ncol=1, byrow=F)
    innerGrad = logit*(partial - temp)
    return(innerGrad)
}

getGradGamma.wtp = function(X, lambda, logNormIDs, gammaMat, drawsMat,
    logitMat, obsID, id) {
    partial = lambda*X
    if (id == 'sigma') {partial = partial*drawsMat[,2:ncol(drawsMat)]}
    if (length(logNormIDs) > 0) {
        partial[,logNormIDs] =
        partial[,logNormIDs]*gammaMat[,logNormIDs]
    }
    tempMat = matrix(rep(rowsum(logitMat*partial, group=obsID), each=3),
                     ncol=ncol(partial), byrow=F)
    innerGrad = logitMat*(partial - tempMat)
    return(innerGrad)
}
# ********





# ============================================================================
# Functions for taking draws for mixed logit models
# ============================================================================

# Returns shifted normal draws for each parameter
makeBetaDraws = function(pars, modelInputs){
    muMat    = getMuMat(pars, modelInputs)
    sigmaMat = getSigmaMat(pars, modelInputs)
    # Shift draws by mu and sigma
    betaDraws = muMat + modelInputs$standardDraws*sigmaMat
    # Exponentiate draws for those with logN distribution
    logNormIDs = which(modelInputs$parSetup$dist==2)
    if (length(logNormIDs) > 0) {
        betaDraws[,logNormIDs] = exp(betaDraws[,logNormIDs])
    }
    return(betaDraws)
}

getMuMat = function(pars, modelInputs) {
    pars.mu = pars[1:modelInputs$numBetas]
    numDraws = modelInputs$options$numDraws
    return(matrix(rep(pars.mu, numDraws), ncol=length(pars.mu), byrow=T))
}

getSigmaMat = function(pars, modelInputs) {
    numDraws   = modelInputs$options$numDraws
    pars.sigma = rep(0, modelInputs$numBetas)
    randParIDs = which(modelInputs$parSetup$dist != 0)
    pars.sigma[randParIDs] = pars[(modelInputs$numBetas+1):length(pars)]
    return(matrix(rep(pars.sigma, numDraws), ncol=length(pars.sigma),
           byrow=T))
}

getStandardDraws = function(parSetup, options) {
    numDraws = options$numDraws
    numBetas = nrow(parSetup)
    # Use Halton draws by default
    draws = getHaltonDraws(numDraws, numBetas)
    if (options$drawType=='normal') {
        draws = getNormalDraws(numDraws, numBetas)
    }
    if (options$drawType=='sobol') {
        draws = getSobolDraws(numDraws, numBetas)
    }
    fixedParIDs = which(parSetup$dist==0)
    draws[,fixedParIDs] = rep(0, numDraws)
    return(draws)
}

# Returns a matrix of numDraws x numBetas random standard normal draws
getNormalDraws = function(numDraws, numBetas) {
    N        = numDraws*numBetas
    draws    = rnorm(N, 0, 1)
    badDraws = which(pnorm(draws) < 0.001 | pnorm(draws) > 0.999)
    # Make sure that none of the draws are too extreme in the tail. It
    # causes the likelihood function to crash because the utility value
    # becomes too close to zero, and then taking log(0) crashes.
    while (length(badDraws) > 0){
        draws[badDraws] = rnorm(length(badDraws), 0, 1)
        badDraws = which(pnorm(draws) < 0.001 | pnorm(draws) > 0.999)
    }
    drawsMatrix = matrix(draws, ncol=numBetas)
    return(drawsMatrix)
}

# Returns a matrix of numDraws x numBetas random standard normal draws
# approximated using Halton draws
getHaltonDraws = function(numDraws, numBetas) {
    return(halton(n=numDraws, dim=numBetas, normal=T))
}

# Returns a matrix of numDraws x numBetas random standard normal draws
# approximated using Sobol draws
getSobolDraws = function(numDraws, numBetas) {
    return(sobol(n=numDraws, dim=numBetas, scrambling=3, normal=T))
}









# ============================================================================
# Other functions
# ============================================================================

# R equivalent of matlab's repmat function
repmat = function(X, m, n) {
    mx = dim(X)[1]
    nx = dim(X)[2]
    return(matrix(t(matrix(X,mx,nx*n)), mx*m, nx*n, byrow=T))
}
