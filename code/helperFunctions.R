# Load required libraries
suppressMessages(library(optimx))

# ============================================================================
# Functions for scaling the model inputs
# ============================================================================

# Function that scales a variable to be between 0 and 1.
scaleVar = function(var, scaleOrder) {
    scalingFactor = 1
    data = var/scalingFactor
    while (abs(mean(data)) >= scaleOrder) {
        scalingFactor = scalingFactor*10
        data = var/scalingFactor
    }
    result = list(var/scalingFactor, scalingFactor)
    return(result)
}

# Function that scales all the variables in X to be between 0 and 1.
scaleX = function(X, scaleOrder) {
    scaleFactors = rep(0, ncol(X))
    for (col in 1:ncol(X)) {
        scalingFactor = 1
        data = X[,col]/scalingFactor
        while (abs(mean(data)) >= scaleOrder) {
            scalingFactor = scalingFactor*10
            data = X[,col]/scalingFactor
        }
        scaleFactors[col] = scalingFactor
        X[,col] = data
    }
    result = list(X, scaleFactors)
    return(result)
}

# ============================================================================
# Functions for making starting points
# ============================================================================

# Returns a random starting point for the optimization routine
getRegularStartPoint = function(d) {
    return(runif(d$numParams, d$lowerBound, d$upperBound))
}

# Returns a custom starting point for the optimization routine
getCustomStartPoint = function(d) {
    pars = rep(0, length(d$lowerBound))
    for (i in 1:length(d$lowerBound)) {
        lb = d$lowerBound[i]
        ub = d$upperBound[i]
        pars[i] = runif(1, lb, ub)
    }
    return(pars)
}

# ============================================================================
# Functions for printing a header
# ============================================================================

# Prints a horizontal line
printHorizontalLine = function() {
    cat(rep('=',79), '\n', sep='')
}

printTitle = function(msg) {
    msgLength = nchar(msg)
    spaceLength = (80 - msgLength)/2 - 1
    printHorizontalLine()
    cat(rep(' ',spaceLength), msg, rep(' ',spaceLength), '\n', sep='')
    printHorizontalLine()
}

# Prints a header to explain the model being run
printHeader = function(d) {
    cat('                     Model Type:', d$modelType,     '\n')
    cat('                    Model Space:', d$modelSpace,    '\n')
    cat('                  Weights Used?:', d$useWeights,    '\n')
    cat('        Analytic Gradient Used?:', d$useAnalyticGrad,       '\n')
    cat('             Parameters Scaled?:', d$scaleParams,   '\n')
    cat('          Number of Multistarts:', d$numMultiStarts, '\n')
    cat('  Max Iterations per Multistart:', d$maxMultiStartIters, '\n')
    cat('    Max Iterations for Best Run:', d$maxFinalIters, '\n')
    cat('         Number of Random Draws:', d$numDraws,      '\n')
    cat('                   optimx used?:', d$useOptimx,      '\n')
    cat('         Optimization algorithm:', d$optAlgorithm,      '\n')
    printHorizontalLine()
}

# ============================================================================
# Main optimization functions
# ============================================================================

# Does the optimization loop and does NOT compute the hessian. Does NOT
# print results. This is used in the multistart runs.
runFastOptimization = function(pars, d) {
    if (d$useOptimx) {
      if (d$useAnalyticGrad) {
          model = optimx(par=pars, fn=d$negLL, gr=d$negGradLL,
                         method=d$optAlgorithm, hessian=FALSE,
                         control=list(maxit=d$maxMultiStartIters), d=d)
      } else {
          model = optimx(par=pars, fn=d$negLL,
                         method=d$optAlgorithm, hessian=FALSE,
                         control=list(maxit=d$maxMultiStartIters), d=d)
      }
    } else {
      if (d$useAnalyticGrad) {
          model = optim(par=pars, fn=d$negLL, gr=d$negGradLL,
                         method=d$optAlgorithm, hessian=FALSE,
                         control=list(maxit=d$maxMultiStartIters), d=d)
      } else {
          model = optim(par=pars, fn=d$negLL,
                         method=d$optAlgorithm, hessian=FALSE,
                         control=list(maxit=d$maxMultiStartIters), d=d)
      }
    }
    return(model)
}

# Does the optimization loop, computes the hessian, and print results.
runOptimization = function(pars, d) {
    if (d$useOptimx) {
      if (d$useAnalyticGrad) {
          model = optimx(par=pars, fn=d$negLL, gr=d$negGradLL,
                         method=d$optAlgorithm, hessian=TRUE,
                         control=list(maxit=d$maxFinalIters,
                         trace=1, REPORT=T), d=d)
      } else {
          model = optimx(par=pars, fn=d$negLL,
                         method=d$optAlgorithm, hessian=TRUE,
                         control=list(maxit=d$maxFinalIters,
                         trace=1, REPORT=T), d=d)
      }
    } else {
      if (d$useAnalyticGrad) {
          model = optim(par=pars, fn=d$negLL, gr=d$negGradLL,
                         method=d$optAlgorithm, hessian=TRUE,
                         control=list(maxit=d$maxFinalIters,
                         trace=1, REPORT=T), d=d)
      } else {
          model = optim(par=pars, fn=d$negLL,
                         method=d$optAlgorithm, hessian=TRUE,
                         control=list(maxit=d$maxFinalIters,
                         trace=1, REPORT=T), d=d)
      }
    }
    return(model)
}

# ============================================================================
# Functions for making the model summary
# ============================================================================

# Function that summarizes the results of an estimated model. Calculates
# the standard errors and creates a summary table. Also gives information
# on the type of model that was run as well as the log-likelihood values,
# AIC, and McFadden R2 at the solution. Returns all results as a data frame.
getModelSummary = function(d) {
    # get header Mat
    settingsMat = getSettingsMat(d)
    # get summary Mat
    summaryMat = getSummaryMat(d)
    # get model fit Mat
    modelFitMat = getModelFitMat(d)
    # combine into one data frame
    appendMat = rbind(modelFitMat, settingsMat)
    fillMat   = matrix(data='', nrow=nrow(appendMat), ncol=4)
    otherDF   = as.data.frame(cbind(appendMat[,2], fillMat))
    row.names(otherDF) = appendMat[,1]
    colnames(otherDF) = colnames(summaryMat)
    modelSummary = rbind(summaryMat, otherDF)
    return(modelSummary)
}

getSettingsMat = function(d) {
    settings     = c(d$modelType, d$modelSpace, d$useWeights,
                     d$useAnalyticGrad, d$scaleParams, d$numMultiStarts,
                     d$maxMultiStartIters, d$maxFinalIters, d$numDraws,
                     d$useOptimx, d$optAlgorithm)
    settingNames = c('Model Type:', 'Model Space:', 'Weights Used?:',
                     'Analytic Gradient Used?:', 'Parameters Scaled?:',
                     'Number of Multistarts:',
                     'Max Iterations per Multistart:',
                     'Max Iterations for Best Run:',
                     'Number of Random Draws:', 'optimx used?:',
                     'Optimization algorithm:')
    settingsMat = data.frame(name=settingNames, value=settings)
    return(settingsMat)
}

getSummaryMat = function(d) {
    roundDigit = 3
    betas = getModelPars(d$bestModel, d)
    betas = getScaledModelPars(betas, d$modelSpace, d$scaleFactors)
    # get the standard errors
    hessian = getModelHessian(d$bestModel, d)
    sds     = getScaledModelSDs(hessian, d$modelSpace, d$scaleFactors)
    # get the main summary stats
    tStat              = as.numeric(betas / sds)
    degreesOfFreedom   = d$numObs - d$numParams
    pVal               = 2*(1-pt(abs(tStat), degreesOfFreedom))
    signif             = rep('', length(pVal))
    signif[which(pVal <= 0.1 && pVal > 0.5)] <- '*'
    signif[which(pVal <= 0.05 && pVal > 0.01)] <- '**'
    signif[which(pVal <= 0.01)] <- '***'
    # round and append other information
    betas = round(betas, roundDigit)
    sds   = round(sds,   roundDigit)
    tStat = round(tStat, roundDigit)
    pVal  = round(pVal,  roundDigit)
    # make summaryMat
    summaryMat = data.frame('Estimate'=as.numeric(betas), 'StdError'=sds,
                            'tStat'=tStat, 'pVal'=pVal, 'signif'=signif)
    row.names(summaryMat) <- names(betas)
    return(summaryMat)
}

getModelFitMat = function(d) {
    #  Get AIC, McFadden R2, and adjusted McFadden R2
    aic        = round(2*d$logL - 2*d$numParams, 3)
    mcFaddenR2 = round(1 - abs(d$logL/d$nullLogL), 3)
    adjMcFaddenR2 = round(1 - abs((d$logL - d$numParams)/d$nullLogL), 3)
    # round and combine
    logL         = round(d$logL, 3)
    nullLogL     = round(d$nullLogL, 3)
    modelFitInfo = matrix(c(logL, nullLogL, aic, mcFaddenR2, adjMcFaddenR2,
                   d$numObs))
    modelFitInfoNames = c('LL at Convergence:', 'Null LL:', 'AIC:',
                          'McFadden R2:', 'Adj. McFadden R2',
                          'Number of Observations:')
    modelFitMat = data.frame(name=modelFitInfoNames, value=modelFitInfo)
    return(modelFitMat)
}


# ============================================================================
# Functions for returning the LogL value, parameters, hessian, and
# standard errors from an estimated model.
# ============================================================================

# Returns the model log likelihood value at convergence given an optim
# model object
getModelLogLValue = function(model) {
    logL = -1*model$value
    return(logL)
}

# Returns the model parameters as a vector given an optim model object
getModelPars = function(model, d) {
    if (d$useOptimx) {
        pars        = as.numeric(model[1:d$numParams])
        names(pars) = names(model[1:d$numParams])
    } else {
        pars = model$par
    }
    # If it's a MXL model, then make the SD coefficients positive
    # Some may be negative, but that doesn't matter. Making them positive
    # is purely to avoid confusion
    if (d$modelType == 'mxl') {
        pars[(d$numBetas + 1):length(pars)] =
        abs(pars[(d$numBetas + 1):length(pars)])
    }
    return(pars)
}

# Returns the model hessian as a matrix given an optim model object
getModelHessian = function(model, d) {
    if (d$useOptimx) {
        hessian            = as.matrix(attr(model, "details")[1,]$nhatend)
        colnames(hessian)  = names(model[1:d$numParams])
        row.names(hessian) = colnames(hessian)
    } else {
        hessian = model$hessian
    }
    return(hessian)
}

# Returns the model standard errors as a vector given an optim model object
getModelSDs = function(model, d) {
    hessian   = getModelHessian(model, d)
    varCovMat = solve(hessian)
    sds       = sqrt(abs(diag(varCovMat)))
    return(sds)
}

# ============================================================================
# Functions for returning scaled model pars, hessians, and standard errors.
# ============================================================================

# Returns the correctly scaled model parameters as a vector given an
# optim model object
getScaledModelPars = function(pars, modelSpace, scaleFactors) {
    if (modelSpace == 'pref') {
        scaledPars = pars/scaleFactors
    } else {
        scaledPars    = pars/scaleFactors*scaleFactors[1]
        scaledPars[1] = scaledPars[1]/scaleFactors[1]
    }
    return(scaledPars)
}

# Returns the correctly scaled model hessian as a matrix given an optim
# model object
getScaledModelHessian = function(hessian, modelSpace, scaleFactors) {
    sf    = matrix(scaleFactors, ncol=1)
    sfMat = sf%*%t(sf)
    if (modelSpace == 'pref') {
        scaledHess = hessian*sfMat
    } else {
        scaledHess = hessian*sfMat/scaleFactors[1]^2
        scaledHess[1,] = scaledHess[1,]*scaleFactors[1]
        scaledHess[,1] = scaledHess[,1]*scaleFactors[1]
    }
    return(scaledHess)
}

# Returns the correctly scaled standard errors as a vector given an optim
# model object
getScaledModelSDs = function(hessian, modelSpace, scaleFactors) {
    scaledHessian = getScaledModelHessian(hessian, modelSpace, scaleFactors)
    varCovMat     = solve(scaledHessian)
    scaledSDs     = sqrt(abs(diag(varCovMat)))
    return(scaledSDs)
}

# ============================================================================
# Other functions
# ============================================================================

# Converts the user-provided information on parameters into a data frame.
# The distributional codes are:
# 0 = fixed
# 1 = normally distributed
# 2 = log-normally distributed
getCovariateSetupDF = function(d){
    # Combine all the covariates
    covariate    = c(d$priceVar, d$fixedVars, d$normalVars, d$logNormalVars)
    # Assign distributions
    distribution = rep(0, length(covariate))
    distribution[which(covariate == d$priceVar)]        = d$priceDist
    distribution[which(covariate %in% d$normalVars)]    = 1
    distribution[which(covariate %in% d$logNormalVars)] = 2
    # Create the covariateSetup data frame
    covariateSetup = data.frame(covariate=covariate, distribution=distribution)
    covariateSetup$covariate = as.character(covariateSetup$covariate)
    # If it's a MNL model, then force all parameters to be fixed
    if (d$modelType == 'mnl') {
        covariateSetup$distribution = rep(0, length(covariate))
    }
    return(covariateSetup)
}
