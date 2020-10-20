# ============================================================================
# Main functions
# ============================================================================

#' The main function for running the logitr program
#'
#' logitr estimates multinomial (MNL) and mixed logit (MXL) models in R.
#' Models can be estimated using "Preference" space or "Willingness-to-pay
#' (WTP)" space utility parameterizations The program includes an option to
#' run a multistart optimization loop with random starting points in each
#' iteration, which is useful for non-convex problems like MXL models or
#' models with WTP space utility parameterizations. The main optimization loop
#' uses the nloptr function to minimize the negative log-likelihood function.
#' @keywords logitr, mnl, mxl, wtp, willingness-to-pay, mixed logit, logit
#'
#' @export
#' @examples
#' # For an example go to: https://github.com/jhelvy/logitr/tree/master/example
logitr = function(data, choiceName, obsIDName, parNames, priceName = NULL,
                  randPars = NULL, randPrice = NULL, modelSpace = 'pref',
                  weightsName = NULL, options = list()) {
    require(nloptr) # Required for optimization
    modelInputs = getModelInputs(data, choiceName, obsIDName, parNames,
                  randPars, priceName, randPrice, modelSpace, weightsName,
                  options)
    allModels = runMultistart(modelInputs)
    if (modelInputs$options$keepAllRuns) {
        models = appendAllModelsInfo(allModels, modelInputs)
        cat('Done!', '\n', sep='')
        return(models)
    } else {
        bestModel = getBestModel(allModels, modelInputs)
        if (modelInputs$options$numMultiStarts > 1) {
            bestModel$multistartSummary = getMultistartSummary(allModels)
            class(bestModel) = c('logitr', 'logitr.multistart')
        }
        cat('Done!', '\n', sep='')
        return(bestModel)
    }
}

appendAllModelsInfo = function(allModels, modelInputs) {
    models = list()
    for (i in 1:length(allModels)) {
        models[[i]] = appendModelInfo(allModels[[i]], modelInputs)
    }
    bestModel = getBestModel(allModels, modelInputs)
    multistartSummary = getMultistartSummary(models)
    result = structure(list(models=models, bestModel=bestModel,
        multistartSummary=multistartSummary),
        class = c('logitr', 'logitr.multistart', 'logitr.allRuns'))
    return(result)
}

getMultistartSummary = function(models) {
    summary = as.data.frame(matrix(0, ncol=4, nrow=length(models)))
    colnames(summary) = c('run', 'logLik', 'iterations', 'status')
    for (i in 1:length(models)) {
        summary[i, 1] = i
        summary[i, 2] = round(models[[i]]$logLik, 5)
        summary[i, 3] = models[[i]]$iterations
        summary[i, 4] = models[[i]]$status
    }
    return(summary)
}

getBestModel = function(models, modelInputs) {
    logLikVals = getLogLikVals(models)
    bestModel  = models[[which(logLikVals == max(logLikVals))[1]]]
    bestModel  = appendModelInfo(bestModel, modelInputs)
    return(bestModel)
}

getLogLikVals = function(models) {
    logLikVals = matrix(0)
    for (i in 1:length(models)) {
        logLikVals[i] = models[[i]]$logLik
    }
    return(logLikVals)
}


