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
logitr = function(data, choiceName, obsIDName, parNames, priceName=NULL,
                  randPars=NULL, randPrice=NULL, modelSpace='pref',
                  options=list()) {
    require(nloptr) # Required for optimization
    modelInputs = getModelInputs(data, choiceName, obsIDName, parNames,
                  randPars, priceName, randPrice, modelSpace, options)
    allModels   = runMultistart(modelInputs)
    if (options$keepAllRuns) {
        models = appendAllModelsInfo(allModels, modelInputs)
        cat('Done!', '\n', sep='')
        return(models)
    } else {
        bestModel = getBestModel(allModels)
        bestModel = appendModelInfo(bestModel, modelInputs)
        cat('Done!', '\n', sep='')
        return(bestModel)
    }
}

appendAllModelsInfo = function(allModels, modelInputs) {
    models = list()
    for (i in 1:length(allModels)) {
        models[[i]] = appendModelInfo(allModels[[i]], modelInputs)
    }
    bestModel = getBestModel(allModels)
    bestModel = appendModelInfo(bestModel, modelInputs)
    result    = list(models=models, bestModel=bestModel)
    class(result) = c('logitr.multistart', 'logitr')
    return(result)
}

getBestModel = function(models) {
    logLikVals = getLogLikVals(models)
    bestModel  = models[[which(logLikVals==max(logLikVals))[1]]]
    return(bestModel)
}

getLogLikVals = function(models) {
    logLikVals = matrix(0)
    for (i in 1:length(models)) {
        logLikVals[i] = models[[i]]$logLik
    }
    return(logLikVals)
}
