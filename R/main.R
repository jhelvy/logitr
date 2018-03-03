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
logitr = function(data, choiceName, obsIDName, parNames, priceName=NULL,
                  parDist=NULL, priceDist=NULL, modelSpace='pref',
                  prefSpaceModel=NULL, standardDraws=NULL, options=list()) {
    require(randtoolbox) # Required for taking Halton draws
    require(nloptr)      # Required for optimization
    options = runOptionsChecks(options)
    # Prepare the modelInputs list
    modelInputs = getModelInputs(data, choiceName, obsIDName, parNames,
        parDist, priceName, priceDist, modelSpace, prefSpaceModel,
        standardDraws, options)
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
