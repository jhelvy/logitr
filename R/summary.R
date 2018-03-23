# ============================================================================
# Functions for printing results and summaries
# ============================================================================

#' Prints a summary of an estimated model of the 'logitr' class.
#'
#' Prints a summary of an estimated model of the 'logitr' class.
#' @keywords logitr, summary
#' @export
#' @examples
#' # View a summary of an estimate model:
#' summary(model)
summary.logitr = function(model) {
    if ('bestModel' %in% names(model)) {
        # This is a list of all models from a multistart, so print the
        # multistart summary first and then print the summary of the best model
        printLine()
        cat('SUMMARY OF ALL MULTISTART RUNS:', '\n', '\n', sep='')
        multistartSummary = getMulitstartSummary(model)
        print(multistartSummary)
        cat('---', '\n', sep='')
        cat('To view meaning of status codes, use logitr.statusCodes()', '\n')
        cat('\n', sep='')
        cat('Summary of BEST model below (run with largest',
            'log-likelihood value)', sep=' ')
        cat('\n', sep='')
        printModelSummary(model$bestModel)
    } else {
        printModelSummary(model)
    }
}

printLine = function() {
    cat('=================================================', '\n', sep='')
}

getMulitstartSummary = function(model) {
    models  = model$models
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

printModelSummary = function(model) {
    basicInfoSummary = getBasicInfoTable(model)
    coefTable        = getCoefTable(model)
    statTable        = getStatTable(model)
    printLine()
    cat('MODEL SUMMARY:', '\n')
    print(basicInfoSummary)
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
        cat('Summary of 10k Draws for Random Coefficients:', '\n')
        print(model$randParSummary)
    }
}

getBasicInfoTable = function(model) {
    modelSpace = 'Preference'
    if (model$modelSpace=='wtp') {modelSpace = 'Willingness-to-Pay'}
    modelRun = paste(model$multistartNumber, 'of',
                     model$options$numMultiStarts, sep=' ')
    modelTime = paste(round(model$time['elapsed'], 3), 'sec', sep=' ')
    basicInfoSummary = data.frame(c(modelSpace, modelRun, model$iterations,
        modelTime))
    colnames(basicInfoSummary) = ''
    row.names(basicInfoSummary) = c('Model Space:', 'Model Run:',
        'Iterations:', 'Elapsed Time:')
    return(basicInfoSummary)
}

getCoefTable = function(model) {
    coef      = model$coef
    se        = model$standErrs
    numObs    = model$numObs
    numParams = model$numParams
    tStat  = rep(NA, length(coef))
    pVal   = rep(NA, length(coef))
    signif = rep('', length(coef))
    if (sum(is.na(se))==0) {
        tStat  = as.numeric(coef / se)
        dof    = numObs - numParams
        pVal   = 2*(1-pt(abs(tStat), dof))
        signif = rep('', length(pVal))
        signif[which(pVal <= 0.001)] <- '***'
        signif[which(pVal >  0.001 & pVal <= 0.01)] <- '**'
        signif[which(pVal >  0.01  & pVal <= 0.05)] <- '*'
        signif[which(pVal >  0.05  & pVal <= 0.1)] <- '.'
    }
    coefTable = data.frame(
        Estimate=round(coef, 6), StdError=round(se, 6), tStat=round(tStat, 4),
        pVal=round(pVal, 4), signif=signif)
    row.names(coefTable) = names(coef)
    return(coefTable)
}

getStatTable = function(model) {
    numObs     = model$numObs
    numParams  = model$numParams
    logLik     = model$logLik
    nullLogLik = model$nullLogLik
    aic        = round(2*numParams - 2*logLik, 4)
    bic        = round(log(numObs)*numParams - 2*logLik, 4)
    result = t(data.frame(
        'Log-Likelihood:'         = logLik,
        'Null Log-Likelihood:'    = nullLogLik,
        'AIC:'                    = aic,
        'BIC:'                    = bic,
        'McFadden R2:'            = 1 - (logLik / nullLogLik),
        'Adj. McFadden R2'        = 1 - ((logLik - numParams) / nullLogLik),
        'Number of Observations:' = numObs))
    colnames(result) = ''
    return(result)
}

getSummaryTable = function(model) {
    coefMat      = getCoefTable(model)
    statMat      = getStatTable(model)
    summaryTable = rbind(coefMat, statMat)
    return(summaryTable)
}

getWtpComparison = function(wtpSpace.coef, wtpSpace.logLik, modelInputs) {
    wtpComparison = data.frame(
        prefSpace = modelInputs$prefSpace.wtp,
        wtpSpace  = wtpSpace.coef)
    # Add logLik values
    logLikCompare     = c(modelInputs$prefSpace.logLik, wtpSpace.logLik)
    wtpComparison = rbind(wtpComparison, logLikCompare)
    row.names(wtpComparison)[nrow(wtpComparison)] = 'logLik'
    wtpComparison$difference = round(wtpComparison$wtpSpace -
        wtpComparison$prefSpace, 8)
    return(wtpComparison)
}

#' Returns the coefficients of an estimated model of the 'logitr' class.
#'
#' Returns the coefficients of an estimated model of the 'logitr' class.
#' @keywords logitr, coef
#' @export
#' @examples
#' # Get the coefficients of an estimated model:
#' coef(model)
coef.logitr = function(model) {
    return(model$coef)
}
