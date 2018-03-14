# ============================================================================
# Functions for printing results and summaries
# ============================================================================

#' Prints a summary of an estimated model using the logitr package.
#'
#' Prints a summary of an estimated model using the logitr package.
#' @keywords logitr, summary
#' @export
#' @examples
#' # View a summary of an estimate model:
#' summary(model)
summary.logitr = function(model) {
    if ('multistartSummary' %in% names(model)) {
        # This is a list of all models from a multistart, so print the
        # multistart summary first and then print the summary of the best model
        printLine()
        cat('SUMMARY OF ALL MULTISTART RUNS:', '\n', '\n', sep='')
        print(model$multistartSummary)
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
    if (model$modelSpace=='wtp') {modelSpace = 'Willingness-to-Pay'}
    modelRun = paste(model$multistartNumber, 'of',
                     model$options$numMultiStarts, sep=' ')
    modelTime = paste(round(model$time['elapsed'], 3), 'sec', sep=' ')
    bestModelSummary = data.frame(c(modelSpace, modelRun, model$iterations,
        modelTime))
    colnames(bestModelSummary) = ''
    row.names(bestModelSummary) = c('Model Space:', 'Model Run:',
        'Iterations:', 'Elapsed Time:')
    # Get the coef and stats tables
    summaryTable = model$summaryTable
    llRowID = which(row.names(summaryTable)=='Log-Likelihood:')
    coefIDs = (1:(llRowID-1))
    statIDs = (llRowID:nrow(summaryTable))
    coefTable = summaryTable[coefIDs,]
    statTable = data.frame(round(summaryTable[statIDs,1], 8))
    colnames(statTable) = ''
    row.names(statTable) = row.names(summaryTable)[statIDs]
    # Print the summary
    printLine()
    cat('MODEL SUMMARY:', '\n')
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
        cat('Summary of 10k Draws for Random Coefficients:', '\n')
        print(model$randParSummary)
    }
    if ((model$modelSpace=='wtp') & (sum(is.na(model$wtpComparison))==0)) {
        cat('\n')
        cat('Comparison of WTP Between Preference and WTP Space Models:', '\n')
        print(format(model$wtpComparison, scientific=F))
    }
}
