# ============================================================================
# functions that create specific error messages

printMissingInputWarningMsg = function(missingVar, setVar) {
    msg = paste('Warning: ', missingVar, ' not found! \n...setting ',
                missingVar, ' to default value of ', setVar, '\n', sep='')
    cat(msg)
}

missingInputError = function(missingVar) {
    errMsg = paste(missingVar, ' not found! \nPlease provide a string ',
                   'value for ', missingVar,  ' and re-run the program.\n',
                   sep='')
    stop(errMsg, call. = FALSE)
}

# ============================================================================
# Check for the main settings

if (!exists('modelType')) {
    printMissingInputWarningMsg('"modelType"', '"mnl"')
    modelType = 'mnl'
}
if (!exists('modelSpace')) {
    printMissingInputWarningMsg('"modelSpace"', '"pref"')
    modelSpace = 'pref'
}
if (!exists('useWeights')) {
    printMissingInputWarningMsg('"useWeights"', 'FALSE')
    useWeights = FALSE
}
if (!exists('useAnalyticGrad')) {
    printMissingInputWarningMsg('"useAnalyticGrad"', 'TRUE')
    useAnalyticGrad = TRUE
}
if (!exists('scaleParams')) {
    printMissingInputWarningMsg('"scaleParams"', 'FALSE')
    scaleParams = FALSE
}
if (!exists('numMultiStarts')) {
    printMissingInputWarningMsg('"numMultiStarts"', '1')
    numMultiStarts = 1
}
if (!exists('maxMultiStartIters')) {
    printMissingInputWarningMsg('"maxMultiStartIters"', '100')
    maxMultiStartIters = 100
}
if (!exists('maxFinalIters')) {
    printMissingInputWarningMsg('"maxFinalIters"', '1000')
    maxFinalIters = 1000
}
if (!exists('numDraws')) {
    printMissingInputWarningMsg('"numDraws"', '125')
    numDraws = 125
}
if (!exists('useOptimx')) {
    printMissingInputWarningMsg('"useOptimx"', 'FALSE')
    ouseOptimx = FALSE
}
if (!exists('optAlgorithm')) {
    printMissingInputWarningMsg('"optAlgorithm"', '"BFGS"')
    optAlgorithm = 'BFGS'
}

# ============================================================================
# Check for the data & parameter setup settings

if (!exists('choiceData')) {
    missingInputError('"choiceData"')
}
if (!exists('respondentID')) {
    missingInputError('"respondentID"')
}
if (!exists('observationID')) {
    missingInputError('"observationID"')
}
if (!exists('choice')) {
    missingInputError('"choice"')
}
if (!exists('weights')) {
    missingInputError('"weights"')
}
if (modelSpace == 'wtp') {
    if (!exists('priceVar')) {
        stop(paste('"priceVar" not found! \nPrice is a required covariate ',
                   'for WTP space models. Please provide a string value for ',
                   '"priceVar" and re-run the program.\n', sep=''),call.=FALSE)
    }
}
if (!exists('priceVar')) {
    printMissingInputWarningMsg('"priceVar"', NULL)
    priceVar = NULL
}
if (!exists('priceDist')) {
    printMissingInputWarningMsg('"priceDist"', 0)
    priceDist = 0
}
if ((modelSpace == 'wtp') && (priceDist != 0)) {
    # For wtp models, right now logitr can only handle fixed price coefficients
    cat('Warning: the current version of logitr can only handle models ',
        'for which\nthe price coefficient is fixed.\n...setting "priceDist" ',
        'to 0 (fixed).', sep='')
    priceDist = 0
}
if (!exists('fixedVars') & !exists('normalVars') & !exists('logNormalVars')) {
    stop(paste('No covariates provided for the model! \nPlease provide ',
               'values for "fixedVars", "normalVars", or "logNormalVars" ',
               'and \nre-run the program.\n', sep=''), call.=FALSE)
}
if (!exists('fixedVars')) {
    fixedVars = NULL
}
if (!exists('normalVars')) {
    normalVars = NULL
}
if (!exists('logNormalVars')) {
    logNormalVars = NULL
}
if (modelType == 'mnl') {
    if (length(normalVars) > 0) {
        cat('Warning: Some covariates are listed as heterogeneously ',
            'distributed, but the \nmodel is type "mnl". \n...the ',
            'estimation will proceed with all covariates modeled as ',
            'fixed parameters.\n', sep='')
    } else if (length(logNormalVars) > 0) {
        cat('Warning: Some covariates are listed as heterogeneously ',
            'distributed, but the \nmodel is type "mnl". \n...the ',
            'estimation will proceed with all covariates modeled as ',
            'fixed parameters.\n', sep='')
    }
}

# ============================================================================
# Check for the start point settings

if (!exists('lowerBound')) {
    printMissingInputWarningMsg('"lowerBound"', '-1')
    lowerBound = -1
}
if (!exists('upperBound')) {
    printMissingInputWarningMsg('"upperBound"', '1')
    upperBound = 1
}
if (!exists('customStartPoints')) {
    printMissingInputWarningMsg('"customStartPoints"', 'NULL')
    customStartPoints = NULL
}
if (!exists('customLowerBound')) {
    printMissingInputWarningMsg('"customLowerBound"', 'NULL')
    customLowerBound = NULL
}
if (!exists('customUpperBound')) {
    printMissingInputWarningMsg('"customUpperBound"', 'NULL')
    customUpperBound = NULL
}

# ============================================================================
# Check for model run settings

if (!exists('runOptimizor')) {
    printMissingInputWarningMsg('"runOptimizor"', 'TRUE')
    runOptimizor = TRUE
}
if (!exists('saveResults')) {
    printMissingInputWarningMsg('"saveResults"', 'TRUE')
    saveResults = TRUE
}
if (!exists('savePath')) {
    missingInputError('"savePath"')
}
