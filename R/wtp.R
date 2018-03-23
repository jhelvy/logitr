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
