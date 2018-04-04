# ============================================================================
# Functions for running simulations
# ============================================================================

#' Returns the expected market shares of a specific set of alternatives based
#' on an estimated model.
#'
#' Returns the expected market shares of a specific set of alternatives based
#' on an estimated model.
#' @keywords logitr, simluation
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' data(yogurt)
#'
#' mnl.pref = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Create a market to simulate.
#' market = subset(yogurt, obsID==1,
#'          select=c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
#'
#' # Run the simulation using the estimated preference space MNL model:
#' logitr.simulation(mnl.pref, market, alpha=0.025)
logitr.simulation = function(model, market, priceName=NULL, alpha=0.025) {
    if (!is.logitr(model)) {
        stop('Model must be estimated using the "logitr" package')
    }
     if (is.logitr.multistart(model)) {
        cat('**Using results for the best model from the multistart**',
            '\n', sep='')
        model = model$bestModel
    }
    getVDraws = setVDraws(model, priceName)
    attributeNames = colnames(market)
    X = as.matrix(market[attributeNames])
    price = NA
    if (model$modelSpace=='wtp') {
        price = -1*market[,which(colnames(market)==priceName)]
        X = as.matrix(market[attributeNames[which(attributeNames != 'price')]])
    }
    betaDraws  = getSimulationBetaDraws(model, X, numDraws=10^4)
    VDraws     = getVDraws(betaDraws, X, price)
    logitDraws = getMxlLogit(VDraws, rep(1, nrow(VDraws)))
    shares     = as.data.frame(t(apply(logitDraws, 1, ci, alpha=0.05)))
    row.names(shares) = paste('Alt-', seq(nrow(X)), sep='')
    return(shares)
}

setVDraws = function(model, priceName) {
    if (model$modelSpace == 'pref') {
        getVDraws = getMxlV.pref
    } else if (model$modelSpace == 'wtp') {
        getVDraws = getMxlV.wtp
        if (is.null(priceName)) {
            stop('This is a WTP space model - must provide "priceName"')
        }
    }
    return(getVDraws)
}

getSimulationBetaDraws = function(model, X, numDraws) {
    betaDraws = getUncertaintyDraws(model, numDraws)
    if (model$modelSpace=='wtp') {
        lambdaDraws = betaDraws['lambda']
        gammaDraws  = betaDraws[colnames(X)]
        betaDraws   = cbind(lambdaDraws, gammaDraws)
    } else if (model$modelSpace=='pref'){
        betaDraws = betaDraws[colnames(X)]
    }
    if (length(unique(model$parSetup)) > 1) {
        # This is a MXL model, so include heterogeneity terms for making draws
        betaDraws = betaDraws
    }
    return(as.matrix(betaDraws))
}




