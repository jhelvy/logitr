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
#' market = subset(yogurt, obsID==42,
#'          select=c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
#' row.names(market) = c('dannon', 'hiland', 'weight', 'yoplait')
#' market
#'
#' # Run the simulation using the estimated preference space MNL model:
#' marketSimulation(mnl.pref, market, alpha=0.025)
marketSimulation.logitr = function(model, market, priceName=NULL, alpha=0.025){
    if (!is.logitr(model)) {
        stop('Model must be estimated using the "logitr" package')
    }
     if (is.logitr.multistart(model)) {
        cat('**Using results for the best model from the multistart**',
            '\n', sep='')
        model = model$bestModel
    }
    if (isMxlModel(model$parSetup)) {
        return(mxlMarketSimulation(model, market, priceName, alpha))
    } else {
        return(mnlMarketSimulation(model, market, priceName, alpha))
    }
}

mnlMarketSimulation = function(model, market, priceName, alpha=0.025) {
    numDraws     = 10^4
    getVUncDraws = getMxlV.pref
    getV         = getMnlV.pref
    attNames     = colnames(market)
    X            = as.matrix(market[attNames])
    price        = NA
    obsID        = rep(1, nrow(X))
    if (model$modelSpace=='wtp') {
        price        = -1*market[,which(colnames(market)==priceName)]
        X            = as.matrix(market[attNames[which(attNames != 'price')]])
        getVUncDraws = getMxlV.wtp
        getV         = getMnlV.wtp
    }
    betaUncDraws  = getUncertaintyDraws(model, numDraws)
    betaUncDraws  = selectSimDraws(betaUncDraws, model, X)
    V             = getV(coef(model)[colnames(betaUncDraws)], X, price)
    meanShare     = getMnlLogit(V, obsID)
    VUncDraws     = getVUncDraws(betaUncDraws, X, price)
    logitUncDraws = getMxlLogit(VUncDraws, obsID)
    shares        = as.data.frame(t(apply(logitUncDraws, 1, ci, alpha)))
    shares$mean   = as.numeric(meanShare)
    row.names(shares) = paste('Alt: ', row.names(market), sep='')
    colnames(shares)  = c('share.mean', 'share.low', 'share.high')
    return(shares)
}

mxlMarketSimulation = function(model, market, priceName, alpha=0.025) {
    numDraws  = 10^4
    getVDraws = getMxlV.pref
    attNames  = colnames(market)
    X         = as.matrix(market[attNames])
    price     = NA
    obsID     = rep(1, nrow(X))
    if (model$modelSpace=='wtp') {
        price     = -1*market[,which(colnames(market)==priceName)]
        X         = as.matrix(market[attNames[which(attNames != 'price')]])
        getVDraws = getMxlV.wtp
    }
    betaUncDraws  = getUncertaintyDraws(model, numDraws)
    meanShare     = getSimPHat(coef(model), model, X, price, obsID, getVDraws)
    logitUncDraws = matrix(0, nrow=nrow(X), ncol=nrow(betaUncDraws))
    for (i in 1:nrow(betaUncDraws)) {
        pars = betaUncDraws[i,]
        logitUncDraws[,i] = getSimPHat(pars, model, X, price, obsID, getVDraws)
    }
    shares      = as.data.frame(t(apply(logitUncDraws, 1, ci, alpha)))
    shares$mean = meanShare
    row.names(shares) = paste('Alt: ', row.names(market), sep='')
    colnames(shares)  = c('share.mean', 'share.low', 'share.high')
    return(shares)
}

selectSimDraws = function(betaDraws, model, X) {
    betaDraws = as.data.frame(betaDraws)
    if (model$modelSpace=='wtp') {
        lambdaDraws = betaDraws['lambda']
        gammaDraws  = betaDraws[colnames(X)]
        betaDraws   = cbind(lambdaDraws, gammaDraws)
    } else {
        betaDraws = betaDraws[colnames(X)]
    }
    return(as.matrix(betaDraws))
}

getSimPHat = function(pars, model, X, price, obsID, getVDraws) {
    betaDraws = makeBetaDraws(pars, model$parSetup, model$options$numDraws,
                model$standardDraws)
    colnames(betaDraws) = names(model$parSetup)
    betaDraws  = selectSimDraws(betaDraws, model, X)
    VDraws     = getVDraws(betaDraws, X, price)
    logitDraws = getMxlLogit(VDraws, obsID)
    pHat       = rowMeans(logitDraws, na.rm=T)
    return(pHat)
}
