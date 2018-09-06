# ============================================================================
# Functions for running simulations
# ============================================================================

#' Returns the expected shares of a specific set of alternatives based
#' on an estimated model.
#'
#' Returns the expected shares of a specific set of alternatives based
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
#' # Create a set of alternatives for which to simulate shares:
#' alts = subset(yogurt, obsID==42,
#'        select=c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
#' row.names(alts) = c('dannon', 'hiland', 'weight', 'yoplait')
#' alts
#'
#' # Run the simulation using the estimated preference space MNL model:
#' simulateShares(mnl.pref, alts, alpha=0.025)
simulateShares.logitr = function(model, alts, priceName=NULL, alpha=0.025){
    model = allRunsCheck(model)
    if (isMxlModel(model$parSetup)) {
        return(mxlSimulation(model, alts, priceName, alpha))
    } else {
        return(mnlSimulation(model, alts, priceName, alpha))
    }
}

mnlSimulation = function(model, alts, priceName, alpha=0.025) {
    numDraws     = 10^4
    getVUncDraws = getMxlV.pref
    getV         = getMnlV.pref
    attNames     = colnames(alts)
    X            = as.matrix(alts[attNames])
    price        = NA
    obsID        = rep(1, nrow(X))
    if (model$modelSpace=='wtp') {
        price        = -1*alts[,which(colnames(alts)==priceName)]
        X            = as.matrix(alts[attNames[which(attNames != 'price')]])
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
    row.names(shares) = paste('Alt: ', row.names(alts), sep='')
    colnames(shares)  = c('share.mean', 'share.low', 'share.high')
    return(shares)
}

mxlSimulation = function(model, alts, priceName, alpha=0.025) {
    numDraws  = 10^4
    getVDraws = getMxlV.pref
    attNames  = colnames(alts)
    X         = as.matrix(alts[attNames])
    price     = NA
    obsID     = rep(1, nrow(X))
    if (model$modelSpace=='wtp') {
        price     = -1*alts[,which(colnames(alts)==priceName)]
        X         = as.matrix(alts[attNames[which(attNames != 'price')]])
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
    row.names(shares) = paste('Alt: ', row.names(alts), sep='')
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
