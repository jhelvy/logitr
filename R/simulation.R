# ============================================================================
# Functions for running simulations
# ============================================================================

#' Simulate expected shares from a set of alternatives
#'
#' Returns the expected shares of a specific set of alternatives based
#' on an estimated model.
#' @keywords logitr, simluation
#'
#' @param model The output of a model estimated model using the `logitr()` function.
#' @param alts A data frame of a set of alternatives for which to simulate shares. Each row is an alternative and each column an attribute corresponding to parameter names in the estimated model.
#' @param priceName The name of the parameter that identifies price. Only required for WTP space models. Defaults to `NULL`.
#' @param alpha The sensitivity of the computed confidence interval, e.g. a 90% CI is obtained with `alpha = 0.05`. Defaults to `alpha = 0.025`.
#'
#' @return A data frame with the estimated shares for each alternative in `alts`.
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' library(logitr)
#'
#' mnl_pref <- logitr(
#'   data = yogurt,
#'   choiceName = "choice",
#'   obsIDName = "obsID",
#'   parNames = c("price", "feat", "dannon", "hiland", "yoplait")
#' )
#'
#' # Create a set of alternatives for which to simulate shares:
#' alts <- subset(yogurt, obsID == 42,
#'   select = c("feat", "price", "dannon", "hiland", "yoplait")
#' )
#' row.names(alts) <- c("dannon", "hiland", "weight", "yoplait")
#' alts
#'
#' # Run the simulation using the estimated preference space MNL model:
#' simulateShares(mnl_pref, alts)
simulateShares <- function(model, alts, priceName = NULL,
                           alpha = 0.025) {
  model <- allRunsCheck(model)
  if (isMxlModel(model$parSetup)) {
    return(mxlSimulation(model, alts, priceName, alpha))
  } else {
    return(mnlSimulation(model, alts, priceName, alpha))
  }
}

mnlSimulation <- function(model, alts, priceName, alpha = 0.025) {
  numDraws <- 10^4
  getVUncDraws <- getMxlV.pref
  getV <- getMnlV.pref
  attNames <- colnames(alts)
  X <- as.matrix(alts[attNames])
  price <- NA
  obsID <- rep(1, nrow(X))
  if (model$modelSpace == "wtp") {
    price <- -1 * alts[, which(colnames(alts) == priceName)]
    X <- as.matrix(alts[attNames[which(attNames != "price")]])
    getVUncDraws <- getMxlV.wtp
    getV <- getMnlV.wtp
  }
  betaUncDraws <- getUncertaintyDraws(model, numDraws)
  betaUncDraws <- selectSimDraws(betaUncDraws, model, X)
  V <- getV(stats::coef(model)[colnames(betaUncDraws)], X, price)
  meanShare <- getMnlLogit(V, obsID)
  VUncDraws <- getVUncDraws(betaUncDraws, X, price)
  logitUncDraws <- getMxlLogit(VUncDraws, obsID)
  shares <- as.data.frame(t(apply(logitUncDraws, 1, ci, alpha)))
  shares$mean <- as.numeric(meanShare)
  row.names(shares) <- paste("Alt: ", row.names(alts), sep = "")
  colnames(shares) <- c("share_mean", "share_low", "share_high")
  return(shares)
}

mxlSimulation <- function(model, alts, priceName, alpha = 0.025) {
  numDraws <- 10^4
  getVDraws <- getMxlV.pref
  attNames <- colnames(alts)
  X <- as.matrix(alts[attNames])
  price <- NA
  obsID <- rep(1, nrow(X))
  if (model$modelSpace == "wtp") {
    price <- -1 * alts[, which(colnames(alts) == priceName)]
    X <- as.matrix(alts[attNames[which(attNames != "price")]])
    getVDraws <- getMxlV.wtp
  }
  betaUncDraws <- getUncertaintyDraws(model, numDraws)
  meanShare <- getSimPHat(stats::coef(model), model, X, price, obsID, getVDraws)
  logitUncDraws <- matrix(0, nrow = nrow(X), ncol = nrow(betaUncDraws))
  for (i in 1:nrow(betaUncDraws)) {
    pars <- betaUncDraws[i, ]
    logitUncDraws[, i] <- getSimPHat(pars, model, X, price, obsID, getVDraws)
  }
  shares <- as.data.frame(t(apply(logitUncDraws, 1, ci, alpha)))
  shares$mean <- meanShare
  row.names(shares) <- paste("Alt: ", row.names(alts), sep = "")
  colnames(shares) <- c("share_mean", "share_low", "share_high")
  return(shares)
}

selectSimDraws <- function(betaDraws, model, X) {
  betaDraws <- as.data.frame(betaDraws)
  if (model$modelSpace == "wtp") {
    lambdaDraws <- betaDraws["lambda"]
    gammaDraws <- betaDraws[colnames(X)]
    betaDraws <- cbind(lambdaDraws, gammaDraws)
  } else {
    betaDraws <- betaDraws[colnames(X)]
  }
  return(as.matrix(betaDraws))
}

getSimPHat <- function(pars, model, X, price, obsID, getVDraws) {
  betaDraws <- makeBetaDraws(
    pars, model$parSetup, model$options$numDraws,
    model$standardDraws
  )
  colnames(betaDraws) <- names(model$parSetup)
  betaDraws <- selectSimDraws(betaDraws, model, X)
  VDraws <- getVDraws(betaDraws, X, price)
  logitDraws <- getMxlLogit(VDraws, obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(pHat)
}
