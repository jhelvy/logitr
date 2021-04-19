# ============================================================================
# Functions for running simulations
# ============================================================================

#' Simulate expected shares
#'
#' Returns the expected shares for a set of one or more alternatives based
#' on the results from an estimated model.
#' @keywords logitr simluation
#'
#' @param model The output of a model estimated model using the `logitr()`
#' function.
#' @param alts A data frame of a set of alternatives for which to simulate
#' shares. Each row is an alternative and each column an attribute
#' corresponding to parameter names in the estimated model.
#' @param obsIDName The name of the column that identifies each set of
#' alternatives. Required if simulating results for more than one set of
#' alternatives. Defaults to `NULL` (for a single set of alternatives).
#' @param priceName The name of the parameter that identifies price. Only
#' required for WTP space models. Defaults to `NULL`.
#' @param alpha The sensitivity of the computed confidence interval, e.g. a
#' 90% CI is obtained with `alpha = 0.05`. Defaults to `alpha = 0.025`.
#' @param numDraws The number of draws to use in simulating uncertainty
#' for the computed confidence interval.
#'
#' @return A data frame with the estimated shares for each alternative in
#' `alts`.
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
simulateShares <- function(
  model,
  alts,
  obsIDName = NULL,
  priceName = NULL,
  alpha = 0.025,
  numDraws = 10^4
) {
  alts <- as.data.frame(alts)
  model <- allRunsCheck(model)
  recoded <- recodeData(alts, model$parNames, model$randPars)
  X <- recoded$X
  # Check if model parNames match those from alts
  checkParNames(model, X)
  price <- NA
  getV <- getMnlV_pref
  getVDraws <- getMxlV_pref
  if (model$modelSpace == "wtp") {
    getVDraws <- getMxlV_wtp
    getV <- getMnlV_wtp
    price <- as.matrix(alts[, which(colnames(alts) == priceName)])
  }
  if (is.null(obsIDName)) {
    obsID <- rep(1, nrow(X))
  } else {
    obsID <- as.matrix(alts[obsIDName])
  }
  if (model$modelType == "mxl") {
    return(mxlSimulation(
      alts, model, X, price, obsID, numDraws, alpha, getV, getVDraws))
  } else {
    return(mnlSimulation(
      alts, model, X, price, obsID, numDraws, alpha, getV, getVDraws))
  }
}

checkParNames <- function(model, X) {
  modelParNames <- names(model$parSetup)
  if (model$modelSpace == "wtp") {
    # Drop lambda parameter
    modelParNames <- modelParNames[2:length(modelParNames)]
  }
  dataNames <- colnames(X)
  if (length(setdiff(modelParNames, dataNames)) > 0) {
    modelPars <- paste(modelParNames, collapse = ", ")
    dataPars <- paste(dataNames, collapse = ", ")
    stop(paste0(
      'The coefficient names for the provided model do not correspond to ',
      'variables in `alts`.\n\n',
      'Expect columns:\n\t', modelPars, '\n\n',
      'Encoded column names from provided `alts` object:\n\t', dataPars, '\n\n',
      'If you have a factor variable in alts, check that the factor ',
      'levels match those of the data used to estimate the model.'
    ))
  }
}

mnlSimulation <- function(alts, model, X, price, obsID, numDraws, alpha,
  getV, getVDraws) {
  # Compute mean shares
  V <- getV(stats::coef(model), X, price)
  meanShare <- getMnlLogit(V, obsID)
  # Compute uncertainty with simulation
  betaUncDraws <- getUncertaintyDraws(model, numDraws)
  betaUncDraws <- selectSimDraws(betaUncDraws, model$modelSpace, X)
  VUncDraws <- getVDraws(betaUncDraws, X, price)
  logitUncDraws <- getMxlLogit(VUncDraws, obsID)
  shares <- as.data.frame(t(apply(logitUncDraws, 1, ci, alpha)))
  shares$mean <- as.numeric(meanShare)
  row.names(shares) <- paste("Alt: ", row.names(alts), sep = "")
  colnames(shares) <- c("share_mean", "share_low", "share_high")
  return(shares)
}

mxlSimulation <- function(alts, model, X, price, obsID, numDraws, alpha,
  getV, getVDraws) {
  betaUncDraws <- getUncertaintyDraws(model, numDraws)
  meanShare <- getSimPHat(
    stats::coef(model), model, X, price, obsID, getVDraws)
  logitUncDraws <- matrix(0, nrow = nrow(X), ncol = nrow(betaUncDraws))
  for (i in seq_len(nrow(betaUncDraws))) {
    pars <- betaUncDraws[i, ]
    logitUncDraws[, i] <- getSimPHat(pars, model, X, price, obsID, getVDraws)
  }
  shares <- as.data.frame(t(apply(logitUncDraws, 1, ci, alpha)))
  shares$mean <- meanShare
  row.names(shares) <- paste("Alt: ", row.names(alts), sep = "")
  colnames(shares) <- c("share_mean", "share_low", "share_high")
  return(shares)
}

selectSimDraws <- function(betaDraws, modelSpace, X) {
  betaDraws <- as.data.frame(betaDraws)
  if (modelSpace == "wtp") {
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
    pars, model$parSetup, model$options$numDraws, model$standardDraws)
  colnames(betaDraws) <- names(model$parSetup)
  betaDraws <- selectSimDraws(betaDraws, model$modelSpace, X)
  VDraws <- getVDraws(betaDraws, X, price)
  logitDraws <- getMxlLogit(VDraws, obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(pHat)
}
