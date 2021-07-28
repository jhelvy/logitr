# ============================================================================
# Functions for predicting choices and choice probabilities
# ============================================================================

#' Predict choices
#'
#' Returns the expected choices for a set of one or more alternatives based
#' on the results from an estimated model.
#' @keywords logitr simulation predict choice
#'
#' @param model The output of a model estimated model using the `logitr()`
#' function.
#' Include if you want to compare true choices from actual observations (e.g.
#' hold outs) to the predicted choices.
#' @param alts A data frame of a set of alternatives for which to predict
#' choices. Each row is an alternative and each column an attribute
#' corresponding to parameter names in the estimated model.
#' @param altID The name of the column that identifies each alternative
#' in each set of alternatives.
#' @param obsID The name of the column that identifies each set of
#' alternatives. Required if predicting results for more than one set of
#' alternatives. Defaults to `NULL` (for a single set of alternatives).
#'
#' @return A data frame with the predicted choices for each alternative in
#' `alts`.
#' @export
#' @examples
#' library(logitr)
#'
#' # Estimate a preference space model
#' mnl_pref <- logitr(
#'   data   = yogurt,
#'   choice = "choice",
#'   obsID  = "obsID",
#'   pars   = c("price", "feat", "brand")
#' )
#'
#' # You can predict choices for any set of alternative, such as hold out
#' # samples or within-sample. For this example, choices will be predicted for
#' # the full yogurt data set, which was used to estimate the model.
#'
#' # Predict choices using the estimated preference space MNL model
#' choices <- predictChoices(
#'   model = mnl_pref,
#'   alts  = yogurt,
#'   altID = "alt",
#'   obsID = "obsID"
#' )
#'
#' head(choices)
#'
#' # Compute the accuracy
#' chosen <-  subset(choices, choice == 1)
#' chosen$correct <- chosen$choice == chosen$choice_predict
#' sum(chosen$correct) / nrow(chosen) # % correctly predicted
predictChoices <- function(model, alts, altID, obsID = NULL) {
  probs <- predictProbs(model, alts, altID, obsID, computeCI = FALSE)
  if (is.null(obsID)) {
    obsID <- "obsID"
  }
  choices <- split(probs, probs[obsID])
  choices <- lapply(choices, simChoice)
  result <- do.call(rbind, choices)
  result <- result['choice_predict']
  return(cbind(alts, result))
}

simChoice <- function(df) {
  choices <- seq_len(nrow(df))
  result <- 0*choices
  result[sample(x = choices, size = 1, prob = df$prob_mean)] <- 1
  df$choice_predict <- result
  return(df)
}

#' Predict expected choice probabilities
#'
#' Returns the expected choice probabilities for a single set or multiple sets
#' of alternatives based on the results from an estimated model.
#' @keywords logitr simulation probabilities predict
#'
#' @param model The output of a model estimated model using the `logitr()`
#' function.
#' @param alts A data frame of a set of alternatives for which to predict
#' choice probabilities. Each row is an alternative and each column an
#' attribute corresponding to parameter names in the estimated model.
#' @param altID The name of the column that identifies each alternative
#' in each set of alternatives.
#' @param obsID The name of the column that identifies each set of
#' alternatives. Required if predicting results for more than one set of
#' alternatives. Defaults to `NULL` (for a single set of alternatives).
#' @param computeCI Should a confidence interval be computed?
#' Defaults to `TRUE`.
#' @param alpha The sensitivity of the computed confidence interval.
#' Defaults to `alpha = 0.025`, reflecting a 95% CI.
#' @param numDraws The number of draws to use in simulating uncertainty
#' for the computed confidence interval.
#'
#' @return A data frame with the estimated choice probabilities for each
#' alternative in `alts`.
#' @export
#' @examples
#' library(logitr)
#'
#' # Estimate a preference space model
#' mnl_pref <- logitr(
#'   data   = yogurt,
#'   choice = "choice",
#'   obsID  = "obsID",
#'   pars   = c("price", "feat", "brand")
#' )
#'
#' # Create a set of alternatives for which to predict choice probabilities.
#' # Each row is an alternative and each column an attribute. In this example,
#' # two of the choice observations from the yogurt dataset are used
#' alts <- subset(
#'     yogurt, obsID %in% c(42, 13),
#'     select = c('obsID', 'alt', 'price', 'feat', 'brand'))
#'
#' alts
#'
#' # Predict choice probabilities using the estimated preference space MNL
#' # model
#' predictProbs(mnl_pref, alts, altID = "alt", obsID = "obsID")
predictProbs <- function(
  model,
  alts,
  altID,
  obsID     = NULL,
  computeCI = TRUE,
  alpha     = 0.025,
  numDraws  = 10^4
) {
  predictInputsCheck(model, alts, altID, obsID)
  alts <- as.data.frame(alts)
  recoded <- recodeData(alts, model$inputs$pars, model$inputs$randPars)
  X <- recoded$X
  # Check if model pars match those from alts
  checkPars(model, X)
  price <- NA
  getV <- getMnlV_pref
  getVDraws <- getMxlV_pref
  if (model$inputs$modelSpace == "wtp") {
    getVDraws <- getMxlV_wtp
    getV <- getMnlV_wtp
    price <- as.matrix(alts[, which(colnames(alts) == model$inputs$price)])
  }
  altIDName <- altID
  obsIDName <- obsID
  altID <- alts[,altID]
  if (is.null(obsID)) {
    obsID <- rep(1, nrow(X))
    obsIDName <- "obsID"
  } else {
    obsID <- alts[,obsIDName]
  }
  if (model$modelType == "mxl") {
    return(
      mxlSimulation(
        alts, model, X, price, altID, obsID, altIDName, obsIDName, numDraws,
        alpha, getV, getVDraws, computeCI))
  } else {
    return(
      mnlSimulation(
        alts, model, X, price, altID, obsID, altIDName, obsIDName, numDraws,
        alpha, getV, getVDraws, computeCI))
  }
}

checkPars <- function(model, X) {
  modelPars <- names(model$parSetup)
  if (model$inputs$modelSpace == "wtp") {
    # Drop lambda parameter
    modelPars <- modelPars[2:length(modelPars)]
  }
  dataNames <- colnames(X)
  if (length(setdiff(modelPars, dataNames)) > 0) {
    modelPars <- paste(modelPars, collapse = ", ")
    dataPars <- paste(dataNames, collapse = ", ")
    stop(paste0(
      'The coefficient names for the provided model do not correspond to ',
      'variables in "alts".\n\n',
      'Expect columns:\n\t', modelPars, '\n\n',
      'Encoded column names from provided `alts` object:\n\t', dataPars, '\n\n',
      'If you have a factor variable in "alts", check that the factor ',
      'levels match those of the data used to estimate the model.'
    ))
  }
}

mnlSimulation <- function(
  alts, model, X, price, altID, obsID, altIDName, obsIDName, numDraws, alpha,
  getV, getVDraws, computeCI) {
  # Compute mean probs
  V <- getV(stats::coef(model), X, price)
  meanProb <- getMnlLogit(V, obsID)
  if (computeCI == FALSE) {
    return(summarizeMeanProbs(meanProb, altID, obsID, altIDName, obsIDName))
  }
  # Compute uncertainty with simulation
  betaUncDraws <- getUncertaintyDraws(model, numDraws)
  betaUncDraws <- selectSimDraws(betaUncDraws, model$inputs$modelSpace, X)
  VUncDraws <- getVDraws(betaUncDraws, X, price)
  logitUncDraws <- getMxlLogit(VUncDraws, obsID)
  return(summarizeUncProbs(
    meanProb, logitUncDraws, altID, obsID, altIDName, obsIDName, alpha))
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

mxlSimulation <- function(
  alts, model, X, price, altID, obsID, altIDName, obsIDName, numDraws, alpha,
  getV, getVDraws, computeCI) {
  # Compute mean probs
  meanProb <- getSimPHat(
    stats::coef(model), model, X, price, obsID, getVDraws)
  if (computeCI == FALSE) {
    return(summarizeMeanProbs(meanProb, altID, obsID, altIDName, obsIDName))
  }
  # Compute uncertainty with simulation
  betaUncDraws <- getUncertaintyDraws(model, numDraws)
  logitUncDraws <- matrix(0, nrow = nrow(X), ncol = nrow(betaUncDraws))
  for (i in seq_len(nrow(betaUncDraws))) {
    pars <- betaUncDraws[i, ]
    logitUncDraws[, i] <- getSimPHat(pars, model, X, price, obsID, getVDraws)
  }
  return(summarizeUncProbs(
    meanProb, logitUncDraws, altID, obsID, altIDName, obsIDName, alpha))
}

getSimPHat <- function(pars, model, X, price, obsID, getVDraws) {
  numDraws <- model$inputs$numDraws
  parSetup <- model$parSetup
  parIDs <- model$parIDs
  standardDraws <- getStandardDraws(parIDs, numDraws)
  betaDraws <- makeBetaDraws(pars, parIDs, numDraws, standardDraws)
  colnames(betaDraws) <- names(parSetup)
  betaDraws <- selectSimDraws(betaDraws, model$inputs$modelSpace, X)
  VDraws <- getVDraws(betaDraws, X, price)
  logitDraws <- getMxlLogit(VDraws, obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(pHat)
}

summarizeMeanProbs <- function(meanProb, altID, obsID, altIDName, obsIDName) {
  probs <- as.data.frame(meanProb)
  colnames(probs) <- "prob_mean"
  probs[altIDName] <- altID
  probs[obsIDName] <- obsID
  return(probs[c(obsIDName, altIDName, "prob_mean")])
}

summarizeUncProbs <- function(
  meanProb, logitUncDraws, altID, obsID, altIDName, obsIDName, alpha) {
  probs <- as.data.frame(t(apply(logitUncDraws, 1, ci, alpha)))
  probs$mean <- as.numeric(meanProb)
  colnames(probs) <- paste0("prob_", colnames(probs))
  names <- c(obsIDName, altIDName, colnames(probs))
  probs[altIDName] <- altID
  probs[obsIDName] <- obsID
  return(probs[names])
}

#' Simulate expected shares
#'
#' This function has been depreciated since logitr version 0.1.4. Use
#' `predictProbs()` instead.
#' @keywords logitr simulation
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
#' @param computeCI Should a confidence interval be computed?
#' Defaults to `TRUE`.
#' @param alpha The sensitivity of the computed confidence interval.
#' Defaults to `alpha = 0.025`, reflecting a 95% CI.
#' @param numDraws The number of draws to use in simulating uncertainty
#' for the computed confidence interval.
#'
#' @return A data frame with the estimated shares for each alternative in
#' `alts`.
#' @export
simulateShares <- function(
  model,
  alts,
  obsIDName = NULL,
  priceName = NULL,
  computeCI = TRUE,
  alpha = 0.025,
  numDraws = 10^4
) {
    # v0.1.4
    .Deprecated("predictProbs")
}
