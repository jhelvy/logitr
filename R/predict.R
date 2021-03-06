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
#' @param altIDName The name of the column that identifies each alternative
#' in each set of alternatives.
#' @param obsIDName The name of the column that identifies each set of
#' alternatives. Required if predicting results for more than one set of
#' alternatives. Defaults to `NULL` (for a single set of alternatives).
#'
#' @return A data frame with the predicted choices for each alternative in
#' `alts`.
#' @export
#' @examples
#' \dontrun{
#' # Run a MNL model in the Preference Space:
#' library(logitr)
#'
#' mnl_pref <- logitr(
#'   data       = yogurt,
#'   choiceName = "choice",
#'   obsIDName  = "obsID",
#'   parNames   = c("price", "feat", "brand")
#' )
#'
#' # You can predict choices for any set of alternative, such as hold out
#' # samples or within-sample. For this example I will predict choices on
#' # the full yogurt data set, which was used to estimate the model.
#'
#' # Run the simulation using the preference space MNL model:
#' choices_mnl_pref <- predictChoices(
#'   model     = mnl_pref,
#'   alts      = yogurt,
#'   altIDName = "alt",
#'   obsIDName = "obsID"
#' )
#'
#' head(choices_mnl_pref)
#'
#' # Compute the accuracy
#' chosen <-  subset(choices, choice == 1)
#' chosen$correct <- chosen$choice == chosen$choice_predict
#' sum(chosen$correct) / nrow(chosen)
#' }
predictChoices <- function(
  model,
  alts,
  altIDName,
  obsIDName = NULL
) {
  probs <- predictProbs(model, alts, altIDName, obsIDName, computeCI = FALSE)
  if (is.null(obsIDName)) {
    obsIDName <- "obsID"
  }
  choices <- split(probs, probs[obsIDName])
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
#' @param altIDName The name of the column that identifies each alternative
#' in each set of alternatives.
#' @param obsIDName The name of the column that identifies each set of
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
#' \dontrun{
#' # Run a MNL model in the Preference Space:
#' library(logitr)
#'
#' mnl_pref <- logitr(
#'   data = yogurt,
#'   choiceName = "choice",
#'   obsIDName = "obsID",
#'   parNames = c("price", "feat", "brand")
#' )
#'
#' # Create a set of alternatives for which to predict choice probabilities.
#' # Each row is an alternative and each column an attribute.
#' # In this example, I just use two of the choice observations from the
#' # yogurt dataset:
#' alts <- subset(
#'     yogurt, obsID %in% c(42, 13),
#'     select = c('obsID', 'alt', 'price', 'feat', 'brand'))
#'
#' alts
#'
#' # Predict choice probabilities using the estimated preference space MNL
#' # model:
#' predictProbs(mnl_pref, alts, altIDName = "alt", obsIDName = "obsID")
#' }
predictProbs <- function(
  model,
  alts,
  altIDName,
  obsIDName = NULL,
  computeCI = TRUE,
  alpha = 0.025,
  numDraws = 10^4
) {
  predictInputsCheck(model, alts, altIDName, obsIDName)
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
    price <- as.matrix(alts[, which(colnames(alts) == model$priceName)])
  }
  altID <- alts[,altIDName]
  if (is.null(obsIDName)) {
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
  betaUncDraws <- selectSimDraws(betaUncDraws, model$modelSpace, X)
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
  betaDraws <- makeBetaDraws(
    pars, model$parSetup, model$options$numDraws, model$standardDraws)
  colnames(betaDraws) <- names(model$parSetup)
  betaDraws <- selectSimDraws(betaDraws, model$modelSpace, X)
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
