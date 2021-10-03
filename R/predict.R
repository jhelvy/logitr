# ============================================================================
# Functions for predicting choices and choice probabilities
# ============================================================================

#' Predict probabilities and / or choices
#'
#' This method is used for computing predicted probabilities and / or choices
#' for either the data used for model estimation or a new data set consisting
#' of a single or multiple sets of alternatives.
#' @keywords logitr probabilities predict
#'
#' @param object is an object of class `logitr`.
#' @param newdata a `data.frame`. Each row is an alternative and each column an
#' attribute corresponding to parameter names in the estimated model. Defaults
#' to `NULL`, in which case predictions are made on the original data used to
#' estimate the model.
#' @param obsID The name of the column that identifies each set of
#' alternatives in the data. Required if predicting results for more than one
#' set of alternatives. Defaults to `NULL`, in which case the value for `obsID`
#' from the estimated `object` is used.
#' @param output A character vector defining what to predict: `probs` for
#' probabilities, `choices` for choices. If you want both outputs, use
#' `c("probs", "choices")`. Choices are predicted randomly according to the
#' predicted probabilities. Defaults to `"probs"`.
#' @param returnData If `TRUE` the data is also returned, otherwise only the
#' predicted values ("probs" and / or  "choices") are returned.
#' Defaults to `TRUE`.
#' @param computeCI Should a confidence interval be computed for predicted
#' probabilities? Defaults to `FALSE`.
#' @param ci The sensitivity of the computed confidence interval (CI).
#' Defaults to `ci = 0.95`, reflecting a 95% CI.
#' @param numDraws The number of draws to use in simulating uncertainty
#' for the computed CI. Defaults to 10^4.
#' @return A data frame of predicted probabilities and / or choices.
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
#' newdata <- subset(
#'     yogurt, obsID %in% c(42, 13),
#'     select = c('obsID', 'alt', 'price', 'feat', 'brand'))
#' newdata
#'
#' # Predict choice probabilities using the estimated model
#' predict(mnl_pref, newdata = newdata, obsID = "obsID")
#'
#' # Predict choice probabilities and choices using the estimated model
#' predict(
#'     mnl_pref,
#'     newdata = newdata,
#'     obsID = "obsID",
#'     output = c("probs", "choices")
#' )
predict.logitr <- function(
  object,
  newdata    = NULL,
  obsID      = NULL,
  output     = "probs",
  returnData = FALSE,
  computeCI  = FALSE,
  ci         = 0.95,
  numDraws   = 10^4
) {
  d <- object$data
  # If no newdata is provided, use the data from the estimated object
  if (is.null(newdata)) {
    data <- list(X = d$X, price = d$price, obsID = d$obsID)
  } else {
    data <- formatNewData(object, newdata, obsID)
  }
  getV <- getMnlV_pref
  getVDraws <- getMxlV_pref
  if (model$inputs$modelSpace == "wtp") {
    getVDraws <- getMxlV_wtp
    getV <- getMnlV_wtp
  }
  if (model$modelType == "mxl") {
    return(
      mxlSimulation(
        object, data, obsID, numDraws, ci, getV, getVDraws, computeCI
    ))
  } else {
    return(
      mnlSimulation(
        object, data, obsID, numDraws, ci, getV, getVDraws, computeCI
    ))
  }
}

formatNewData <- function(object, newdata, obsID) {
  predictInputsCheck(object, newdata, obsID)
  inputs <- object$inputs
  newdata <- as.data.frame(newdata) # tibbles break things
  recoded <- recodeData(newdata, inputs$pars, inputs$randPars)
  X <- recoded$X
  predictParCheck(object, X) # Check if model pars match those from newdata
  price <- NA
  if (inputs$modelSpace == "wtp") {
    price <- as.matrix(newdata[, which(colnames(newdata) == inputs$price)])
  }
  if (is.null(obsID)) {
    obsIDName <- inputs$obsID # Use obsID from estimated object
  } else {
    obsIDName <- obsID
  }
  obsID <- newdata[, obsIDName]
  return(list(X = X, price = price, obsID = obsID))
}






mnlSimulation <- function(
  alts, model, price, X, altID, obsID, altIDName, obsIDName, numDraws, ci,
  getV, getVDraws, computeCI
) {
  # Compute mean probs
  V <- getV(stats::coef(model), X, price)
  meanProb <- predictLogit(V, obsID)
  if (computeCI == FALSE) {
    return(summarizeMeanProbs(meanProb, altID, obsID, altIDName, obsIDName))
  }
  # Compute uncertainty with simulation
  betaUncDraws <- getUncertaintyDraws(model, numDraws)
  betaUncDraws <- selectSimDraws(betaUncDraws, model$inputs$modelSpace, X)
  VUncDraws <- getVDraws(betaUncDraws, X, price)
  logitUncDraws <- predictLogit(VUncDraws, obsID)
  return(summarizeUncProbs(
    meanProb, logitUncDraws, altID, obsID, altIDName, obsIDName, ci))
}

mxlSimulation <- function(
  alts, model, price, X, altID, obsID, altIDName, obsIDName, numDraws, ci,
  getV, getVDraws, computeCI
) {
  # Compute mean probs
  meanProb <- getSimPHat(stats::coef(model), model, X, price, obsID, getVDraws)
  if (computeCI == FALSE) {
    return(summarizeMeanProbs(meanProb, altID, obsID, altIDName, obsIDName))
  }
  # Compute uncertainty with simulation
  betaUncDraws <- getUncertaintyDraws(model, numDraws)
  logitUncDraws <- matrix(0, nrow = nrow(X), ncol = nrow(betaUncDraws))
  for (i in seq_len(nrow(betaUncDraws))) {
    pars <- betaUncDraws[i, ]
    logitUncDraws[, i] <- getSimPHat(
      pars, model, X, price, obsID, getVDraws)
  }
  return(summarizeUncProbs(
    meanProb, logitUncDraws, altID, obsID, altIDName, obsIDName, ci))
}

predictLogit <- function(V, obsID) {
  expV <- exp(V)
  sumExpV <- rowsum(expV, group = obsID, reorder = FALSE)
  reps <- table(obsID)
  return(expV / sumExpV[rep(seq_along(reps), reps),])
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
  logitDraws <- predictLogit(VDraws, obsID)
  return(rowMeans(logitDraws, na.rm = T))
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

summarizeMeanProbs <- function(meanProb, altID, obsID, altIDName, obsIDName) {
  probs <- as.data.frame(meanProb)
  colnames(probs) <- "prob_mean"
  probs[altIDName] <- altID
  probs[obsIDName] <- obsID
  return(probs[c(obsIDName, altIDName, "prob_mean")])
}

summarizeUncProbs <- function(
  meanProb, logitUncDraws, altID, obsID, altIDName, obsIDName, ci
) {
  probs <- as.data.frame(t(apply(logitUncDraws, 1, getCI, ci)))
  probs$mean <- as.numeric(meanProb)
  colnames(probs) <- paste0("prob_", colnames(probs))
  names <- c(obsIDName, altIDName, colnames(probs))
  probs[altIDName] <- altID
  probs[obsIDName] <- obsID
  return(probs[names])
}

# Returns a confidence interval from a vector of data
getCI <- function(data, ci = 0.95) {
  alpha <- (1 - ci)/2
  B <- mean(data, na.rm = T)
  L <- stats::quantile(data, alpha, na.rm = T)
  U <- stats::quantile(data, 1 - alpha, na.rm = T)
  ests <- c(B, L, U)
  names(ests) <- c("mean", "low", "high")
  return(ests)
}

simChoice <- function(df) {
  choices <- seq_len(nrow(df))
  result <- 0*choices
  result[sample(x = choices, size = 1, prob = df$prob_mean)] <- 1
  df$choice_predict <- result
  return(df)
}












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
    # v0.3.2
    .Deprecated("predict")
}

  # probs <- predictProbs(model, alts, altID, obsID, computeCI = FALSE)
  # if (is.null(obsID)) {
  #   obsID <- "obsID"
  # }
  # choices <- split(probs, probs[obsID])
  # choices <- lapply(choices, simChoice)
  # result <- do.call(rbind, choices)
  # result <- result['choice_predict']
  # return(cbind(alts, result))

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
#' @param ci The sensitivity of the computed confidence interval (CI).
#' Defaults to `ci = 0.95`, reflecting a 95% CI.
#' @param numDraws The number of draws to use in simulating uncertainty
#' for the computed confidence interval.
#' @param alpha The sensitivity of the computed confidence interval.
#' No longer used as of v0.2.7 - if provided, a warning is shown and `ci`
#' is computed from `alpha`.
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
  ci        = 0.95,
  numDraws  = 10^4,
  alpha
) {
    # v0.3.2
    .Deprecated("predict")
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
