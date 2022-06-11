# ============================================================================
# Functions for predicting probabilities and outcomes
# ============================================================================

#' Predict probabilities and / or outcomes
#'
#' This method is used for computing predicted probabilities and / or outcomes
#' for either the data used for model estimation or a new data set consisting
#' of a single or multiple sets of alternatives.
#' @keywords logitr probabilities predict
#'
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param newdata a `data.frame`. Each row is an alternative and each column an
#' attribute corresponding to parameter names in the estimated model. Defaults
#' to `NULL`, in which case predictions are made on the original data used to
#' estimate the model.
#' @param obsID The name of the column that identifies each set of
#' alternatives in the data. Required if newdata != NULL. Defaults to `NULL`,
#' in which case the value for `obsID` from the data in `object` is used.
#' @param price The name of the column that identifies the price variable.
#' Required if the `object` is a WTP space model and if newdata != NULL.
#' Defaults to `NULL`.
#' @param type A character vector defining what to predict: `prob` for
#' probabilities, `outcomes` for outcomes. If you want both outputs, use
#' `c("prob", "outcome")`. Outcomes are predicted randomly according to the
#' predicted probabilities. Defaults to `"prob"`.
#' @param returnData If `TRUE` the data is also returned, otherwise only the
#' predicted values ("prob" and / or  "outcome") are returned.
#' Defaults to `FALSE`.
#' @param ci If a confidence interval (CI) for the predicted probabilities is
#' desired, set `ci` to a number between 0 and 1 to define the CI sensitivity.
#' For example, `ci = 0.95` will return a 95% CI. Defaults to `NULL`, in which
#' case no CI is computed.
#' @param numDrawsCI The number of draws to use in simulating uncertainty
#' for the computed CI. Defaults to 10^3.
#' @param ... further arguments.
#' @return A data frame of predicted probabilities and / or outcomes.
#' @export
#' @examples
#' library(logitr)
#'
#' # Estimate a preference space model
#' mnl_pref <- logitr(
#'   data    = yogurt,
#'   outcome = "choice",
#'   obsID   = "obsID",
#'   pars    = c("price", "feat", "brand")
#' )
#'
#' # Predict probabilities and / or outcomes
#'
#' # Predict probabilities for each alternative in the model data
#' probs <- predict(mnl_pref)
#' head(probs)
#'
#' # Create a set of alternatives for which to make predictions.
#' # Each row is an alternative and each column an attribute.
#' data <- subset(
#'     yogurt, obsID %in% c(42, 13),
#'     select = c('obsID', 'alt', 'price', 'feat', 'brand'))
#' data
#'
#' # Predict probabilities using the estimated model
#' predict(mnl_pref, newdata = data, obsID = "obsID")
#'
#' # Predict outcomes
#' predict(mnl_pref, newdata = data, obsID = "obsID", type = "outcome")
#'
#' # Predict outcomes and probabilities
#' predict(mnl_pref, newdata = data, obsID = "obsID", type = c("prob", "outcome"))
predict.logitr <- function(
  object,
  newdata    = NULL,
  obsID      = NULL,
  price      = NULL,
  type       = "prob",
  returnData = FALSE,
  ci         = NULL,
  numDrawsCI = 10^3,
  ...
) {
  predictInputsCheck(object, newdata, obsID, price, type, ci)
  d <- object$data
  # If no newdata is provided, use the data from the estimated object
  if (is.null(newdata)) {
    data <- list(X = d$X, price = d$price, obsID = d$obsID, outcome = d$outcome)
    obsID <- object$inputs$obsID
  } else {
    data <- formatNewData(object, newdata, obsID)
  }
  getV <- getMnlV_pref
  getVDraws <- getMxlV_pref
  if (object$inputs$modelSpace == "wtp") {
    getVDraws <- getMxlV_wtp
    getV <- getMnlV_wtp
  }

  # Decide what to predict
  predict_outcome <- FALSE
  predict_prob <- TRUE
  if ("outcome" %in% type) {
    predict_outcome <- TRUE
  }
  if (! ("prob" %in% type)) {
    predict_prob <- FALSE
    ci <- NULL
  }

  if (object$modelType == "mxl") {
    result <- getMxlProbs(object, data, obsID, ci, numDrawsCI, getV, getVDraws)
  } else {
    result <- getMnlProbs(object, data, obsID, ci, numDrawsCI, getV, getVDraws)
  }
  if (predict_outcome) {
    result <- addOutcomes(result, obsID)
  }
  if (!predict_prob) { # Remove predicted_prob if "prob" not in type
    result$predicted_prob <- NULL
  }
  if (returnData) {
    result <- addData(object, result, data, newdata)
  }
  return(result)
}

formatNewData <- function(object, newdata, obsID) {
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

getMnlProbs <- function(object, data, obsID, ci, numDrawsCI, getV, getVDraws) {
  obsIDName <- obsID
  # Compute mean probs
  coefs <- stats::coef(object)
  V <- getV(coefs, data$X, data$price)
  probs_mean <- predictLogit(V, data$obsID)
  if (is.null(ci)) {
    probs <- formatProbsMean(probs_mean, data$obsID, obsIDName)
  } else {
    # Compute uncertainty with simulation
    n <- object$n
    n$draws <- numDrawsCI
    betaUncDraws <- getUncertaintyDraws(object, numDrawsCI)
    betaUncDraws <- selectDraws(betaUncDraws, object$inputs$modelSpace, data$X)
    VUncDraws <- getVDraws(betaUncDraws, data$X, data$price, n)
    logitUncDraws <- predictLogit(VUncDraws, data$obsID)
    probs <- formatProbsUnc(
      probs_mean, logitUncDraws, data$obsID, obsIDName, ci)
  }
  return(probs)
}

getMxlProbs <- function(object, data, obsID, ci, numDrawsCI, getV, getVDraws) {
  obsIDName <- obsID
  # Compute mean probs
  coefs <- stats::coef(object)
  probs_mean <- predictLogitDraws(coefs, object, data, getVDraws)
  if (is.null(ci)) {
    probs <- formatProbsMean(probs_mean, data$obsID, obsIDName)
  } else {
    # Compute uncertainty with simulation
    betaUncDraws <- getUncertaintyDraws(object, numDrawsCI)
    logitUncDraws <- apply(
      betaUncDraws, 1, predictLogitDraws,
      object, data, getVDraws)
    probs <- formatProbsUnc(probs_mean, logitUncDraws, data$obsID, obsIDName, ci)
  }
  return(probs)
}

# Reorders the columns depending on if the model is a WTP space model or not
# and returns only the draws for the vars in X
selectDraws <- function(betaDraws, modelSpace, X) {
  names <- colnames(betaDraws)
  gammaDraws <- betaDraws[,which(names %in% colnames(X))]
  if (modelSpace == "wtp") {
    lambdaDraws <- betaDraws[,which(names == "lambda")]
    return(as.matrix(cbind(lambdaDraws, gammaDraws)))
  }
  return(as.matrix(gammaDraws))
}

predictLogit <- function(V, obsID) {
  expV <- exp(V)
  sumExpV <- rowsum(expV, group = obsID, reorder = FALSE)
  reps <- table(obsID)
  return(expV / sumExpV[rep(seq_along(reps), reps),])
}

predictLogitDraws <- function(coefs, object, data, getVDraws) {
  modelSpace <- object$inputs$modelSpace
  correlation <- object$inputs$correlation
  betaDraws <- makeBetaDraws(
      coefs, object$parIDs, object$n, object$standardDraws, correlation
  )
  colnames(betaDraws) <- names(object$parSetup)
  betaDraws <- selectDraws(betaDraws, modelSpace, data$X)
  VDraws <- getVDraws(betaDraws, data$X, data$price, object$n)
  logitDraws <- predictLogit(VDraws, data$obsID)
  return(rowMeans(logitDraws, na.rm = TRUE))
}

# Functions for formatting the probabilities

formatProbsMean <- function(probs_mean, obsID, obsIDName) {
  probs <- as.data.frame(probs_mean)
  colnames(probs) <- "predicted_prob"
  probs[obsIDName] <- obsID
  return(probs[c(obsIDName, "predicted_prob")])
}

formatProbsUnc <- function(probs_mean, logitUncDraws, obsID, obsIDName, ci) {
  probs_bounds <- getCI(logitUncDraws, ci)
  colnames(probs_bounds) <- paste0("predicted_prob_", colnames(probs_bounds))
  probs <- cbind(predicted_prob = probs_mean, probs_bounds)
  names <- c(obsIDName, colnames(probs))
  probs[obsIDName] <- obsID
  return(probs[names])
}

getCI <- function(draws, ci = 0.95) {
  alpha <- (1 - ci)/2
  probs <- c(alpha, 1 - alpha)
  result <- apply(draws, 1, fquantile, probs = probs)
  result <- as.data.frame(t(result))
  names(result) <- c("lower", "upper")
  return(result)
}

#' Predict probabilities and / or outcomes
#'
#' This function is a faster implementation of the "type 7" `quantile()`
#' algorithm and is modified from this gist:
#' https://gist.github.com/sikli/f1775feb9736073cefee97ec81f6b193
#' It returns sample quantiles corresponding to the given probabilities.
#' The smallest observation corresponds to a probability of 0 and the largest
#' to a probability of 1. For speed, output quantile names are removed as are
#' error handling such as checking if x are factors, or if probs lie outside
#' the `[0,1]` range.
#' @param x numeric vector whose sample quantiles are wanted. `NA` and `NaN`
#' values are not allowed in numeric vectors unless `na.rm` is `TRUE`.
#' @param probs numeric vector of probabilities with values in `[0,1]`.
#' (Values up to `2e-14` outside that range are accepted and moved to the
#' nearby endpoint.)
#' @param na.rm logical; if `TRUE`, any `NA` and `NaN`'s are removed from `x`
#' before the quantiles are computed.
#' @return A vector of length `length(probs)` is returned;
#' @export
#' @examples
#' library(logitr)
#'
fquantile <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  index <- 1 + (n - 1) * probs
  lo <- floor(index)
  hi <- ceiling(index)
  x  <- sort(x, partial = unique(c(lo, hi)))
  qs <- x[lo]
  i  <- 1:length(probs)
  h  <- index - lo
  qs <- (1 - h) * qs + h * x[hi]
  return(qs)
}

addOutcomes <- function(probs, obsID) {
  outcomes <- split(probs, probs[obsID])
  outcomes <- lapply(outcomes, simOutcome)
  probs$predicted_outcome <- do.call(rbind, outcomes)$predicted_outcome
  return(probs)
}

# Simulate outcomes based on probabilities
simOutcome <- function(df) {
  outcomes <- seq_len(nrow(df))
  result <- 0*outcomes
  result[sample(x = outcomes, size = 1, prob = df$predicted_prob)] <- 1
  df$predicted_outcome <- result
  return(df)
}

addData <- function(object, result, data, newdata) {
  if (!is.null(newdata)) {
      df <- newdata[which(! names(newdata) %in% names(result))]
  } else {
    df <- as.data.frame(data$X)
    if (object$inputs$modelSpace == "wtp") {
      df <- cbind(df, price = data$price)
    }
    if (!is.null(data$outcome)) {
      df <- cbind(df, outcome = data$outcome)
    }
  }
  return(cbind(result, df))
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
#' @keywords internal
#' @export
predictChoices <- function(model, alts, altID, obsID = NULL) {
    # v0.3.2
    .Deprecated("predict")
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
#' @keywords internal
#' @export
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
#' @keywords internal
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
