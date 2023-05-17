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
#' @param type A character vector defining what to predict: `prob` for
#' probabilities, `outcomes` for outcomes. If you want both outputs, use
#' `c("prob", "outcome")`. Outcomes are predicted randomly according to the
#' predicted probabilities. Defaults to `"prob"`.
#' @param returnData If `TRUE` the data is also returned, otherwise only the
#' predicted values ("prob" and / or  "outcome") are returned.
#' Defaults to `FALSE`.
#' @param interval Type of interval calculation: "none" (default) or
#' "confidence". Future versions will include "prediction" intervals as well.
#' @param level Tolerance / confidence interval. Defaults to 0.95.
#' @param ci No longer used as of v1.1.0 - if provided, this is passed
#' to the `level` argument, `interval` is set to `"confidence"`,
#' and a warning is displayed.
#' @param numDrawsCI The number of draws to use in simulating uncertainty
#' for the computed CI. Defaults to 10^4.
#' @param pars The names of the parameters to be estimated in the model.
#' Must be the same as the column names in the `data` argument. For WTP space
#' models, do not include the `scalePar` variable in `pars`.
#' @param scalePar The name of the column that identifies the scale variable,
#' which is typically "price" for WTP space models, but could be any
#' continuous variable, such as "time". Defaults to `NULL`.
#' @param randPars A named vector whose names are the random parameters and
#' values the distribution: `'n'` for normal, `'ln'` for log-normal, or
#' `'cn'` for zero-censored normal. Defaults to `NULL`.
#' @param randScale The random distribution for the scale parameter: `'n'` for
#' normal, `'ln'` for log-normal, or `'cn'` for zero-censored normal. Only used
#' for WTP space MXL models. Defaults to `NULL`.
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
#' # Predict probabilities and include a 95% confidence interval
#' predict(
#'   mnl_pref,
#'   newdata = data,
#'   obsID = "obsID",
#'   interval = "confidence",
#'   level = 0.95
#' )
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
  type       = "prob",
  returnData = FALSE,
  interval   = "none",
  level      = 0.95,
  numDrawsCI = 10^4,
  pars       = NULL,
  scalePar   = NULL,
  randPars   = NULL,
  randScale  = NULL,
  ci,
  ...
) {
  # Argument names were changed in v1.1.0
  call <- match.call()
  calls <- names(sapply(call, deparse))[-1]
  if (any("ci" %in% calls)) {
    level <- ci
    interval <- "confidence"
    warning(
      "The 'ci' argument is outdated as of v1.1.0. Use 'level' instead and ",
      "set interval = 'confidence'"
    )
  }
  predictInputsCheck(object, newdata, obsID, type, level, interval)
  # If user provides parameters, use them instead of the original pars
  # used to estimate the model
  # if (!is.null(pars)) { object$inputs$pars <- pars }
  # if (!is.null(randPars)) { object$inputs$randPars <- inputs$randPars}
  # if (!is.null(scalePar)) { object$inputs$scalePar <- inputs$scalePar }
  # if (!is.null(randScale)) { randScale <- inputs$randScale }
  d <- object$data
  # If no newdata is provided, use the data from the estimated object
  if (is.null(newdata)) {
    data <- list(
      X        = d$X,
      scalePar = d$scalePar,
      obsID    = d$obsID,
      outcome  = d$outcome)
    obsID <- object$inputs$obsID
  } else {
    data <- formatNewData(object, newdata, obsID)
  }
  getV <- getMnlV_pref
  getVDraws <- getMxlV_pref
  if (object$modelSpace == "wtp") {
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
    level <- NULL
    interval <- "none"
  }

  if (object$modelType == "mxl") {
    result <- getMxlProbs(
      object, data, obsID, interval, level, numDrawsCI, getV, getVDraws)
  } else {
    result <- getMnlProbs(
      object, data, obsID, interval, level, numDrawsCI, getV, getVDraws)
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
  newdata <- checkFactorLevels(object, newdata)
  recoded <- recodeData(newdata, inputs$pars, inputs$randPars)
  X <- recoded$X
  predictParCheck(object, X) # Check if model pars match those from newdata
  scalePar <- NA
  if (object$modelSpace == "wtp") {
    scalePar <- as.matrix(
      newdata[, which(colnames(newdata) == object$inputs$scalePar)])
  }
  if (is.null(obsID)) {
    obsIDName <- inputs$obsID # Use obsID from estimated object
  } else {
    obsIDName <- obsID
  }
  obsID <- newdata[, obsIDName]
  return(list(X = X, scalePar = scalePar, obsID = obsID))
}

# If some factor levels present in the data used to estimate the model
# are missing, then they need to be added back into the newdata so that
# the coefficients are correctly interpreted. This function adds back
# missing factor levels
checkFactorLevels <- function(object, newdata) {
  levels_orig <- object$data$factorLevels
  factorLevels <- getFactorLevels(newdata, object$inputs$pars)
  if (length(factorLevels) > 0) {
    for (i in 1:length(factorLevels)) {
      par <- names(factorLevels)[[i]]
      levels_new <- factorLevels[[i]]
      levels_old <- levels_orig[[par]]
      if (! identical(levels_new, levels_old)) {
        newdata[[par]] <- factor(newdata[[par]], levels = levels_old)
      }
    }
  }
  return(newdata)
}

getMnlProbs <- function(
    object, data, obsID, interval, level, numDrawsCI, getV, getVDraws
) {
  obsIDName <- obsID
  # Compute mean probs
  coefs <- stats::coef(object)
  V <- getV(coefs, data$X, data$scalePar)
  probs_mean <- predictLogit(V, data$obsID)
  if (interval != "confidence") {
    return(probsNoIntervals(probs_mean, data, interval, obsIDName))
  }
  # Compute confidence interval with simulation
  n <- object$n
  n$draws <- numDrawsCI
  betaUncDraws <- getUncertaintyDraws(object, numDrawsCI)
  betaUncDraws <- selectDraws(betaUncDraws, object$modelSpace, data$X)
  VUncDraws <- getVDraws(betaUncDraws, data$X, data$scalePar, n)
  logitUncDraws <- predictLogit(VUncDraws, data$obsID)
  probs <- formatProbsUnc(
    probs_mean, logitUncDraws, data$obsID, obsIDName, level)
  return(probs)
}

getMxlProbs <- function(
    object, data, obsID, interval, level, numDrawsCI, getV, getVDraws
) {
  obsIDName <- obsID
  # Compute mean probs
  coefs <- stats::coef(object)
  probs_mean <- predictLogitDraws(coefs, object, data, getVDraws)
  if (interval != "confidence") {
    return(probsNoIntervals(probs_mean, data, interval, obsIDName))
  }
  # Compute uncertainty with simulation
  betaUncDraws <- getUncertaintyDraws(object, numDrawsCI)
  logitUncDraws <- apply(
    betaUncDraws, 1, predictLogitDraws,
    object, data, getVDraws)
  probs <- formatProbsUnc(
    probs_mean, logitUncDraws, data$obsID, obsIDName, level)
  return(probs)
}

probsNoIntervals <- function(probs_mean, data, interval, obsIDName) {
  probs <- formatProbsMean(probs_mean, data$obsID, obsIDName)
  if (interval == "prediction") {
    warning(
      "The version of {logitr} you are using does not yet support ",
      "prediction intervals, so none have been provided. You can obtain ",
      "confidence intervals by setting interval = 'confidence'."
    )
  }
  return(probs)
}

# Reorders the columns depending on if the model is a WTP space model or not
# and returns only the draws for the vars in X
selectDraws <- function(betaDraws, modelSpace, X) {
  names <- colnames(betaDraws)
  gammaDraws <- betaDraws[,which(names %in% colnames(X))]
  if (modelSpace == "wtp") {
    lambdaDraws <- betaDraws[,which(names == "scalePar")]
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
  correlation <- object$inputs$correlation
  betaDraws <- makeBetaDraws(
      coefs, object$parIDs, object$n, object$standardDraws, correlation
  )
  colnames(betaDraws) <- names(object$parSetup)
  betaDraws <- selectDraws(betaDraws, object$modelSpace, data$X)
  VDraws <- getVDraws(betaDraws, data$X, data$scalePar, object$n)
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

formatProbsUnc <- function(probs_mean, logitUncDraws, obsID, obsIDName, level) {
  probs_bounds <- ci(t(logitUncDraws), level)[c("lower", "upper")]
  colnames(probs_bounds) <- paste0("predicted_prob_", colnames(probs_bounds))
  probs <- cbind(predicted_prob = probs_mean, probs_bounds)
  names <- c(obsIDName, colnames(probs))
  probs[obsIDName] <- obsID
  return(probs[names])
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
    if (object$modelSpace == "wtp") {
      df <- cbind(df, scalePar = data$scalePar)
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
    .Deprecated("predict")
}
