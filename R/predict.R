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
#' alternatives in the data. Required if newdata != NULL. Defaults to `NULL`,
#' in which case the value for `obsID` from the data in `object` is used.
#' @param price The name of the column that identifies the price variable.
#' Required if the `object` is a WTP space model and if newdata != NULL.
#' Defaults to `NULL`.
#' @param type A character vector defining what to predict: `probs` for
#' probabilities, `choices` for choices. If you want both outputs, use
#' `c("probs", "choices")`. Choices are predicted randomly according to the
#' predicted probabilities. Defaults to `"probs"`.
#' @param returnData If `TRUE` the data is also returned, otherwise only the
#' predicted values ("probs" and / or  "choices") are returned.
#' Defaults to `TRUE`.
#' @param ci If a confidence interval (CI) for the predicted probabilities is
#' desired, set `ci` to a number between 0 and 1 to define the CI sensitivity.
#' For example, `ci = 0.95` will return a 95% CI. Defaults to `NULL`, in which
#' case no CI is computed.
#' @param numDrawsCI The number of draws to use in simulating uncertainty
#' for the computed CI. Defaults to 10^3.
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
#' # Predict probabilities and / or choices
#'
#' # Predict probabilities for each alternative in the model data
#' p <- predict(mnl_pref)
#' head(p)
#'
#' # Create a set of alternatives for which to make predictions.
#' # Each row is an alternative and each column an attribute. In this example,
#' # two of the choice observations from the yogurt dataset are used
#' alts <- subset(
#'     yogurt, obsID %in% c(42, 13),
#'     select = c('obsID', 'alt', 'price', 'feat', 'brand'))
#' alts
#'
#' # Predict choice probabilities using the estimated model
#' predict(mnl_pref, alts, obsID = "obsID")
#'
#' # Predict choices and probabilities using the estimated model
#' predict(mnl_pref, alts, obsID = "obsID", type = c("probs", "choices"))
predict.logitr <- function(
  object,
  newdata    = NULL,
  obsID      = NULL,
  price      = NULL,
  type       = "probs",
  returnData = TRUE,
  ci         = NULL,
  numDrawsCI = 10^3
) {
  predictInputsCheck(object, newdata, obsID, price, type, ci)
  d <- object$data
  # If no newdata is provided, use the data from the estimated object
  if (is.null(newdata)) {
    data <- list(X = d$X, price = d$price, obsID = d$obsID)
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
  predict_choice <- FALSE
  predict_probs <- TRUE
  if ("choices" %in% type) {
    predict_choice <- TRUE
  }
  if (! ("probs" %in% type)) {
    predict_probs <- FALSE
    ci <- NULL
  }

  if (object$modelType == "mxl") {
    result <- getMxlProbs(
       object, data, obsID, returnData, ci, numDrawsCI, getV, getVDraws)
  } else {
    result <- getMnlProbs(
       object, data, obsID, returnData, ci, numDrawsCI, getV, getVDraws)
  }
  if (predict_choice) {
    result <- addChoices(result, obsID)
  }
  if (!predict_probs) { # Remove prob_predict if "probs" not in type
    result$prob_predict <- NULL
  }
  if (returnData) {
    result <- addData(result, data)
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

getMnlProbs <- function(
  object, data, obsID, returnData, ci, numDrawsCI, getV, getVDraws
) {
  obsIDName <- obsID
  # Compute mean probs
  coefs <- stats::coef(object)
  V <- getV(coefs, data$X, data$price)
  probs_mean <- predictLogit(V, data$obsID)
  if (is.null(ci)) {
    probs <- formatProbsMean(probs_mean, data$obsID, obsIDName)
  } else {
    # Compute uncertainty with simulation
    betaUncDraws <- getUncertaintyDraws(object, numDrawsCI)
    betaUncDraws <- selectDraws(betaUncDraws, object$inputs$modelSpace, data$X)
    VUncDraws <- getVDraws(betaUncDraws, data$X, data$price)
    logitUncDraws <- predictLogit(VUncDraws, data$obsID)
    probs <- formatProbsUnc(
      probs_mean, logitUncDraws, data$obsID, obsIDName, ci)
  }
  return(probs)
}

getMxlProbs <- function(
  object, data, obsID, returnData, ci, numDrawsCI, getV, getVDraws
) {
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
  numDrawsLogit <- object$inputs$numDraws
  standardDraws <- getStandardDraws(object$parIDs, numDrawsLogit)
  betaDraws <- makeBetaDraws(coefs, object$parIDs, numDrawsLogit, standardDraws)
  colnames(betaDraws) <- names(object$parSetup)
  betaDraws <- selectDraws(betaDraws, modelSpace, data$X)
  VDraws <- getVDraws(betaDraws, data$X, data$price)
  logitDraws <- predictLogit(VDraws, data$obsID)
  return(rowMeans(logitDraws, na.rm = TRUE))
}

# Functions for formatting the probabilities

formatProbsMean <- function(probs_mean, obsID, obsIDName) {
  probs <- as.data.frame(probs_mean)
  colnames(probs) <- "prob_predict"
  probs[obsIDName] <- obsID
  return(probs[c(obsIDName, "prob_predict")])
}

formatProbsUnc <- function(probs_mean, logitUncDraws, obsID, obsIDName, ci) {
  probs_bounds <- getCI(logitUncDraws, ci)
  colnames(probs_bounds) <- paste0("prob_predict_", colnames(probs_bounds))
  probs <- cbind(prob_predict = probs_mean, probs_bounds)
  names <- c(obsIDName, colnames(probs))
  probs[obsIDName] <- obsID
  return(probs[names])
}

getCI <- function(draws, ci = 0.95) {
  alpha <- (1 - ci)/2
  probs <- c(alpha, 1 - alpha)
  result <- apply(draws, 1, quantile_speed, probs = probs)
  result <- as.data.frame(t(result))
  names(result) <- c("lower", "upper")
  return(result)
}

# quantile_speed is copied from this gist:
# https://gist.github.com/sikli/f1775feb9736073cefee97ec81f6b193
quantile_speed <- function(x, probs = c(0.1, 0.9), na.rm = FALSE) {

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
  qs

}

addChoices <- function(probs, obsID) {
  choices <- split(probs, probs[obsID])
  choices <- lapply(choices, simChoice)
  probs$choice_predict <- do.call(rbind, choices)$choice_predict
  return(probs)
}

# Simulate choices based on probabilities
simChoice <- function(df) {
  choices <- seq_len(nrow(df))
  result <- 0*choices
  result[sample(x = choices, size = 1, prob = df$prob_predict)] <- 1
  df$choice_predict <- result
  return(df)
}

addData <- function(result, data) {
  df <- as.data.frame(data$X)
  if (length(data$price) > 1) {
    df <- cbind(df, price = data$price)
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
