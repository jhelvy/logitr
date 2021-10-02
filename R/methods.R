#' Methods for logitr objects
#'
#' Miscellaneous methods for `logitr` class objects.
#'
#' @name miscmethods.logitr
#' @aliases logLik.logitr terms.logitr coef.logitr coef.summary.logitr
#' summary.logitr print.logitr print.summary.logitr se.logitr vcov.logitr
#' predict.logitr
#' @param x is an object of class `logitr`.
#' @param object is an object of class `logitr`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing.
#' @param newData a `data.frame` for the `predict` method. Each row is an
#' alternative and each column an attribute corresponding to parameter names
#' in the estimated model. Defaults to `NULL`, in which case the original data
#' used to estimate the model are used.
#' @param obsID The name of the column that identifies each set of
#' alternatives in the data for the `predict` method. Required if predicting
#' results for more than one set of alternatives. Defaults to `NULL`, in which
#' case the value for `obsID` from the estimated `object` is used.
#' @param returnProbs for the `predict` method, if `TRUE` the predicted
#' probabilities are returned. Defaults to `TRUE`.
#' @param returnChoices for the `predict` method, if `TRUE` the predicted
#' choices are returned. Defaults to `FALSE`.
#' @param returnData for the `predict` method, if `TRUE` the data is also
#' returned, otherwise only the predicted values (probs or choices) are
#' returned. Defaults to `TRUE`.
#' @param computeCI Should a confidence interval be computed for the `predict`
#' method? Defaults to `FALSE`.
#' @param ci The sensitivity of the computed confidence interval (CI) for the
#' `predict` method. Defaults to `ci = 0.95`, reflecting a 95% CI.
#' @param numDraws The number of draws to use in simulating uncertainty
#' for the computed CI for the `predict` method. Defaults to 10^4.
#' @param ... further arguments.
#'
#' @rdname miscmethods.logitr
#' @export
logLik.logitr <- function(object, ...) {
  return(structure(
    object$logLik,
    df    = object$numParams,
    null  = sum(object$freq * log(object$freq / object$numObs)),
    class = "logLik"
  ))
}

#' @rdname miscmethods.logitr
#' @export
terms.logitr <- function(x, ...) {
  return(x$inputs$pars)
}

#' @rdname miscmethods.logitr
#' @export
coef.logitr <- function(object, ...) {
    return(object$coef)
}

#' @rdname miscmethods.logitr
#' @method coef summary.logitr
#' @export
coef.summary.logitr <- function(object, ...) {
    return(object$coefTable)
}

#' @rdname miscmethods.logitr
#' @export
summary.logitr <- function (object, ...) {
    object$modelInfoTable <- getModelInfoTable(object)
    coefs <- stats::coef(object)
    standErr <- se(object)
    object$coefTable <- getCoefTable(coefs, standErr)
    object$statTable <- getStatTable(object)
    if (object$modelType == "mxl") {
        object$randParSummary <- getRandParSummary(object)
    }
    class(object) <- c("summary.logitr", "logitr")
    return(object)
}

#' @rdname miscmethods.logitr
#' @export
print.logitr <- function (
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
) {
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  modelType <- getModelType(x)
  modelSpace <- getModelSpace(x)
  cat("A", modelType, "model estimated in the", modelSpace, "space\n\n")
  cat("Exit Status: ", x$status, ", ", getExitMessage(x), "\n\n", sep = "")
  # Print which run was "best" if a multistart was used
  if (x$inputs$numMultiStarts > 1) {
    modelRun <- getModelRun(x)
    cat(
      "Results below are from run", modelRun, "multistart runs\n",
      "as it had the largest log-likelihood value\n")
    cat("\n")
  }
  # Print coefficients & log-likelihood
  if (!any(is.na(stats::coef(x)))) {
      cat("Coefficients:\n")
      print.default(
        format(x$coef, digits = digits), print.gap = 2, quote = FALSE)
      print(x$logLik)
  } else {
    cat("No coefficients\n")
  }
  cat("\n")
  invisible(x)
}

#' @rdname miscmethods.logitr
#' @method print summary.logitr
#' @export
print.summary.logitr <- function(
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
) {
  cat("=================================================", "\n", sep = "")
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("Frequencies of alternatives:\n")
  print(prop.table(x$freq), digits = digits)
  # Print multistart summary
  if (!is.null(x$multistartSummary)) {
    cat("\n")
    cat("Summary Of Multistart Runs:\n")
    print(x$multistartSummary)
    cat("\n")
    cat("Use statusCodes() to view the meaning of each status code\n")
  }
  cat("\nExit Status: ", x$status, ", ", getExitMessage(x), "\n", sep = "")
  print(x$modelInfoTable)
  cat("\n")
  cat("Model Coefficients:", "\n")
  stats::printCoefmat(x$coefTable, digits = digits)
  print(x$statTable)
  if (x$modelType == "mxl") {
      cat("\n")
      cat("Summary of 10k Draws for Random Coefficients:", "\n")
      print(x$randParSummary)
  }
  invisible(x)
}

getModelInfoTable <- function(object) {
  modelType <- getModelType(object)
  modelSpace <- getModelSpace(object)
  modelRun <- getModelRun(object)
  modelTime <- convertTime(object$time)
  algorithm <- object$options$algorithm
  modelInfoTable <- data.frame(c(
    modelType, modelSpace, modelRun, object$iterations,
    modelTime, algorithm, object$weightsUsed
  ))
  colnames(modelInfoTable) <- ""
  row.names(modelInfoTable) <- c(
    "Model Type:", "Model Space:", "Model Run:", "Iterations:",
    "Elapsed Time:", "Algorithm:", "Weights Used?:"
  )
  panelID <- object$inputs$panelID
  if (!is.null(panelID)) {
    modelInfoTable <- rbind(modelInfoTable, panelID)
    row.names(modelInfoTable)[nrow(modelInfoTable)] <- "Panel ID:"
  }
  if (!is.null(object$numClusters)) {
    if (object$numClusters > 0) {
      modelInfoTable <- rbind(modelInfoTable, object$inputs$clusterID)
      row.names(modelInfoTable)[nrow(modelInfoTable)] <- "Cluster ID:"
    }
  }
  robust <- object$inputs$robust
  if (!is.null(robust)) {
    modelInfoTable <- rbind(modelInfoTable, robust)
    row.names(modelInfoTable)[nrow(modelInfoTable)] <- "Robust?"
  }
  return(modelInfoTable)
}

getCoefTable <- function(coefs, standErr) {
    z <- coefs / standErr
    p <- 2 * (1 - stats::pnorm(abs(z)))
    coefTable <- cbind(coefs, standErr, z, p)
    colnames(coefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
    return(as.data.frame(coefTable))
}

getStatTable <- function(object) {
  aic <- stats::AIC(object)
  bic <- round(log(object$numObs) * object$numParams - 2 * object$logLik, 4)
  mcR2 <- 1 - (object$logLik / object$nullLogLik)
  adjMcR2 <- 1 - ((object$logLik - object$numParams) / object$nullLogLik)
  statTable <- data.frame(c(
    object$logLik, object$nullLogLik, aic, bic, mcR2, adjMcR2, object$numObs
  ))
  colnames(statTable) <- ""
  row.names(statTable) <- c(
    "Log-Likelihood:", "Null Log-Likelihood:", "AIC:", "BIC:", "McFadden R2:",
    "Adj McFadden R2:" , "Number of Observations:")
  if (!is.null(object$numClusters)) { # Added for backwards compatibility
    if (object$numClusters > 0) {
      statTable <- rbind(statTable, object$numClusters)
      row.names(statTable)[nrow(statTable)] <- "Number of Clusters"
    }
  }
  return(statTable)
}

getRandParSummary <- function(object) {
  parSetup <- object$parSetup
  parIDs <- object$parIDs
  numDraws <- 10^4
  standardDraws <- getStandardDraws(parIDs, numDraws)
  betaDraws <- makeBetaDraws(object$coef, parIDs, numDraws, standardDraws)
  randParSummary <- apply(betaDraws, 2, summary)
  # Add names to summary
  distName <- rep("", length(parSetup))
  distName[parIDs$normal] <- "normal"
  distName[parIDs$logNormal] <- "log-normal"
  summaryNames <- paste(names(parSetup), " (", distName, ")", sep = "")
  colnames(randParSummary) <- summaryNames
  randParSummary <- t(randParSummary[, parIDs$random])
  return(as.data.frame(randParSummary))
}

getModelType <- function(x) {
  return(ifelse(x$modelType == "mnl", "Multinomial Logit", "Mixed Logit"))
}

getModelSpace <- function(x) {
  return(ifelse(
    x$inputs$modelSpace == "pref", "Preference", "Willingness-to-Pay"))
}

getModelRun <- function(x) {
  return(paste(x$multistartNumber, "of", x$inputs$numMultiStarts))
}

getExitMessage <- function(x) {
  codes <- getStatusCodes()
  return(codes$message[which(codes$code == x$status)])
}

#' Get standard errors
#'
#' @param object is an object of class `logitr`.
#' @param ... further arguments.
#' @export
se <- function(object, ...) {
  UseMethod("se")
}

#' @rdname miscmethods.logitr
#' @export
se.logitr <- function(object, ...) {
  return(sqrt(diag(stats::vcov(object))))
}

#' @rdname miscmethods.logitr
#' @export
vcov.logitr <- function(object, ...) {
  clusterID <- object$data$clusterID
  if (is.null(clusterID) | object$inputs$robust == FALSE) {
    return(getCovarianceNonRobust(object$hessian))
  }
  return(getCovarianceRobust(object))
}

getCovarianceNonRobust <- function(hessian) {
  covariance <- hessian*NA
  tryCatch(
    {
      covariance <- solve(-1*hessian)
    },
    error = function(e) {}
  )
  return(covariance)
}

getCovarianceRobust <- function(object) {
  numClusters <- object$numClusters
  inputs <- object$inputs
  parSetup <- object$parSetup
  modelInputs <- list(
    logitFuncs = setLogitFunctions(inputs$modelSpace),
    evalFuncs = setEvalFunctions(object$modelType, inputs$useAnalyticGrad),
    inputs = inputs,
    modelType = object$modelType,
    numBetas = length(parSetup),
    numDraws = inputs$numDraws,
    parSetup = parSetup,
    parIDs = object$parIDs,
    standardDraws = object$standardDraws,
    panel = !is.null(inputs$panelID),
    data_diff = makeDiffData(object$data, object$modelType)
  )
  clusterID <- modelInputs$data_diff$clusterID
  scaleFactors <- object$data$scaleFactors
  parsUnscaled <- stats::coef(object)*scaleFactors
  gradMat <- matrix(NA, nrow = numClusters, ncol = length(parsUnscaled))
  clusters <- sort(unique(clusterID))
  for (i in seq_len(length(clusters))) {
    indices <- which(clusterID == i)
    tempMI <- getClusterModelInputs(indices, modelInputs, i)
    gradMat[i, ] <- getGradient(parsUnscaled, scaleFactors, tempMI)
  }
  gradMeanMat <- repmat(matrix(colMeans(gradMat), nrow = 1), numClusters, 1)
  diffMat <- gradMat - gradMeanMat
  M <- t(diffMat) %*% diffMat
  M <- M * (numClusters / (numClusters - 1)) # small sample correction
  D <- getCovarianceNonRobust(object$hessian)
  if (any(is.na(D))) { return(D) } # If there are NAs the next line will error
  return(D %*% M %*% D)
}

getClusterModelInputs <- function (indices, mi, i) {
  X <- mi$data_diff$X[indices,]
  # Cast to matrix in cases where there is 1 independent variable
  X <- checkMatrix(X)
  mi$data_diff$X <- X
  mi$data_diff$price <- mi$data_diff$price[indices]
  obsID <- mi$data_diff$obsID[indices]
  unique_obsID <- unique(obsID)
  mi$data_diff$obsID <- obsID
  panelID <- mi$data_diff$panelID[unique_obsID]
  weights <- mi$data_diff$weights[unique_obsID]
  if (!is.null(panelID)) {
    weights <- mi$data_diff$weights[i]
  }
  mi$data_diff$panelID <- panelID
  mi$data_diff$weights <- weights
  mi$data_diff$clusterID <- NULL
  if (isMxlModel(mi$parSetup)) {
    mi$partials <- makePartials(mi)
  }
  mi$nrowX <- nrow(X)
  return(mi)
}

#' @rdname miscmethods.logitr
#' @export
print.logitr_wtp <- function (
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
) {
  stats::printCoefmat(x, digits = digits)
}

#' @rdname miscmethods.logitr
#' @export
predict.logitr <- function(
  object,
  newData       = NULL,
  obsID         = NULL,
  returnProbs   = TRUE,
  returnChoices = FALSE,
  returnData    = FALSE,
  computeCI     = FALSE,
  ci            = 0.95,
  numDraws      = 10^4,
  ...
) {
  d <- object$data
  # If no newData is provided, use the data from the estimated object
  if (is.null(newData)) {
    data <- list(X = d$X, price = d$price, obsID = d$obsID)
  } else {
    data <- formatNewData(object, newData, obsID)
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
        alts, model, price, X, altID, obsID, altIDName, obsIDName, numDraws,
        ci, getV, getVDraws, computeCI))
  } else {
    return(
      mnlSimulation(
        alts, model, price, X, altID, obsID, altIDName, obsIDName, numDraws,
        ci, getV, getVDraws, computeCI))
  }
}

formatNewData <- function(object, newData, obsID) {
  predictInputsCheck(object, newData, obsID)
  inputs <- object$inputs
  newData <- as.data.frame(newData) # tibbles break things
  recoded <- recodeData(newData, inputs$pars, inputs$randPars)
  X <- recoded$X
  predictParCheck(object, X) # Check if model pars match those from newData
  price <- NA
  if (inputs$modelSpace == "wtp") {
    price <- as.matrix(newData[, which(colnames(newData) == inputs$price)])
  }
  if (is.null(obsID)) {
    obsIDName <- inputs$obsID # Use obsID from estimated object
  } else {
    obsIDName <- obsID
  }
  obsID <- newData[, obsIDName]
  return(list(X = X, price = price, obsID = obsID))
}

predictLogit <- function(V, obsID) {
  expV <- exp(V)
  sumExpV <- rowsum(expV, group = obsID, reorder = FALSE)
  reps <- table(obsID)
  return(expV / sumExpV[rep(seq_along(reps), reps),])
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
