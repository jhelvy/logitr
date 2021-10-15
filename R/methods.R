#' Methods for logitr objects
#'
#' Miscellaneous methods for `logitr` class objects.
#'
#' @name miscmethods.logitr
#' @aliases logLik.logitr terms.logitr coef.logitr coef.summary.logitr
#' summary.logitr print.logitr print.summary.logitr se.logitr vcov.logitr
#' @param x is an object of class `logitr`.
#' @param object is an object of class `logitr`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing.
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
    return(object$coefficients)
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
        format(x$coefficients, digits = digits), print.gap = 2, quote = FALSE)
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
  betaDraws <- makeBetaDraws(stats::coef(object), parIDs, numDraws, standardDraws)
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
  scaleFactors <- object$scaleFactors
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

#' Extract Model Fitted Values
#'
#' Returns fitted values from an object of class `logitr`.
#' @keywords logitr fitted fitted.values
#'
#' @param object is an object of class `logitr`.
#'
#' @return A data frame of the `obsID` and the fitted values extracted from
#' `object`.
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
#' # Extract the fitted values from the model
#' fitted(mnl_pref)
fitted.logitr <- function(object) {
  probs <- predict(object, type = "probs")
  choice <- object$data$choice
  fitted <- probs[which(choice == 1),]
  names(fitted)[which(names(fitted) == 'prob_predict')] <- "fitted_value"
  return(fitted)
}

#' Extract Model Residuals
#'
#' Returns model residuals from an object of class `logitr`.
#' @keywords logitr residuals resid
#'
#' @param object is an object of class `logitr`.
#'
#' @return A data frame of the `obsID` and the residuals (response minus fitted
#' values) extracted from `object`.
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
#' # Extract the residuals from the model
#' residuals(mnl_pref)
residuals.logitr <- function(object) {
  fitted <- fitted(object)
  reps <- table(object$data$obsID)
  residuals <- fitted[rep(seq_along(reps), reps),]
  residuals$residual <- object$data$choice - residuals$fitted_value
  residuals$fitted_value <- NULL
  return(residuals)
}
