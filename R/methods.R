#' Methods for logitr objects
#'
#' Miscellaneous methods for `logitr` class objects.
#'
#' @name miscmethods.logitr
#' @aliases print.logitr logLik.logitr coef.logitr coef.summary.logitr
#' vcov.logitr summary.logitr print.summary.logitr
#' @param x is an object of class `logitr`.
#' @param object is an object of class `logitr`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing,
#' @param ... further arguments.
#'
#' @rdname miscmethods.logitr
#' @export
logLik.logitr <- function(object, ...) {
    return(object$logLik)
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
    standErr <- sqrt(diag(stats::vcov(object)))
    object$coefTable <- getCoefTable(coefs, standErr)
    object$statTable <- getStatTable(object)
    if (object$modelType == "mxl") {
        object$randParSummary <- getRandParSummary(object)
    }
    class(object) <- c("summary.logitr", "logitr")
    return(object)
}

#' @rdname miscmethods.mlogit
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
  robust <- object$inputs$robust
  if (!is.null(robust)) { # Added for backwards compatibility
    modelInfoTable <- rbind(modelInfoTable, robust)
    row.names(modelInfoTable)[nrow(modelInfoTable)] <- "Robust?"
  }
  if (!is.null(object$numClusters)) { # Added for backwards compatibility
    if (object$numClusters > 0) {
      modelInfoTable <- rbind(modelInfoTable, object$inputs$cluster)
      row.names(modelInfoTable)[nrow(modelInfoTable)] <- "Cluster Name:"
    }
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

getStatTable <- function(model) {
  aic <- round(2 * model$numParams - 2 * model$logLik, 4)
  bic <- round(log(model$numObs) * model$numParams - 2 * model$logLik, 4)
  mcR2 <- 1 - (model$logLik / model$nullLogLik)
  adjMcR2 <- 1 - ((model$logLik - model$numParams) / model$nullLogLik)
  statTable <- data.frame(c(
    model$logLik, model$nullLogLik, aic, bic, mcR2, adjMcR2, model$numObs
  ))
  colnames(statTable) <- ""
  row.names(statTable) <- c(
    "Log-Likelihood:", "Null Log-Likelihood:", "AIC:", "BIC:", "McFadden R2:",
    "Adj McFadden R2:" , "Number of Observations:")
  if (!is.null(model$numClusters)) { # Added for backwards compatibility
    if (model$numClusters > 0) {
      statTable <- rbind(statTable, model$numClusters)
      row.names(statTable)[nrow(statTable)] <- "Number of Clusters"
    }
  }
  return(statTable)
}

getRandParSummary <- function(object) {
  parSetup <- object$parSetup
  numDraws <- 10^4
  randParIDs <- getRandParIDs(parSetup)
  standardDraws <- getStandardDraws(parSetup, numDraws)
  betaDraws <- makeBetaDraws(object$coef, parSetup, 10^4, standardDraws)
  randParSummary <- apply(betaDraws, 2, summary)
  # Add names to summary
  distName <- rep("", length(parSetup))
  distName[getNormParIDs(parSetup)] <- "normal"
  distName[getLogNormParIDs(parSetup)] <- "log-normal"
  summaryNames <- paste(names(parSetup), " (", distName, ")", sep = "")
  colnames(randParSummary) <- summaryNames
  randParSummary <- t(randParSummary[, randParIDs])
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

#' @rdname miscmethods.logitr
#' @export
vcov.logitr <- function(object, ...) {
  clusterID <- object$clusterIDs
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
  i <- 0
  gradientList <- c()
  clusterID <- object$clusterIDs
  modelInputs <- list()
  modelInputs$logitFuncs <- setLogitFunctions(object$inputs$modelSpace)
  modelInputs$evalFuncs <- setEvalFunctions(
    object$modelType, object$inputs$useAnalyticGrad)
  modelInputs$inputs <- object$inputs
  parsUnscaled <- stats::coef(object)
  scaleFactors <- object$scaleFactors
  if (object$inputs$scaleInputs) {
    parsUnscaled <- parsUnscaled * scaleFactors
  }
  for (tempID in sort(unique(clusterID))) {
    indices <- which(clusterID == tempID)
    tempModelInputs <- getClusterModelInputs(object, indices, modelInputs)
    tempGradient <- getGradient(parsUnscaled, scaleFactors, tempModelInputs)
    gradientList <- c(gradientList, tempGradient)
    i <- i + 1
  }
  gradMat <- matrix(gradientList, nrow = i, length(tempGradient), byrow = TRUE)
  gradMean <- colMeans(gradMat)
  gradMeans <- c()
  for (tempID in sort(unique(clusterID))) {
    gradMeans <- c(gradMeans, gradMean)
  }
  gradMeanMat <- matrix(
    gradMeans, nrow = i, length(tempGradient), byrow = TRUE)

  diffMat <- gradMat - gradMeanMat

  M <- t(diffMat) %*% diffMat
  smallSampleCorrection <- (i / (i - 1))
  M <- smallSampleCorrection * M
  D <- getCovarianceNonRobust(object$hessian)
  if (any(is.na(D))) {
    return(D) # If there are NAs the next line will error
  }
  return(D %*% M %*% D)
}

getClusterModelInputs <- function (object, indices, modelInputs) {
  X <- object$X[indices, ]
  # Cast to matrix in cases where there is 1 independent variable
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  modelInputs$X          <- X
  modelInputs$choice     <- object$choice[indices]
  modelInputs$price      <- object$price[indices]
  modelInputs$weights    <- object$weights[indices]
  modelInputs$obsID      <- object$obsID[indices]
  modelInputs$clusterIDs <- object$clusterIDs[indices]
  return(modelInputs)
}
