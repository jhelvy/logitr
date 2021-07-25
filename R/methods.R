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
vcov.logitr <- function(object, ...) {
    return(object$covariance)
}

#' @rdname miscmethods.logitr
#' @export
summary.logitr <- function (object, ...) {
    object$modelInfoTable <- getModelInfoTable(object)
    object$coefTable <- getCoefTable(object$coef, object$standErrs)
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
  if (nrow(x$multistartSummary) > 1) {
    modelRun <- getModelRun(x)
    cat(
      "Results below are from run", modelRun, "multistart runs\n",
      "as it had the largest log-likelihood value\n")
    cat("\n")
  }
  # Print coefficients
  if (length(x$coef)) {
      cat("Coefficients:\n")
      print.default(
        format(x$coef, digits = digits), print.gap = 2, quote = FALSE)
  } else {
    cat("No coefficients\n")
  }
  # Print log-likelihood
  print(x$logLik)
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
  if (nrow(x$multistartSummary) > 1) {
    cat("\n")
    cat("Summary Of Multistart Runs:\n")
    print(x$multistartSummary)
    cat("\n")
    cat("Use statusCodes() to view the meaning of each status code\n")
  }
  cat("\nExit Status:", x$status, "\n")
  cat(getExitMessage(x))
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

getModelInfoTable <- function(model) {
  modelType <- getModelType(model)
  modelSpace <- getModelSpace(model)
  modelRun <- getModelRun(model)
  modelTime <- convertTime(model$time)
  algorithm <- model$options$algorithm
  modelInfoTable <- data.frame(c(
    modelType, modelSpace, modelRun, model$iterations,
    modelTime, algorithm, model$weightsUsed
  ))
  colnames(modelInfoTable) <- ""
  row.names(modelInfoTable) <- c(
    "Model Type:", "Model Space:", "Model Run:", "Iterations:",
    "Elapsed Time:", "Algorithm:", "Weights Used?:"
  )
  if (!is.null(model$robust)) { # Added for backwards compatibility
    modelInfoTable <- rbind(modelInfoTable, model$robust)
    row.names(modelInfoTable)[nrow(modelInfoTable)] <- "Robust?"
  }
  if (!is.null(model$numClusters)) { # Added for backwards compatibility
    if (model$numClusters > 0) {
      modelInfoTable <- rbind(modelInfoTable, model$inputs$cluster)
      row.names(modelInfoTable)[nrow(modelInfoTable)] <- "Cluster Name:"
    }
  }
  return(modelInfoTable)
}

getCoefTable <- function(coef, standErrs) {
    z <- coef / standErrs
    p <- 2 * (1 - stats::pnorm(abs(z)))
    coefTable <- cbind(coef, standErrs, z, p)
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
  return(paste(x$multistartNumber, "of", x$options$numMultiStarts))
}

getExitMessage <- function(x) {
  codes <- getStatusCodes()
  return(codes$message[which(codes$code == x$status)])
}
