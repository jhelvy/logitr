# ============================================================================
# Methods for printing, summarizing, and doing other computations with
# logitr class objects
# ============================================================================

#' Methods for logitr objects
#'
#' Miscellaneous methods for `logitr` class objects.
#'
#' @name miscmethods.logitr
#' @aliases print.logitr logLik.logitr coef.logitr coef.summary.logitr
#' vcov.logitr summary.logitr print.summary.logitr
#'
#' @param x is an object of class `logitr`.
#' @param digits the number of digits for printing, defaults to `3`.
#'
#' @rdname miscmethods.logitr
#' @export
print.logitr <- function(x, digits = max(3, getOption("digits") - 2)) {
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  modelType <- ifelse(x$modelType == "mnl", "Multinomial Logit", "Mixed Logit")
  modelSpace <- ifelse(
    x$modelSpace == "pref",
    "Preference", "Willingness-to-Pay")
  cat("A", modelType, "model estimated in the", modelSpace, "space\n\n")
  if (nrow(x$multistartSummary) > 1) {
    printMutlistartMessage(x)
    cat("\n")
  }
  # Print coefficients
  if (length(coef(x))) {
      cat("Coefficients:\n")
      print.default(
        format(coef(x), digits = digits), print.gap = 2, quote = FALSE)
  } else {
    cat("No coefficients\n")
  }
  # Print log-likelihood
  print(logLik(x))
  cat("\n")
  invisible(x)
}

#' @rdname miscmethods.logitr
#' @export
logLik.logitr <- function(x) {
    x$logLik
}

#' @rdname miscmethods.logitr
#' @export
coef.logitr <- function(object) {
    return(object$coef)
}

#' @rdname miscmethods.logitr
#' @method coef summary.logitr
#' @export
coef.summary.logitr <- function(object) {
    return(object$coefTable)
}

#' @rdname miscmethods.logitr
#' @export
vcov.logitr <- function(object) {
    return(object$covariance)
}

#' @rdname miscmethods.logitr
#' @export
summary.logitr <- function (object) {
    object$modelInfoTable <- getModelInfoTable(object)
    object$coefTable <- getCoefTable(object$coef, object$standErrs)
    object$statTable <- getStatTable(object)
    if (object$modelType == "mxl") {
        object$randParSummary <- getRandParSummary(object)
    }
    class(object) <- c("summary.logitr", "logitr")
    return(object)
}

#' @rdname miscmethods.logitr
#' @method print summary.logitr
#' @export
print.summary.logitr <- function(x, digits = max(3, getOption("digits") - 2)) {
  cat("=================================================", "\n", sep = "")
  cat("Call:\n")
  print(x$call)
  cat("\n")
  cat("Frequencies of alternatives:\n")
  print(prop.table(x$freq), digits = digits)
  # Print multistart summary
  if (nrow(x$multistartSummary) > 1) {
    cat("\n")
    printMutlistartSummary(x)
  }
  print(x$modelInfoTable)
  cat("\n")
  cat("Model Coefficients:", "\n")
  printCoefmat(x$coefTable, digits = digits)
  cat("\n")
  cat("Model Fit:", "\n")
  print(x$statTable)
  if (x$modelType == "mxl") {
      cat("\n")
      cat("Summary of 10k Draws for Random Coefficients:", "\n")
      print(x$randParSummary)
  }
  invisible(x)
}

getModelInfoTable <- function(model) {
  modelSpace <- "Preference"
  if (model$modelSpace == "wtp") {
    modelSpace <- "Willingness-to-Pay"
  }
  modelRun <- paste(model$multistartNumber, "of",
    model$options$numMultiStarts, sep = " "
  )
  modelTime <- convertTime(model$time)
  modelInfoTable <- data.frame(c(
    modelSpace, modelRun, model$iterations,
    modelTime, model$status, model$weightsUsed
  ))
  colnames(modelInfoTable) <- ""
  row.names(modelInfoTable) <- c(
    "Model Space:", "Model Run:", "Iterations:",
    "Elapsed Time:", "Exit Status:", "Weights Used?:"
  )
  if (!is.null(model$robust)) { # Added for backwards compatibility
    modelInfoTable <- rbind(modelInfoTable, model$robust)
    row.names(modelInfoTable)[nrow(modelInfoTable)] <- "robust?"
  }
  if (!is.null(model$numClusters)) { # Added for backwards compatibility
    if (model$numClusters > 0) {
      modelInfoTable <- rbind(modelInfoTable, model$clusterName)
      row.names(modelInfoTable)[nrow(modelInfoTable)] <- "Cluster Name:"
    }
  }
  return(modelInfoTable)
}

getCoefTable <- function(coef, standErrs) {
    z <- coef / standErrs
    p <- 2 * (1 - pnorm(abs(z)))
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
  betaDraws <- makeBetaDraws(coef(object), parSetup, 10^4, standardDraws)
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

printMutlistartMessage <- function(x) {
  cat(
    "Results below are from run", x$multistartNumber, "of",
    x$options$numMultiStarts, "multistart runs\n",
    "as it had the largest log-likelihood value\n"
  )
}

printMutlistartSummary <- function(x) {
    cat("Summary Of Multistart Runs:\n")
    print(x$multistartSummary)
    cat("\n")
    cat("Use statusCodes() to view the meaning of each status code\n")
}
