# ============================================================================
# Methods for printing, summarizing, and doing other computations with
# logitr class objects
# ============================================================================

#' Methods for logitr objects
#'
#' Miscellaneous methods for `logitr` class objects.
#'
#' @name miscmethods.logitr
#' @aliases print.logitr logLik.logitr summary.logitr print.summary.logitr
#' coef.logitr coef.summary.logitr
#' @param x is an object of class `logitr`.
#' @param digits the number of digits for printing, defaults to `3`.
#' @param ... further arguments.
#'
#' @rdname miscmethods.logitr
#' @export
print.logitr <- function(x, digits = max(3, getOption("digits") - 2)) {
  printCall(x)
  printCoefs(x, digits)
  print(logLik(x))
  cat("\n")
  invisible(x)
}

#' @rdname miscmethods.logitr
#' @export
print.multistart <- function(x, digits = max(3, getOption("digits") - 2)) {
  printCall(x)
  printMutlistartMessage(x)
  printCoefs(x, digits)
  print(logLik(x))
  cat("\n")
  invisible(x)
}

printCall <- function(x) {
    cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
}

printMutlistartMessage <- function(x) {
  cat(
    "Results below are from run", x$multistartNumber, "of",
    model$options$numMultiStarts, "multistart runs as it has the ",
    "largest log-likelihood value\n\n"
  )
}

printCoefs <- function(x, digits) {
    if (length(coef(x))) {
        cat("Coefficients:\n")
        print.default(
          format(coef(x), digits = digits), print.gap = 2, quote = FALSE)
    }
    else cat("No coefficients\n")
}

#' @rdname miscmethods.logitr
#' @export
logLik.logitr <- function(x) {
    x$logLik
}

#' @rdname miscmethods.logitr
#' @export
coef.logitr <- function(object) {
    object <- useBestModel(object)
    return(object$coef)
}

#' @rdname miscmethods.logitr
#' @method coef summary.logitr
#' @export
coef.summary.logitr <- function(object) {
    return(object$coefTable)
}

#' @rdname miscmethods.logitr
#' @method coef summary.multistart
#' @export
coef.summary.multistart <- function(object) {
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
    object <- makeSummaryObject(object)
    class(object) <- c("summary.logitr", "logitr")
    return(object)
}

#' @rdname miscmethods.logitr
#' @export
summary.multistart <- function (object) {
    object <- makeSummaryObject(object)
    class(object) <- c("summary.multistart", "multistart")
    return(object)
}

makeSummaryObject <- function (object) {
    object$modelInfoTable <- getModelInfoTable(object)
    object$coefTable <- getCoefTable(object)
    object$statTable <- getStatTable(object)
    if (! is.null(object$randPars)){
        object$randParSummary <- getRandParSummary(object)
    }
    return(object)
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

#' Get the coefficient summary table as a data frame
#'
#' Returns a data frame of the coefficient summary table for a `logitr` class
#' object.
#' @param x is an object of class `logitr`.
#' @return Returns a data frame of the coefficient summary table of a model
#' estimated using the `logitr()` function.
#' @export
#' @examples
#' library(logitr)
#'
#' # Run a MNL model in the preference space
#' mnl_pref <- logitr(
#'   data = yogurt,
#'   choiceName = "choice",
#'   obsIDName = "obsID",
#'   parNames = c("price", "feat", "dannon", "hiland", "yoplait")
#' )
#'
#' # Get the coefficient summary table as a data frame
#' getCoefTable(mnl_pref)
getCoefTable <- function(object) {
    b <- coef(object)
    std.err <- sqrt(diag(vcov(object)))
    z <- b / std.err
    p <- 2 * (1 - pnorm(abs(z)))
    coefTable <- cbind(b, std.err, z, p)
    colnames(coefTable) <- c("Estimate", "Std. Error", "z-value", "Pr(>|z|)")
    return(coefTable)
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

#' @rdname miscmethods.logitr
#' @method print summary.logitr
#' @export
print.summary.logitr <- function(x, digits = max(3, getOption("digits") - 2)) {
    printLine()
    printSummary(x, digits)
}

#' @rdname miscmethods.logitr
#' @method print summary.multistart
#' @export
print.summary.multistart <- function(
  x, digits = max(3, getOption("digits") - 2)) {
  printLine()
  printMultistartSummary(x)
  printSummary(x, digits)
}

printSummary <- function(x, digits = max(3, getOption("digits") - 2)) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n")
    cat("Frequencies of alternatives:\n")
    print(prop.table(x$freq), digits = digits)
    print(x$modelInfoTable)
    cat("\n")
    cat("Model Coefficients:", "\n")
    printCoefmat(x$coefTable, digits = digits)
    cat("\n")
    cat("Model Fit:", "\n")
    print(x$statTable)
    if (!is.na(x$randParSummary)) {
        cat("\n")
        cat("Summary of 10k Draws for Random Coefficients:", "\n")
        print(x$randParSummary)
    }
    invisible(x)
}

printMultistartSummary <- function(x) {
  printLine()
  cat("SUMMARY OF ALL MULTISTART RUNS:\n\n")
  print(x$multistartSummary)
  cat("\n")
  cat("Use statusCodes() to view the meaning of each status code\n\n")
  printMutlistartMessage(x)
}

printLine <- function() {
  cat("=================================================", "\n", sep = "")
}
