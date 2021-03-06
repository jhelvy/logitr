# ============================================================================
# Functions for printing results and summaries
# ============================================================================

#' Get the model coefficients
#'
#' Returns the coefficients of an estimated model of the 'logitr' class.
#' @keywords logitr coef
#' @param object The output of a model estimated using the `logitr()` function.
#' @param ... other arguments
#' @return A vector of the coefficients from a model estimated using the
#' `logitr()` function.
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' data(yogurt)
#'
#' mnl_pref <- logitr(
#'   data = yogurt,
#'   choiceName = "choice",
#'   obsIDName = "obsID",
#'   parNames = c("price", "feat", "dannon", "hiland", "yoplait")
#' )
#'
#' # Get the model coefficients:
#' coef(mnl_pref)
coef.logitr <- function(object, ...) {
  object <- allRunsCheck(object)
  return(object$coef)
}

#' View summary of estimated model
#'
#' Prints a summary of a model estimated using the `logitr()` function
#' @keywords logitr summary logitr.multistart
#' @param object The output of a model estimated model using the `logitr()`
#' function.
#' @param ... other arguments
#' @return No return value; prints a summary of the model results to the
#' console.
#' @export
#' @examples
#' # Run a MNL model in the Preference Space with a multistart:
#' data(yogurt)
#'
#' mnl_pref <- logitr(
#'   data = yogurt,
#'   choiceName = "choice",
#'   obsIDName = "obsID",
#'   parNames = c("price", "feat", "dannon", "hiland", "yoplait"),
#'   options = list(
#'     numMultiStarts = 5,
#'     keepAllRuns = TRUE
#'   )
#' )
#'
#' # View a summary of the model:
#' summary(mnl_pref)
summary.logitr <- function(object, ...) {
  if (is_logitr(object) == FALSE) {
    stop('Model must be estimated using the "logitr" package')
  }
  if (is_logitr_multistart(object)) {
    printMultistartSummary(object)
  }
  if (is_logitr_allRuns(object)) {
    printModelSummary(object$bestModel)
  } else {
    printModelSummary(object)
  }
}

printLine <- function() {
  cat("=================================================", "\n", sep = "")
}

printMultistartSummary <- function(model) {
  printLine()
  cat("SUMMARY OF ALL MULTISTART RUNS:\n\n")
  print(model$multistartSummary)
  cat("---\n")
  cat("Use statusCodes() to view the meaning of the status codes\n\n")
  if (is_logitr_allRuns(model)) {
    model <- model$bestModel
  }
  cat(
    "Below is the summary of run", model$multistartNumber, "of",
    model$options$numMultiStarts,
    "multistart runs\n(the run with the largest log-likelihood value)\n"
  )
}

printModelSummary <- function(model) {
  coefTable <- getCoefTable(model)
  statTable <- getStatTable(model)
  printLine()
  cat("MODEL SUMMARY:", "\n")
  print(getBasicInfoTable(model))
  cat("\n")
  cat("Model Coefficients:", "\n")
  print(coefTable)
  cat("---", "\n", sep = "")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1", "\n",
    sep = ""
  )
  cat("\n")
  cat("Model Fit Values:", "\n")
  print(statTable)
  if (sum(grepl("_sigma", names(model$coef))) > 0) {
    cat("\n")
    cat("Summary of 10k Draws for Random Coefficients:", "\n")
    print(model$randParSummary)
  }
}

getBasicInfoTable <- function(model) {
  modelSpace <- "Preference"
  if (model$modelSpace == "wtp") {
    modelSpace <- "Willingness-to-Pay"
  }
  modelRun <- paste(model$multistartNumber, "of",
    model$options$numMultiStarts,
    sep = " "
  )
  modelTime <- convertTime(model$time)
  basicInfoSummary <- data.frame(c(
    modelSpace, modelRun, model$iterations,
    modelTime, model$status, model$weightsUsed
  ))
  colnames(basicInfoSummary) <- ""
  row.names(basicInfoSummary) <- c(
    "Model Space:", "Model Run:", "Iterations:",
    "Elapsed Time:", "Exit Status:", "Weights Used?:"
  )
  if (!is.null(model$robust)) { # Added for backwards compatibility
    basicInfoSummary <- rbind(basicInfoSummary, model$robust)
    row.names(basicInfoSummary)[nrow(basicInfoSummary)] <- "robust?"
  }
  if (!is.null(model$numClusters)) { # Added for backwards compatibility
    if (model$numClusters > 0) {
      basicInfoSummary <- rbind(basicInfoSummary, model$clusterName)
      row.names(basicInfoSummary)[nrow(basicInfoSummary)] <- "Cluster Name:"
    }
  }
  return(basicInfoSummary)
}

#' Get the coefficient summary table as a data frame
#'
#' Returns a data frame of the coefficient summary table of a model estimated
#' using the `logitr()` function.
#' @keywords logitr summary coefTable
#' @param object The output of a model estimated model using the `logitr()`
#' function.
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
  coefTable <- getCoefSummaryTable(
    coef      = object$coef,
    se        = object$standErrs,
    numObs    = object$numObs,
    numParams = object$numParams
  )
  return(coefTable)
}

getCoefSummaryTable <- function(coef, se, numObs, numParams) {
  tStat <- rep(NA, length(coef))
  pVal <- rep(NA, length(coef))
  signif <- rep("", length(coef))
  if (sum(is.na(se)) == 0) {
    tStat <- as.numeric(coef / se)
    dof <- numObs - numParams
    pVal <- 2 * (1 - stats::pt(abs(tStat), dof))
    signif <- getSignifCodes(pVal)
  }
  coefTable <- data.frame(
    Estimate = round(coef, 6),
    StdError = round(se, 6),
    tStat = round(tStat, 4),
    pVal = round(pVal, 4),
    signif = signif
  )
  row.names(coefTable) <- names(coef)
  return(coefTable)
}

getSignifCodes <- function(pVal) {
  signif <- rep("", length(pVal))
  signif[which(pVal <= 0.001)] <- "***"
  signif[which(pVal > 0.001 & pVal <= 0.01)] <- "**"
  signif[which(pVal > 0.01 & pVal <= 0.05)] <- "*"
  signif[which(pVal > 0.05 & pVal <= 0.1)] <- "."
  return(signif)
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
