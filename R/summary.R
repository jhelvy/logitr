# ============================================================================
# Functions for printing results and summaries
# ============================================================================

#' Get the model coefficients
#'
#' Returns the coefficients of an estimated model of the 'logitr' class.
#' @keywords logitr, coef
#' @param object The output of a model estimated using the `logitr()` function.
#' @param ... other arguments
#' @return A vector of the coefficients from a model estimated using the `logitr()` function.
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
#' @keywords logitr, summary, logitr.multistart
#' @param object The output of a model estimated model using the `logitr()` function.
#' @param ... other arguments
#' @return Prints a summary of the model results to the console.
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
  if (is.logitr(object) == FALSE) {
    stop('Model must be estimated using the "logitr" package')
  }
  if (is.logitr.multistart(object)) {
    printMultistartSummary(object)
  }
  if (is.logitr.allRuns(object)) {
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
  cat("SUMMARY OF ALL MULTISTART RUNS:", "\n", "\n", sep = "")
  print(model$multistartSummary)
  cat("---", "\n", sep = "")
  cat("To view meaning of status codes, use statusCodes()", "\n")
  cat("\n", sep = "")
  cat("Summary of BEST model below (run with largest",
    "log-likelihood value)",
    sep = " "
  )
  cat("\n", sep = "")
}

printModelSummary <- function(model) {
  coefTable <- getCoefTable(
    model$coef, model$standErrs, model$numObs,
    model$numParams
  )
  statTable <- getStatTable(
    model$logLik, model$nullLogLik, model$numObs,
    model$numParams
  )
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
  if (sum(grepl(".sigma", names(model$coef))) > 0) {
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
    modelTime, model$weightsUsed
  ))
  colnames(basicInfoSummary) <- ""
  row.names(basicInfoSummary) <- c(
    "Model Space:", "Model Run:",
    "Iterations:", "Elapsed Time:", "Weights Used?:"
  )
  return(basicInfoSummary)
}

getCoefTable <- function(coef, se, numObs, numParams) {
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

getStatTable <- function(logLik, nullLogLik, numObs, numParams) {
  aic <- round(2 * numParams - 2 * logLik, 4)
  bic <- round(log(numObs) * numParams - 2 * logLik, 4)
  result <- t(data.frame(
    "Log-Likelihood:"         = logLik,
    "Null Log-Likelihood:"    = nullLogLik,
    "AIC:"                    = aic,
    "BIC:"                    = bic,
    "McFadden R2:"            = 1 - (logLik / nullLogLik),
    "Adj. McFadden R2"        = 1 - ((logLik - numParams) / nullLogLik),
    "Number of Observations:" = numObs
  ))
  colnames(result) <- ""
  return(result)
}
