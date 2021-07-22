# ============================================================================
# Other functions
# ============================================================================

#' View a description the nloptr status codes
#'
#' Prints a description of the status codes from the nloptr optimization routine.
#' @keywords logitr nloptr status codes
#' @details
#' Code | Description
#' ---|------------------------------------------------------
#' 1 |  Generic success return value.
#' 2 |  Optimization stopped because stopval was reached.
#' 3 |  Optimization stopped because ftol_rel or ftol_abs was reached.
#' 4 |  Optimization stopped because xtol_rel or xtol_abs was reached.
#' 5 |  Optimization stopped because maxeval was reached.
#' 6 |  Optimization stopped because maxtime was reached.
#' -1 | Generic failure code.
#' -2 | Invalid arguments (e.g. lower bounds are bigger than upper bounds,
#' an unknown algorithm was specified, etc.).
#' -3 | Ran out of memory.
#' -4 | Halted because roundoff errors limited progress (in this case, the optimization still typically returns a useful result.).
#' -5 | Halted because of a forced termination: the user called `nlopt_force_stop(opt)` on the optimization's nlopt_opt object opt the user's objective function or constraints.
#' @return No return value; prints a summary of the `nloptr` status codes to
#' the console.
#' @export
#' @examples
#' statusCodes()
statusCodes <- function() {
  cat("Status codes:", "\n", sep = "")
  cat("1:  Generic success return value.", "\n", sep = "")
  cat("2:  Optimization stopped because stopval was reached.", "\n", sep = "")
  cat("3:  Optimization stopped because ftol_rel or ftol_abs was reached.",
    "\n",
    sep = ""
  )
  cat("4:  Optimization stopped because xtol_rel or xtol_abs was reached.",
    "\n",
    sep = ""
  )
  cat("5:  Optimization stopped because maxeval was reached.", "\n", sep = "")
  cat("6:  Optimization stopped because maxtime was reached.", "\n", sep = "")
  cat("-1: Generic failure code.", "\n", sep = "")
  cat("-2: Invalid arguments (e.g. lower bounds are bigger than upper ",
    "bounds, an unknown algorithm was specified, etc.).", "\n",
    sep = ""
  )
  cat("-3: Ran out of memory.", "\n", sep = "")
  cat("-4: Halted because roundoff errors limited progress. (In this case, ",
    "the optimization still typically returns a useful result.)", "\n",
    sep = ""
  )
  cat("-5: Halted because of a forced termination: the user called ",
    "nlopt_force_stop(opt) on the optimization's nlopt_opt object opt ",
    "from the user's objective function or constraints.", "\n",
    sep = ""
  )
}

makeBlankModel <- function(modelInputs) {
  result <- structure(list(
    coef             = NA,
    standErrs        = NA,
    logLik           = NA,
    nullLogLik       = NA,
    gradient         = NA,
    hessian          = NA,
    covariance       = NA,
    numObs           = NA,
    numParams        = NA,
    call             = NA,
    freq             = NA,
    iterations       = NA,
    message          = NA,
    standardDraws    = NA,
    status           = -99,
    modelType        = modelInputs$modelType,
    modelSpace       = modelInputs$modelSpace,
    priceName        = modelInputs$priceName,
    parNames         = modelInputs$parNames,
    randPars         = modelInputs$randPars,
    parSetup         = modelInputs$parSetup,
    weightsUsed      = modelInputs$weightsUsed,
    clusterName      = modelInputs$clusterName,
    numClusters      = modelInputs$numClusters,
    robust           = modelInputs$robust,
    standardDraws    = NA,
    options          = options
  ),
  class = "logitr"
  )
  return(result)
}

# R equivalent of matlab's repmat function
repmat <- function(X, m, n) {
  mx <- dim(X)[1]
  nx <- dim(X)[2]
  return(matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = T))
}

# Replicates matrix mat n times by row
repmatRow <- function(mat, n) {
  return(mat[rep(seq(nrow(mat)), n), ])
}

# Replicates each row of matrix mat n times
repmatRowEach <- function(mat, n) {
  return(mat[rep(seq(nrow(mat)), each = n), ])
}

# Replicates matrix mat n times by column
repmatCol <- function(mat, n) {
  return(mat[, rep(seq(ncol(mat)), n)])
}

# Replicates each column of matrix mat n times
repmatColEach <- function(mat, n) {
  return(mat[, rep(seq(ncol(mat)), each = n)])
}

# Converts seconds into hours, minutes, and seconds
# (modified from the gmnl package)
convertTime <- function(time) {
  et <- time["elapsed"]
  if (et < 1) {
    s <- round(et, 2)
  } else {
    s <- round(et, 0)
  }
  h <- s %/% 3600
  s <- s - 3600 * h
  m <- s %/% 60
  s <- s - 60 * m
  return(paste(h, "h:", m, "m:", s, "s", sep = ""))
}

# Returns a confidence interval from a vector of data
ci <- function(data, alpha = 0.025) {
  B <- mean(data, na.rm = T)
  L <- stats::quantile(data, alpha, na.rm = T)
  U <- stats::quantile(data, 1 - alpha, na.rm = T)
  ests <- c(B, L, U)
  names(ests) <- c("mean", "low", "high")
  return(ests)
}

# Class check functions
is_logitr <- function(x) {
  inherits(x, "logitr")
}

isMxlModel <- function(parSetup) {
  return(("n" %in% parSetup) | ("ln" %in% parSetup))
}

# Functions for getting specific parameter indexes
getFixedParIDs <- function(parSetup) {
  return(which(parSetup == "f"))
}

getRandParIDs <- function(parSetup) {
  return(which(parSetup != "f"))
}

getNormParIDs <- function(parSetup) {
  return(which(parSetup == "n"))
}

getLogNormParIDs <- function(parSetup) {
  return(which(parSetup == "ln"))
}
