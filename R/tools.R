# ============================================================================
# Other functions
# ============================================================================

#' Creates dummy-coded variables.
#'
#' Use this function to create dummy-coded variables in a data frame.
#' @param df A data frame.
#' @param vars The variables in the data frame for which you want to
#' create new dummy coded variables.
#' @export
#' @examples
#' # Create an example data frame:
#' df <- data.frame(
#'     animal   = c("dog", "goldfish", "bird", "dog", "goldfish"),
#'     numLegs  = c(4, 0, 2, 4, 0),
#'     lifeSpan = c(10, 10, 5, 10, 10))
#'
#' # Create dummy coded variables for the variables "animal" and "numLegs":
#' df_dummy <- dummyCode(df, vars = c("animal", "numLegs"))
#' df_dummy
#'
dummyCode = function(df, vars) {
    df = as.data.frame(df)
    nonVars = colnames(df)[which(! colnames(df) %in% vars)]
    # Keep the original variables and the order to restore later after merging
    df$order = seq(nrow(df))
    for (i in 1:length(vars)) {
        var      = vars[i]
        colIndex = which(colnames(df) == var)
        levels   = sort(unique(df[,colIndex]))
        mergeMat = as.data.frame(diag(length(levels)))
        mergeMat = cbind(levels, mergeMat)
        colnames(mergeMat) = c(var, paste(var, levels, sep='_'))
        df = merge(df, mergeMat)
    }
    # Restore the original column order
    new = colnames(df)[which(! colnames(df) %in% c(vars, nonVars))]
    df = df[c(nonVars, vars, new)]
    # Restore the original row order
    df = df[order(df$order),]
    row.names(df) = df$order
    df$order <- NULL
    return(df)
}

#' View a description the nloptr status codes
#'
#' Prints a description of the status codes from the nloptr optimization routine.
#' @keywords logitr, nloptr, status codes
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
#' -2 | Invalid arguments (e.g. lower bounds are bigger than upper bounds, an unknown algorithm was specified, etc.).
#' -3 | Ran out of memory.
#' -4 | Halted because roundoff errors limited progress (in this case, the optimization still typically returns a useful result.).
#' -5 | Halted because of a forced termination: the user called `nlopt_force_stop(opt)` on the optimization's nlopt_opt object opt the user's objective function or constraints.
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
is_logitr_multistart <- function(x) {
  inherits(x, "logitr.multistart")
}
is_logitr_allRuns <- function(x) {
  inherits(x, "logitr.allRuns")
}

allRunsCheck <- function(model) {
  if (is_logitr_allRuns(model)) {
    model <- useBestModel(model)
  }
  return(model)
}

# Return the best model, and print a warning statement
useBestModel <- function(model) {
  model <- model$bestModel
  modelRun <- paste(model$multistartNumber, "of",
    model$options$numMultiStarts,
    sep = " "
  )
  cat(paste(
    "**Using results for model ", modelRun, ",\n",
    "the best model (largest log-likelihood) from the multistart**",
    "\n",
    sep = ""
  ))
  return(model)
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

isMxlModel <- function(parSetup) {
  return(("n" %in% parSetup) | ("ln" %in% parSetup))
}
