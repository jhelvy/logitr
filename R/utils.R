# ============================================================================
# Other functions
# ============================================================================

#' Predict probabilities and / or outcomes
#'
#' This function is a faster implementation of the "type 7" `quantile()`
#' algorithm and is modified from this gist:
#' https://gist.github.com/sikli/f1775feb9736073cefee97ec81f6b193
#' It returns sample quantiles corresponding to the given probabilities.
#' The smallest observation corresponds to a probability of 0 and the largest
#' to a probability of 1. For speed, output quantile names are removed as are
#' error handling such as checking if x are factors, or if probs lie outside
#' the `[0,1]` range.
#' @param x numeric vector whose sample quantiles are wanted. `NA` and `NaN`
#' values are not allowed in numeric vectors unless `na.rm` is `TRUE`.
#' @param probs numeric vector of probabilities with values in `[0,1]`.
#' (Values up to `2e-14` outside that range are accepted and moved to the
#' nearby endpoint.)
#' @param na.rm logical; if `TRUE`, any `NA` and `NaN`'s are removed from `x`
#' before the quantiles are computed.
#' @return A vector of length `length(probs)` is returned;
#' @export
#' @examples
#' library(logitr)
#'
fquantile <- function(x, probs = seq(0, 1, 0.25), na.rm = FALSE) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)
  index <- 1 + (n - 1) * probs
  lo <- floor(index)
  hi <- ceiling(index)
  x  <- sort(x, partial = unique(c(lo, hi)))
  qs <- x[lo]
  i  <- 1:length(probs)
  h  <- index - lo
  qs <- (1 - h) * qs + h * x[hi]
  return(qs)
}

#' Compute logit fraction for sets of alternatives given coefficient draws
#'
#' Returns a data frame of the predicted probabilities (with a confidence
#' interval) for a data frame of alternatives given coefficient draws.
#' WARNING: Most of the time you probably want to use `predict()` instead of
#' this function. Where `logit_probs()` is useful is if you estimate a model
#' with an interaction parameter to see differences between groups. In those
#' cases, you can obtain draws of the estimated parameters and then use the
#' draws to predict probabilities for each group after summing together the
#' appropriate columns of the draws for each group. Also note that this function
#' is only useful for multinomial logit models and is not appropriate for mixed
#' logit models.
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param coef_draws A data frame of coefficients draws.
#' @param newdata A data frame of sets of alternatives for which to compute
#' logit probabilities. Each row is an alternative.
#' @param obsID The name of the column in `newdata` that identifies each set of
#' alternatives. Defaults to `NULL`, in which case it assumes the newdata
#' are all one choice scenario.
#' @param level The sensitivity of the computed confidence interval (CI).
#' Defaults to `level = 0.95`, reflecting a 95% CI.
#' @export
#' @examples
#' library(logitr)
#'
#' # Estimate a preference space model
#' mnl_pref <- logitr(
#'   data    = yogurt,
#'   outcome = "choice",
#'   obsID   = "obsID",
#'   pars    = c("price", "feat", "brand")
#' )
#'
#' # Create a set of alternatives for which to simulate probabilities
#' # (Columns are attributes, rows are alternatives)
#' data <- data.frame(
#'   altID       = c(1, 2, 3, 4),
#'   obsID       = c(1, 1, 1, 1),
#'   price       = c(8, 6, 7, 10),
#'   feat    = c(0, 1, 0, 0),
#'   brand   = c('dannon', 'hiland', 'weight', 'yoplait')
#' )
#'
#' # Obtain 10,000 draws of parameters from model
#' coefs <- coef(mnl_pref)
#' covariance <- vcov(mnl_pref)
#' coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
#'
#' # Compute the probabilities
#' sim <- logit_probs(
#'   mnl_pref,
#'   coef_draws = coef_draws,
#'   newdata = data,
#'   obsID = 'obsID',
#'   level = 0.95
#' )
#'
logit_probs <- function(object, coef_draws, newdata, obsID = NULL, level = 0.95) {
  if (is.null(obsID)) {
    obsID <- "obsID"
    newdata$obsID <- rep(1, nrow(newdata))
  }
  obsID <- newdata[, obsID]
  recoded <- recodeData(newdata, object$inputs$pars, object$inputs$randPars)
  expV <- exp(recoded$X %*% t(coef_draws))
  sumExpV <- rowsum(expV, group = obsID, reorder = FALSE)
  reps <- table(obsID)
  probs <- expV / sumExpV[rep(seq_along(reps), reps),]
  return(ci(t(probs), level))
}

#' Obtain a confidence interval from coefficient draws
#'
#' Returns a data frame with the columns 'mean', 'lower', and 'upper'
#' reflecting the mean and lower and upper bounds of a confidence
#' interval (quantiles) for every column in a data frame of draws
#' @param df A data frame of draws with all numeric columns.
#' @param level The sensitivity of the computed confidence interval (CI).
#' Defaults to `level = 0.95`, reflecting a 95% CI.
#' @export
#' @examples
#' library(logitr)
#'
#' # Estimate a preference space model
#' mnl_pref <- logitr(
#'   data    = yogurt,
#'   outcome = "choice",
#'   obsID   = "obsID",
#'   pars    = c("price", "feat", "brand")
#' )
#'
#' # Obtain 10,000 draws of parameters from model
#' coefs <- coef(mnl_pref)
#' covariance <- vcov(mnl_pref)
#' coef_draws <- as.data.frame(MASS::mvrnorm(10^4, coefs, covariance))
#'
#' # Compute a confidence interval
#' ci(coef_draws, level = 0.95)
#'
ci <- function(df, level = 0.95) {
  lower <- (1 - level)/2
  upper <- 1 - lower
  df <- data.frame(
    mean  = apply(df, 2, mean, na.rm = TRUE),
    lower = apply(df, 2, function(x) fquantile(x, lower, na.rm = TRUE)),
    upper = apply(df, 2, function(x) fquantile(x, upper, na.rm = TRUE))
  )
  return(df)
}

#' View a description the nloptr status codes
#'
#' Prints a description of the status codes from the nloptr optimization routine.
#' @keywords logitr nloptr status codes
#' @return No return value; prints a summary of the `nloptr` status codes to
#' the console.
#' @export
#' @examples
#' statusCodes()
statusCodes <- function() {
  codes <- getStatusCodes()
  cat("Status codes:", "\n", sep = "")
  for (i in seq_len(nrow(codes))) {
    row <- codes[i,]
    cat(row$code, ": ", row$message, "\n", sep = "")
  }
}

getStatusCodes <- function() {
  result <- data.frame(
    code = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -4, -5),
    message = c(
      "Generic success return value.",
      "Optimization stopped because stopval was reached.",
      "Optimization stopped because ftol_rel or ftol_abs was reached.",
      "Optimization stopped because xtol_rel or xtol_abs was reached.",
      "Optimization stopped because maxeval was reached.",
      "Optimization stopped because maxtime was reached.",
      "Generic failure code.",
      paste(
        "Invalid arguments (e.g. lower bounds are bigger than upper",
        "bounds, an unknown algorithm was specified, etc.)."
      ),
      "Ran out of memory.",
      paste(
        "Halted because roundoff errors limited progress. (In this case,",
        "the optimization still typically returns a useful result.)"
      ),
      paste(
        "Halted because of a forced termination: the user called",
        "nlopt_force_stop(opt) on the optimization's nlopt_opt object opt",
        "from the user's objective function or constraints."
      )
    )
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

# Class check functions
is_logitr <- function(x) {
  inherits(x, "logitr")
}

isMnlModel <- function(parSetup) {
  return(all(parSetup == "f"))
}

isMxlModel <- function(parSetup) {
  return(!isMnlModel(parSetup))
}

checkMatrix <- function(x) {
  if (!is.matrix(x)) { return(as.matrix(x)) }
  return(x)
}

#' Display version number and date when the package is loaded.
#' @importFrom utils packageDescription
#' @noRd
.onAttach <- function(libname, pkgname) {
  desc  <- utils::packageDescription(pkgname, libname)
  packageStartupMessage(
    "Version:  ", desc$Version, "\n",
    "Author:   ", "John Paul Helveston (George Washington University)", "\n\n",
    "Consider submitting praise at\n",
    "https://github.com/jhelvy/logitr/issues/8.\n\n",
    "Please cite the JSS article in your publications, see:\ncitation(\"logitr\")"
  )
}
