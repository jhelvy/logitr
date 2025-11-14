#' Methods for logitr objects
#'
#' Miscellaneous methods for `logitr` class objects.
#'
#' @name miscmethods.logitr
#' @aliases logLik.logitr terms.logitr coef.logitr coef.summary.logitr
#' summary.logitr print.logitr print.summary.logitr
#' @param x is an object of class `logitr`.
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param digits the number of digits for printing, defaults to `3`.
#' @param width the width of the printing.
#' @param ... further arguments.
#'
#' @rdname miscmethods.logitr
#' @export
logLik.logitr <- function(object, ...) {
  return(structure(
    object$logLik,
    df = object$n$pars,
    null = sum(object$freq * log(object$freq / object$n$obs)),
    class = "logLik"
  ))
}

#' @rdname miscmethods.logitr
#' @export
terms.logitr <- function(x, ...) {
  return(stats::terms(x$formula))
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
summary.logitr <- function(object, ...) {
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
print.logitr <- function(
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
  if (x$n$multiStarts > 1) {
    modelRun <- getModelRun(x)
    cat(
      "Results below are from run",
      modelRun,
      "multistart runs\n",
      "as it had the largest log-likelihood value\n"
    )
    cat("\n")
  }
  # Print coefficients & log-likelihood
  if (!any(is.na(stats::coef(x)))) {
    cat("Coefficients:\n")
    print.default(
      format(x$coefficients, digits = digits),
      print.gap = 2,
      quote = FALSE
    )
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
  if (is.null(x$date)) {
    x$date <- "date missing"
  }
  if (is.null(x$version)) {
    x$date <- "version missing"
  }
  cat("\nModel estimated on:", x$date, "\n")
  cat("\nUsing logitr version:", x$version, "\n\n")
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
    modelType,
    modelSpace,
    modelRun,
    object$iterations,
    modelTime,
    algorithm,
    object$weightsUsed
  ))
  colnames(modelInfoTable) <- ""
  row.names(modelInfoTable) <- c(
    "Model Type:",
    "Model Space:",
    "Model Run:",
    "Iterations:",
    "Elapsed Time:",
    "Algorithm:",
    "Weights Used?:"
  )
  panelID <- object$inputs$panelID
  if (!is.null(panelID)) {
    modelInfoTable <- rbind(modelInfoTable, panelID)
    row.names(modelInfoTable)[nrow(modelInfoTable)] <- "Panel ID:"
  }
  if (!is.null(object$n$clusters)) {
    if (object$n$clusters > 0) {
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
  bic <- round(log(object$n$obs) * object$n$pars - 2 * object$logLik, 4)
  mcR2 <- 1 - (object$logLik / object$nullLogLik)
  adjMcR2 <- 1 - ((object$logLik - object$n$pars) / object$nullLogLik)
  statTable <- data.frame(c(
    object$logLik,
    object$nullLogLik,
    aic,
    bic,
    mcR2,
    adjMcR2,
    object$n$obs
  ))
  colnames(statTable) <- ""
  row.names(statTable) <- c(
    "Log-Likelihood:",
    "Null Log-Likelihood:",
    "AIC:",
    "BIC:",
    "McFadden R2:",
    "Adj McFadden R2:",
    "Number of Observations:"
  )
  if (!is.null(object$n$clusters)) {
    # Added for backwards compatibility
    if (object$n$clusters > 0) {
      statTable <- rbind(statTable, object$n$clusters)
      row.names(statTable)[nrow(statTable)] <- "Number of Clusters"
    }
  }
  return(statTable)
}

getRandParSummary <- function(object) {
  parSetup <- object$parSetup
  parIDs <- object$parIDs
  n <- object$n
  n$draws <- 10^4
  standardDraws <- getStandardDraws(parIDs, n$draws, 'halton')
  betaDraws <- makeBetaDraws(
    stats::coef(object),
    parIDs,
    n,
    standardDraws,
    object$inputs$correlation
  )
  randParSummary <- apply(betaDraws, 2, summary)
  # Add names to summary
  nIDs <- parIDs$n
  lnIDs <- parIDs$ln
  cnIDs <- parIDs$cn
  distName <- rep("", length(parSetup))
  distName[nIDs] <- "normal"
  distName[lnIDs] <- "log-normal"
  distName[cnIDs] <- "zero-censored normal"
  summaryNames <- paste(names(parSetup), " (", distName, ")", sep = "")
  colnames(randParSummary) <- summaryNames
  randParSummary <- as.data.frame(t(randParSummary))
  # Set min and max values for unbounded distributions
  if (length(nIDs) > 0) {
    randParSummary[nIDs, ]$Min. <- -Inf
    randParSummary[nIDs, ]$Max. <- Inf
  }
  if (length(lnIDs) > 0) {
    randParSummary[lnIDs, ]$Min. <- 0
    randParSummary[lnIDs, ]$Max. <- Inf
  }
  if (length(cnIDs) > 0) {
    randParSummary[cnIDs, ]$Max. <- Inf
  }
  # Add names and drop fixed pars
  randParSummary <- randParSummary[parIDs$r, ]
  row.names(randParSummary) <- names(parIDs$r)
  return(randParSummary)
}

getModelType <- function(x) {
  return(ifelse(x$modelType == "mnl", "Multinomial Logit", "Mixed Logit"))
}

getModelSpace <- function(x) {
  return(ifelse(
    x$modelSpace == "pref",
    "Preference",
    "Willingness-to-Pay"
  ))
}

getModelRun <- function(x) {
  return(paste(x$multistartNumber, "of", x$n$multiStarts))
}

getExitMessage <- function(x) {
  codes <- getStatusCodes()
  return(codes$message[which(codes$code == x$status)])
}

#' Extract standard errors
#'
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param ... further arguments.
#' @export
se <- function(object, ...) {
  UseMethod("se")
}

#' Extract standard errors
#'
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param ... further arguments.
#' @export
se.logitr <- function(object, ...) {
  return(sqrt(diag(stats::vcov(object))))
}

#' Calculate the variance-covariance matrix
#'
#' Returns the variance-covariance matrix of the main parameters of a fitted
#' model object.
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param ... further arguments.
#' @export
vcov.logitr <- function(object, ...) {
  if (!is.null(object$vcov)) {
    # vcov was already computed during model estimation
    return(object$vcov)
  }
  if (is.null(object$data$clusterID) | object$inputs$robust == FALSE) {
    return(getCovarianceNonRobust(object$hessian))
  }
  return(getCovarianceRobust(object))
}

getCovarianceNonRobust <- function(hessian) {
  covariance <- hessian * NA
  tryCatch(
    {
      covariance <- solve(-1 * hessian)
    },
    error = function(e) {}
  )
  return(covariance)
}

getCovarianceRobust <- function(object) {
  numClusters <- object$n$clusters
  inputs <- object$inputs
  parSetup <- object$parSetup
  modelInputs <- list(
    logitFuncs = setLogitFunctions(object$modelSpace),
    evalFuncs = setEvalFunctions(object$modelType, inputs$useAnalyticGrad),
    inputs = inputs,
    modelType = object$modelType,
    modelSpace = object$modelSpace,
    n = object$n,
    parSetup = parSetup,
    parIDs = object$parIDs,
    panel = !is.null(inputs$panelID),
    standardDraws = object$standardDraws,
    data_diff = makeDiffData(object$data, object$modelType)
  )
  clusterID <- modelInputs$data_diff$clusterID
  clusters <- unique(clusterID)
  pars <- stats::coef(object)
  gradMat <- matrix(NA, nrow = numClusters, ncol = length(pars))
  for (i in seq_len(length(clusters))) {
    indices <- which(clusterID == i)
    tempMI <- getClusterModelInputs(indices, modelInputs, i)
    gradMat[i, ] <- -1 * modelInputs$evalFuncs$negGradLL(pars, tempMI)
  }
  gradMeanMat <- repmat(matrix(colMeans(gradMat), nrow = 1), numClusters, 1)
  diffMat <- gradMat - gradMeanMat
  M <- t(diffMat) %*% diffMat
  M <- M * (numClusters / (numClusters - 1)) # small sample correction
  D <- getCovarianceNonRobust(object$hessian)
  if (any(is.na(D))) {
    return(D)
  } # If there are NAs the next line will error
  return(D %*% M %*% D)
}

getClusterModelInputs <- function(indices, mi, i) {
  X <- mi$data_diff$X[indices, ]
  # Cast to matrix in cases where there is 1 independent variable
  if (length(indices) == 1) {
    X <- matrix(X, nrow = 1)
  }
  mi$data_diff$X <- X
  mi$data_diff$scalePar <- mi$data_diff$scalePar[indices]
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
    mi$partials <- makePartials(mi, mi$data_diff)
  }
  mi$n$rowX <- nrow(X)
  return(mi)
}

#' @rdname miscmethods.logitr
#' @export
print.logitr_wtp <- function(
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
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param probs Predicted probabilities for an object of class `logitr` to use
#' in computing fitted values Defaults to `NULL`.
#' @param ... further arguments.
#'
#' @return A data frame of the `obsID` and the fitted values extracted from
#' `object`.
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
#' # Extract the fitted values from the model
#' fitted(mnl_pref)
fitted.logitr <- function(object, probs = NULL, ...) {
  if (is.null(probs)) {
    probs <- stats::predict(object, type = "prob")
  }
  outcome <- object$data$outcome
  fitted <- probs[which(outcome == 1), ]
  names(fitted)[which(names(fitted) == 'predicted_prob')] <- "fitted_value"
  return(fitted)
}

#' Extract Model Residuals
#'
#' Returns model residuals from an object of class `logitr`.
#' @keywords logitr residuals resid
#'
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param fitted Fitted values for an object of class `logitr` to use in
#' computing residuals. Defaults to `NULL`.
#' @param ... further arguments.
#'
#' @return A data frame of the `obsID` and the residuals (response minus fitted
#' values) extracted from `object`.
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
#' # Extract the residuals from the model
#' residuals(mnl_pref)
residuals.logitr <- function(object, fitted = NULL, ...) {
  if (is.null(fitted)) {
    fitted <- stats::fitted(object)
  }
  reps <- table(object$data$obsID)
  residuals <- fitted[rep(seq_along(reps), reps), ]
  resids <- object$data$outcome - residuals$fitted_value
  residuals$fitted_value <- NULL
  residuals$residual <- as.vector(resids)
  return(residuals)
}

#' Extract Model Confidence Interval
#'
#' Returns confidence intervals from an object of class `logitr`.
#' @keywords logitr confint
#'
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param parm A specification of which parameters are to be given confidence
#' intervals, either a vector of numbers or a vector of names.
#' If missing, all parameters are considered.
#' @param level The confidence level required.
#' @param ... further arguments.
#'
#' @return A data frame of the confidence intervals of model coefficients.
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
#' # Compute a confidence interval
#' confint(mnl_pref)
confint.logitr <- function(object, parm, level = 0.95, ...) {
  draws <- getUncertaintyDraws(object, numDraws = 10^4)
  lower <- (1 - level) / 2
  upper <- 1 - lower
  df <- data.frame(
    lower = apply(draws, 2, function(x) fquantile(x, lower, na.rm = TRUE)),
    upper = apply(draws, 2, function(x) fquantile(x, upper, na.rm = TRUE))
  )
  names(df) <- c(paste(lower * 100, "%"), paste(upper * 100, "%"))
  return(df)
}

#' Construct Design Matrices
#'
#' Creates a design (or model) matrix, e.g., by expanding factors to a set of
#' dummy variables (depending on the contrasts) and expanding interactions
#' similarly.
#' @keywords logitr model.matrix
#'
#' @param object an object of an appropriate class. For the default method,
#' a model `formula` or a `terms` object.
#' @param ... further arguments.
#'
#' @return A design matrix
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
#' # Get the model.matrix design matrix
#' model.matrix(mnl_pref)
model.matrix.logitr <- function(object, ...) {
  return(object$data$X)
}

#' Extracting the Model Frame from a Formula or Fit
#'
#' Returns a data.frame with the variables needed to use formula and
#' any `...` arguments.
#' @keywords logitr model.frame
#'
#' @param formula a model `formula` or `terms` object or an R object.
#' @param ... further arguments.
#'
#' @return A data.frame with the variables needed to use formula and
#' any `...` arguments.
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
#' # Get the model.frame data frame
#' model.frame(mnl_pref)
model.frame.logitr <- function(formula, ...) {
  stats::model.frame.default(
    formula$formula,
    data = eval(formula$call$data, envir = parent.frame())
  )
}

#' @rdname miscmethods.logitr
#' @export
print.logitr_validation <- function(x, ...) {
  cat("=== LOGITR DATA VALIDATION ===\n\n")

  # Basic data info
  cat("Data Overview:\n")
  cat("  Rows:", x$data_info$nrows, "\n")
  cat("  Columns:", x$data_info$ncols, "\n")
  cat("  Outcome variable:", x$data_info$outcome, "\n")
  cat("  Observation ID:", x$data_info$obsID, "\n")
  if (!is.null(x$data_info$pars)) {
    cat("  Parameters:", paste(x$data_info$pars, collapse = ", "), "\n")
  }
  if (!is.null(x$data_info$scalePar)) {
    cat("  Scale parameter:", x$data_info$scalePar, "\n")
  }
  if (!is.null(x$data_info$panelID)) {
    cat("  Panel ID:", x$data_info$panelID, "\n")
  }

  # Summary statistics
  if (!is.null(x$summary$total_observations)) {
    cat("\nData Structure:\n")
    cat("  Total observations:", x$summary$total_observations, "\n")
    cat("  Total alternatives:", x$summary$total_alternatives, "\n")
    cat("  Valid choices:", x$summary$valid_choices %||% "Unknown", "\n")

    if (!is.null(x$summary$alternatives_per_obs)) {
      cat("  Alternatives per observation:\n")
      alt_table <- x$summary$alternatives_per_obs
      for (i in seq_along(alt_table)) {
        cat(
          "    ",
          names(alt_table)[i],
          "alternatives:",
          alt_table[i],
          "observations\n"
        )
      }
    }

    if (!is.null(x$summary$individuals)) {
      cat("  Panel structure:\n")
      cat("    Individuals:", x$summary$individuals, "\n")
      if (!is.null(x$summary$obs_per_individual)) {
        obs_summary <- x$summary$obs_per_individual
        cat(
          "    Observations per individual: Min =",
          obs_summary["Min."],
          ", Max =",
          obs_summary["Max."],
          ", Mean =",
          round(obs_summary["Mean"], 1),
          "\n"
        )
      }
    }
  }

  # Parameter information
  if (!is.null(x$summary$parameter_info)) {
    cat("\nParameter Information:\n")
    for (par_name in names(x$summary$parameter_info)) {
      par_info <- x$summary$parameter_info[[par_name]]
      cat("  ", par_name, "(", par_info$type, "):")

      if (par_info$type %in% c("character", "factor")) {
        cat(" ", par_info$n_levels, "levels")
        if (par_info$n_levels <= 5) {
          cat(" -", paste(par_info$unique_values, collapse = ", "))
        }
      } else if (par_info$type == "numeric") {
        cat(
          " range [",
          round(par_info$range[1], 3),
          ",",
          round(par_info$range[2], 3),
          "]"
        )
      }

      if (par_info$na_count > 0) {
        cat(" (", par_info$na_count, "missing)")
      }
      cat("\n")
    }
  }

  # Scale parameter information
  if (!is.null(x$summary$scalePar_info)) {
    cat("\nScale Parameter Information:\n")
    scalePar_info <- x$summary$scalePar_info
    cat("  ", scalePar_info$name, "(", scalePar_info$type, "):")
    cat(
      " range [",
      round(scalePar_info$range[1], 3),
      ",",
      round(scalePar_info$range[2], 3),
      "]"
    )
    if (scalePar_info$na_count > 0) {
      cat(" (", scalePar_info$na_count, "missing)")
    }
    cat("\n")
  }

  # Results section
  cat("\n=== VALIDATION RESULTS ===\n")

  if (length(x$errors) == 0 && length(x$warnings) == 0) {
    cat("\u2713 Data validation PASSED - no issues found!\n")
    cat("\u2713 Data appears ready for use with logitr()!\n")
  } else {
    # Print errors
    if (length(x$errors) > 0) {
      cat("\u2717 ERRORS found:\n")
      for (i in seq_along(x$errors)) {
        cat("  ", i, ".", x$errors[i], "\n")
      }

      # Print detailed diagnostics for errors
      if (!is.null(x$diagnostics$multiple_choice_details)) {
        cat("\n  Detailed locations for multiple choices:\n")
        for (obsID in names(x$diagnostics$multiple_choice_details)) {
          rows <- x$diagnostics$multiple_choice_details[[obsID]]
          cat("    ObsID", obsID, "- rows:", paste(rows, collapse = ", "), "\n")
        }
      }

      if (!is.null(x$diagnostics$no_choice_details)) {
        cat("\n  Detailed locations for no choices:\n")
        for (obsID in names(x$diagnostics$no_choice_details)) {
          rows <- x$diagnostics$no_choice_details[[obsID]]
          cat("    ObsID", obsID, "- rows:", paste(rows, collapse = ", "), "\n")
        }
      }

      if (!is.null(x$diagnostics$noncontiguous_details)) {
        cat("\n  Detailed locations for non-contiguous obsID blocks:\n")
        for (obsID in names(x$diagnostics$noncontiguous_details)) {
          rows <- x$diagnostics$noncontiguous_details[[obsID]]
          cat(
            "    ObsID",
            obsID,
            "appears in rows:",
            paste(rows, collapse = ", "),
            "\n"
          )
        }
      }
    }

    # Print warnings
    if (length(x$warnings) > 0) {
      if (length(x$errors) > 0) {
        cat("\n")
      }
      cat("\u26A0 WARNINGS:\n")
      for (i in seq_along(x$warnings)) {
        cat("  ", i, ".", x$warnings[i], "\n")
      }
    }

    # Final status
    cat("\n")
    if (x$valid) {
      cat("\u2713 Despite warnings, data structure appears valid for logitr().\n")
      cat("  Consider addressing warnings for cleaner results.\n")
    } else {
      cat(" Please fix the errors above before using with logitr().\n")
    }
  }

  invisible(x)
}
