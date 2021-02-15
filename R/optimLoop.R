# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart <- function(modelInputs) {
  # Setup lists for storing results
  numMultiStarts <- modelInputs$options$numMultiStarts
  models <- list()
  didUserProvidePars <- ! (is.null(modelInputs$options$startVals))
  for (i in 1:numMultiStarts) {
    if (numMultiStarts == 1) {
      message("Running Model...")
    } else {
      message("Running Multistart ", i, " of ", numMultiStarts, "...")
    }
    logLik <- NA
    noFirstRunErr <- TRUE
    while (is.na(logLik)) {
      tryCatch(
        {
          startPars <- getStartPars(
            modelInputs, i, noFirstRunErr, didUserProvidePars)
          model <- runModel(modelInputs, startPars)
          logLik <- model$logLik
          model$multistartNumber <- i
        },
        error = function(e) {
          warning("ERROR: failed to converge...restarting search", failCount)
        }
      )
      if ((i == 1) &
          is.na(logLik) &
          didUserProvidePars) {
        noFirstRunErr <- FALSE
        warning(
          "NOTE: User provided starting values did not converge...",
          "using random starting values now"
        )
      }
    }
    models[[i]] <- model
  }
  return(models)
}

getStartPars <- function(modelInputs, i, noFirstRunErr, didUserProvidePars) {
  startPars <- getRandomStartPars(modelInputs)
  if (i == 1) {
    if (didUserProvidePars &
        noFirstRunErr) {
      message("NOTE: Using user-provided starting values for this run")
      userStartPars <- modelInputs$options$startVals
      if (length(userStartPars) != length(startPars)) {
        stop(paste0(
          "Number of user-provided starting values do not match number ",
          "of model parameters."
        ))
      }
      return(startPars)
    } else if (noFirstRunErr) {
      startPars <- 0 * startPars
    }
  }
  startPars <- checkStartPars(startPars, modelInputs)
  return(startPars)
}

# Returns randomly drawn starting parameters from a uniform distribution
# between modelInputs$options$startParBounds
getRandomStartPars <- function(modelInputs) {
  parNameList <- modelInputs$parNameList
  bounds <- modelInputs$options$startParBounds
  lower <- bounds[1]
  upper <- bounds[2]
  # For mxl models, need both '_mu' and '_sigma' parameters
  pars_mu <- stats::runif(length(parNameList$mu), lower, upper)
  pars_sigma <- stats::runif(length(parNameList$sigma), lower, upper)
  startPars <- c(pars_mu, pars_sigma)
  names(startPars) <- parNameList$all
  return(startPars)
}

# For lambda and logN parameters must start with positive numbers
checkStartPars <- function(startPars, modelInputs) {
  lambdaParIDs <- NULL
  if (modelInputs$modelSpace == "wtp") {
    lambdaParIDs <- which(grepl("lambda", modelInputs$parNameList$all))
  }
  logNParNames <- names(getLogNormParIDs(modelInputs$parSetup))
  logNParIDs <- c()
  if (length(logNParNames) > 0) {
    for (parName in logNParNames) {
      logNParIDs <- c(logNParIDs,
                      which(grepl(parName, modelInputs$parNameList$all)))
    }
  }
  positiveParIDs <- unique(c(lambdaParIDs, logNParIDs))
  if (length(positiveParIDs) > 0) {
    startPars[positiveParIDs] <- stats::runif(
      length(positiveParIDs), 0.01, 0.1)
  }
  return(startPars)
}

# Runs the MNL model
runModel <- function(modelInputs, startPars) {
  startTime <- proc.time()
  model <- nloptr::nloptr(
    x0 = startPars,
    eval_f = modelInputs$evalFuncs$objective,
    modelInputs = modelInputs,
    opts = list(
      "algorithm" = modelInputs$options$algorithm,
      "xtol_rel"  = modelInputs$options$xtol_rel,
      "xtol_abs"  = modelInputs$options$xtol_abs,
      "ftol_rel"  = modelInputs$options$ftol_rel,
      "ftol_abs"  = modelInputs$options$ftol_abs,
      print_level = modelInputs$options$printLevel,
      maxeval     = modelInputs$options$maxeval
    )
  )
  model$startPars <- startPars
  model$logLik <- -1 * model$objective # -1 for (+) rather than (-) LL
  model$time <- proc.time() - startTime
  return(model)
}
