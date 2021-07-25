# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart <- function(modelInputs) {
  # Setup lists for storing results
  numMultiStarts <- modelInputs$options$numMultiStarts
  models <- list()
  for (i in 1:numMultiStarts) {
    if (numMultiStarts == 1) {
      message("Running Model...")
    } else {
      message("Running Multistart ", i, " of ", numMultiStarts, "...")
    }
    startTime <- proc.time()
    model <- makeBlankModel(modelInputs)
    startPars <- getStartPars(modelInputs, i)
    tryCatch({
      model <- runModel(modelInputs, startPars)
      },
      error = function(e) { message("ERROR: failed to converge") }
    )
    model$startPars <- startPars
    model$multistartNumber <- i
    model$time <- proc.time() - startTime
    models[[i]] <- model
  }
  return(models)
}

# Runs the MNL model
runModel <- function(modelInputs, startPars) {
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
  model$logLik <- -1*model$objective # -1 for (+) rather than (-) LL
  return(model)
}

getStartPars <- function(modelInputs, i) {
  startPars <- getRandomStartPars(modelInputs)
  if (i == 1) {
    if (! (is.null(modelInputs$options$startVals))) {
      message("NOTE: Using user-provided starting values for this run")
      userStartPars <- modelInputs$options$startVals
      if (length(userStartPars) != length(startPars)) {
        stop(
          "Number of user-provided starting values do not match number ",
          "of model parameters."
        )
      }
      return(userStartPars)
    } else {
      # For first run only, use all 0s as the starting parameters
      startPars <- 0 * startPars
    }
  }
  startPars <- checkStartPars(startPars, modelInputs)
  return(startPars)
}

# Returns randomly drawn starting parameters from a uniform distribution
# between modelInputs$options$startParBounds
getRandomStartPars <- function(modelInputs) {
  parList <- modelInputs$parList
  bounds <- modelInputs$options$startParBounds
  lower <- bounds[1]
  upper <- bounds[2]
  # For mxl models, need both '_mu' and '_sigma' parameters
  pars_mu <- stats::runif(length(parList$mu), lower, upper)
  pars_sigma <- stats::runif(length(parList$sigma), lower, upper)
  startPars <- c(pars_mu, pars_sigma)
  names(startPars) <- parList$all
  return(startPars)
}

# For lambda and logN parameters must start with positive numbers
checkStartPars <- function(startPars, modelInputs) {
  lambdaParIDs <- NULL
  if (modelInputs$inputs$modelSpace == "wtp") {
    lambdaParIDs <- which(grepl("lambda", modelInputs$parList$all))
  }
  logNPars <- names(getLogNormParIDs(modelInputs$parSetup))
  logNParIDs <- c()
  if (length(logNPars) > 0) {
    for (par in logNPars) {
      logNParIDs <- c(logNParIDs, which(grepl(par, modelInputs$parList$all)))
    }
  }
  positiveParIDs <- unique(c(lambdaParIDs, logNParIDs))
  if (length(positiveParIDs) > 0) {
    startPars[positiveParIDs] <- stats::runif(
      length(positiveParIDs), 0.01, 0.1)
  }
  return(startPars)
}
