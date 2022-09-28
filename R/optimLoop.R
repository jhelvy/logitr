# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart <- function(mi) {
  numMultiStarts <- mi$n$multiStarts
  miList <- makeModelInputsList(mi, numMultiStarts)
  numCores <- mi$n$cores
  # If there's no multi-start, just print that the model is running
  if (numMultiStarts == 1) {
    message("Running model...")
    return(lapply(miList, runModel))
  }
  # If there is a multi-start, print the number of iterations and cores
  printMultistartHeader(mi$inputs$startVals, numMultiStarts, numCores)
  if (Sys.info()[['sysname']] == 'Windows') {
    cl <- parallel::makeCluster(numCores, "PSOCK")
    result <- suppressMessages(suppressWarnings(
      parallel::parLapply(cl = cl, miList, runModel)
    ))
    parallel::stopCluster(cl)
  } else {
    result <- suppressMessages(suppressWarnings(
      parallel::mclapply(miList, runModel, mc.cores = numCores)
    ))
  }
  return(result)
}

printMultistartHeader <- function(startVals, numMultiStarts, numCores) {
  message(
    "Running multistart...\n",
    "  Random starting point iterations: ", numMultiStarts, "\n",
    "  Number of cores: ", numCores
  )
  if (!is.null(startVals)) {
    message("  NOTE: Using user-provided starting values for first iteration")
  }
}

makeModelInputsList <- function(mi, numMultiStarts) {
  # Make repeated list of modelInputs
  mi$model <- makeModelTemplate(mi)
  miList <- rep(list(mi), numMultiStarts)
  # Add starting parameters and multistartNumber for each modelInputs
  for (i in 1:numMultiStarts) {
    miList[[i]]$model$startVals <- getStartVals(mi, i)
    miList[[i]]$model$multistartNumber <- i
  }
  return(miList)
}

getStartVals <- function(mi, i) {
  startVals <- getRandomStartVals(mi)
  if (i == 1) {
    return(firstIterStartVals(startVals, mi))
  }
  startVals <- checkStartVals(startVals, mi)
  return(startVals)
}

# Returns randomly drawn starting parameters from a uniform distribution
# between modelInputs$inputs$startValBounds
getRandomStartVals <- function(mi) {
  parNames <- mi$parNames
  bounds <- mi$inputs$startValBounds
  lower <- bounds[1]
  upper <- bounds[2]
  # For mxl models, need both mean and sd parameters
  vals_mean <- stats::runif(length(parNames$mean), lower, upper)
  vals_sd <- stats::runif(length(parNames$sd), lower, upper)
  startVals <- c(vals_mean, vals_sd)
  names(startVals) <- parNames$all
  return(startVals)
}

firstIterStartVals <- function(startVals, mi) {
  if (!is.null(mi$inputs$startVals)) {
    userStartVals <- mi$inputs$startVals
    if (length(userStartVals) != length(startVals)) {
      stop(
        "Number of user-provided starting values do not match number ",
        "of model parameters."
      )
    } else {
      startVals[1:length(startVals)] <- userStartVals
      return(startVals)
    }
  }
  startVals <- 0 * startVals
  # For correlated parameters in mxl models, set sd vals to 0.1
  if (mi$inputs$correlation) {
    startVals[mi$parNames$sd] <- 0.1
  }
  # For log-normal or censored-normal parameters, set vals
  lnIDs <- mi$parIDs$ln
  if (length(lnIDs) > 0) {
    startVals[lnIDs] <- 0.1
  }
  cnIDs <- mi$parIDs$cn
  if (length(cnIDs) > 0) {
    startVals[cnIDs] <- 0.1
  }
  if (mi$modelSpace == "wtp") {
    # Force starting with scalePar = 1 for WTP space models for stability
    startVals[1] <- 1
  }
  return(startVals)
}

# Sets tighter bounds on starting parameters for stability in these cases:
# - SD parameters for MXL models with correlation
# - Log-normal parameters in MXL models (must be positive)
# - Censored-normal parameters in MXL models (must be positive)
# - scalePar term in WTP space models (always start at 1)
checkStartVals <- function(startVals, mi) {
  # For correlated parameters in mxl models, set sd vals to between [0.1, 0.2]
  if (mi$inputs$correlation) {
    startVals[mi$parNames$sd] <- stats::runif(length(mi$parNames$sd), 0.1, 0.2)
  }
  # For log-normal or censored-normal parameters, force positivity
  lnIDs <- mi$parIDs$ln
  if (length(lnIDs) > 0) {
    startVals[lnIDs] <- stats::runif(length(lnIDs), 0.1, 0.2)
  }
  cnIDs <- mi$parIDs$cn
  if (length(cnIDs) > 0) {
    startVals[cnIDs] <- stats::runif(length(cnIDs), 0.1, 0.2)
  }
  if (mi$modelSpace == "wtp") {
    # Force starting with scalePar = 1 for WTP space models for stability
    startVals[1] <- 1
  }
  return(startVals)
}

makeModelTemplate <- function(mi) {
  # Make default values to return if the model fails
  pars <- mi$parNames$all
  result <- structure(list(
    fail              = TRUE,
    coefficients      = rep(NA, length(pars)),
    logLik            = NA,
    nullLogLik        = NA,
    gradient          = NA,
    hessian           = NA,
    probabilities     = NULL,
    fitted.values     = NULL,
    residuals         = NULL,
    startVals         = NA,
    multistartNumber  = NA,
    multistartSummary = NULL,
    time              = NA,
    iterations        = NA,
    message           = "Generic failure code.",
    status            = -1,
    call              = mi$call,
    formula           = mi$formula,
    date              = mi$date,
    version           = mi$version,
    inputs            = mi$inputs,
    n                 = mi$n,
    freq              = mi$freq,
    modelType         = mi$modelType,
    modelSpace        = mi$modelSpace,
    weightsUsed       = mi$weightsUsed,
    parSetup          = mi$parSetup,
    parIDs            = mi$parIDs,
    scaleFactors      = mi$scaleFactors,
    standardDraws     = mi$standardDraws,
    drawType          = mi$drawType,
    options           = mi$options
  ),
  class = "logitr"
  )
  return(result)
}

runModel <- function(mi) {
  time <- system.time({
    model <- mi$model
    result <- NULL
    tryCatch(
      {
        result <- nloptr::nloptr(
          x0     = mi$model$startVals,
          eval_f = mi$evalFuncs$objective,
          mi     = mi,
          opts   = mi$options
        )
      },
      error = function(e) {}
    )
    if (!is.null(result)) {
      # Didn't fail, so add result values to model
      model$fail         <- FALSE
      model$coefficients <- result$solution
      model$logLik       <- as.numeric(-1*result$objective) # -1 for (+) LL
      model$iterations   <- result$iterations
      model$status       <- result$status
      model$message      <- result$message
    }
  })
  model$time <- time
  return(model)
}
