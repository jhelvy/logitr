# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart <- function(mi) {
  numMultiStarts <- mi$n$multiStarts
  miList <- makeModelInputsList(mi, numMultiStarts)
  numCores <- mi$n$cores
  if ((numMultiStarts == 1) | (numCores == 1)) {
    message("Running model...")
    return(lapply(miList, runModel))
  }
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
    "  Iterations: ", numMultiStarts, "\n",
    "  Cores: ", numCores
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
    miList[[i]]$model$startPars <- getStartPars(mi, i)
    miList[[i]]$model$multistartNumber <- i
  }
  return(miList)
}

getStartPars <- function(mi, i) {
  startPars <- getRandomStartPars(mi)
  if (i == 1) {
    if (!is.null(mi$inputs$startVals)) {
      userStartPars <- mi$inputs$startVals
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
  startPars <- checkStartPars(startPars, mi, i)
  return(startPars)
}

# Returns randomly drawn starting parameters from a uniform distribution
# between modelInputs$inputs$startParBounds
getRandomStartPars <- function(mi) {
  parNames <- mi$parNames
  bounds <- mi$inputs$startParBounds
  lower <- bounds[1]
  upper <- bounds[2]
  # For mxl models, need both mean and sd parameters
  pars_mean <- stats::runif(length(parNames$mean), lower, upper)
  pars_sd <- stats::runif(length(parNames$sd), lower, upper)
  startPars <- c(pars_mean, pars_sd)
  names(startPars) <- parNames$all
  return(startPars)
}

checkStartPars <- function(startPars, mi, i) {
  if (mi$inputs$modelSpace == "wtp") {
    # Force starting with lambda = 1 for WTP space models for stability
    startPars[1] <- 1
  }
  # For correlated parameters in mxl models, set sd pars to 0.1
  if (mi$inputs$correlation) {
    startPars[mi$parNames$sd] <- 0.1
  }
  # For log-normal parameters, force positivity
  parIDs <- mi$parIDs
  lnIDs <- parIDs$ln
  if (length(lnIDs) > 0) {
    startPars[lnIDs] <- 0.1
  }
  return(startPars)
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
    startPars         = NA,
    multistartNumber  = NA,
    multistartSummary = NULL,
    time              = NA,
    iterations        = NA,
    message           = "Generic failure code.",
    status            = -1,
    call              = mi$call,
    date              = mi$date,
    version           = mi$version,
    inputs            = mi$inputs,
    n                 = mi$n,
    freq              = mi$freq,
    modelType         = mi$modelType,
    weightsUsed       = mi$weightsUsed,
    parSetup          = mi$parSetup,
    parIDs            = mi$parIDs,
    scaleFactors      = mi$scaleFactors,
    standardDraws     = mi$standardDraws,
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
          x0     = mi$model$startPars,
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
