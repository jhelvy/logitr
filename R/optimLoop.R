# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart <- function(modelInputs, numCores) {
  numMultiStarts <- modelInputs$inputs$numMultiStarts
  if (numMultiStarts == 1) {
    message("Running model...")
  } else {
    message(
      "Running multistart...\n",
      "  Iterations: ", numMultiStarts, "\n",
      "  Cores: ", numCores
    )
  }
  if (!is.null(modelInputs$inputs$startVals)) {
    message("  NOTE: Using user-provided starting values for first iteration")
  }
  modelInputsList <- makeModelInputsList(numMultiStarts, modelInputs)
  if (Sys.info()[['sysname']] == 'Windows') {
    cl <- parallel::makeCluster(numCores, "PSOCK")
    result <- suppressMessages(suppressWarnings(
      parallel::parLapply(cl = cl, modelInputsList, runModel)
    ))
    parallel::stopCluster(cl)
  } else {
    result <- suppressMessages(suppressWarnings(
      parallel::mclapply(modelInputsList, runModel, mc.cores = numCores)
    ))
  }
  return(result)
}

makeModelInputsList <- function(numMultiStarts, modelInputs) {
  # Make repeated list of modelInputs
  modelInputs$model <- makeModelTemplate(modelInputs)
  modelInputsList <- rep(list(modelInputs), numMultiStarts)
  # Add starting parameters and multistartNumber for each modelInputs
  for (i in 1:numMultiStarts) {
    modelInputsList[[i]]$model$startPars <- getStartPars(modelInputs, i)
    modelInputsList[[i]]$model$multistartNumber <- i
  }
  return(modelInputsList)
}

getStartPars <- function(modelInputs, i) {
  startPars <- getRandomStartPars(modelInputs)
  if (i == 1) {
    if (!is.null(modelInputs$inputs$startVals)) {
      userStartPars <- modelInputs$inputs$startVals
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
  startPars <- checkStartPars(startPars, modelInputs, i)
  return(startPars)
}

# Returns randomly drawn starting parameters from a uniform distribution
# between modelInputs$inputs$startParBounds
getRandomStartPars <- function(modelInputs) {
  parList <- modelInputs$parList
  bounds <- modelInputs$inputs$startParBounds
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
checkStartPars <- function(startPars, modelInputs, i) {
  if (modelInputs$inputs$modelSpace == "wtp") {
    # Force starting with lambda = 1 for WTP space models for stability
    startPars[1] <- 1
  }
  # For log-normal parameters, force positivity
  parIDs <- modelInputs$parIDs
  lnIDs <- parIDs$logNormal
  if (length(lnIDs) > 0) {
    if (i == 1) {
      startPars[lnIDs] <- 1
    } else {
      startPars[lnIDs] <- stats::runif(length(lnIDs), 0.1, 1)
    }
  }
  return(startPars)
}

makeModelTemplate <- function(modelInputs) {
  # Make default values to return if the model fails
  pars <- modelInputs$parList$all
  result <- structure(list(
    fail              = TRUE,
    coefficients      = rep(NA, length(pars)),
    logLik            = NA,
    nullLogLik        = NA,
    gradient          = NA,
    hessian           = NA,
    probabilities     = NA,
    fitted.values     = NA,
    residuals         = NA,
    startPars         = NA,
    multistartNumber  = NA,
    multistartSummary = NULL,
    time              = NA,
    iterations        = NA,
    message           = "Generic failure code.",
    status            = -1,
    call              = modelInputs$call,
    inputs            = modelInputs$inputs,
    data              = modelInputs$data,
    numObs            = sum(modelInputs$data$outcome),
    numParams         = length(pars),
    freq              = modelInputs$freq,
    modelType         = modelInputs$modelType,
    weightsUsed       = modelInputs$weightsUsed,
    numClusters       = modelInputs$numClusters,
    parSetup          = modelInputs$parSetup,
    parIDs            = modelInputs$parIDs,
    scaleFactors      = modelInputs$scaleFactors,
    standardDraws     = modelInputs$standardDraws,
    options           = modelInputs$options
  ),
  class = "logitr"
  )
  return(result)
}

runModel <- function(modelInputs) {
  time <- system.time({
    model <- modelInputs$model
    result <- NULL
    tryCatch(
      {
        result <- nloptr::nloptr(
          x0     = modelInputs$model$startPars,
          eval_f = modelInputs$evalFuncs$objective,
          mi     = modelInputs,
          opts   = modelInputs$options
        )
      },
      error = function(e) {}
    )
    if (!is.null(result)) {
      # Didn't fail, so add result values to model
      model$fail <- FALSE
      model$coefficients <- result$solution
      model$logLik <- as.numeric(-1*result$objective) # -1 for (+) LL
      model$iterations <- result$iterations
      model$status <- result$status
      model$message <- result$message
    }
  })
  model$time <- time
  return(model)
}
