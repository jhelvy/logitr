# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart <- function(modelInputs, numCores) {
  numMultiStarts <- modelInputs$inputs$numMultiStarts
  if (numMultiStarts == 1) {
    message("Running model...")
  } else {
      ending <- ifelse(numCores == 1, " core...", " cores...")
    message(
      "Running a ", numMultiStarts, "-iteration multistart using ", numCores,
      ending)
  }
  model <- makeModelTemplate(modelInputs)
  if (Sys.info()[['sysname']] == 'Windows') {
      result <- multistartPsock(numMultiStarts, modelInputs, model, numCores)
  } else {
      result <- suppressMessages(suppressWarnings(
        parallel::mclapply(
          seq(numMultiStarts),
          runSingleModel,
          modelInputs = modelInputs,
          model = model,
          mc.cores = numCores)
      ))
  }
  # Replace iterations that had an error with template model
  errors <- sapply(result, inherits, what = "try-error")
  if (any(errors)) {
    badIDs <- which(errors)
    result[badIDs] <- lapply(result[badIDs], function(x) x <- model)
  }
  return(result)
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

multistartPsock <- function(numMultiStarts, modelInputs, model, numCores) {
    cl <- parallel::makeCluster(numCores, "PSOCK")
    parallel::clusterExport(
        cl = cl,
        varlist = c("numMultiStarts", "modelInputs", "model", "numCores"))
    result <- suppressMessages(suppressWarnings(
      parallel::parLapply(
        cl = cl,
        fun = runSingleModel,
        seq = seq(numMultiStarts),
        modelInputs = modelInputs,
        model = model)
    ))
    parallel::stopCluster(cl)
    return(result)
}

runSingleModel <- function(i, modelInputs, model) {
  time <- system.time({
    startPars <- getStartPars(modelInputs, i)
    result <- NULL
    result <- runModel(modelInputs, startPars)
    if (!is.null(result)) {
      # Didn't fail, so add result values to model
      model$fail <- FALSE
      model$coefficients <- result$solution
      model$logLik <- as.numeric(-1*result$objective) # -1 for (+) LL
      model$iterations <- result$iterations
      model$status <- result$status
      model$message <- result$message
    }
    model$startPars <- startPars
    model$multistartNumber <- i
  })
  model$time <- time
  return(model)
}

getStartPars <- function(modelInputs, i) {
  startPars <- getRandomStartPars(modelInputs)
  if (i == 1) {
    if (! (is.null(modelInputs$inputs$startVals))) {
      message("NOTE: Using user-provided starting values for this run")
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

runModel <- function(modelInputs, startPars) {
  model <- nloptr::nloptr(
    x0     = startPars,
    eval_f = modelInputs$evalFuncs$objective,
    mi     = modelInputs,
    opts   = modelInputs$options
  )
  return(model)
}
