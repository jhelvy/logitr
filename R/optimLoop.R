# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart <- function(modelInputs) {

  numMultiStarts <- modelInputs$inputs$numMultiStarts
  modelTemplate <- makeModelTemplate(modelInputs)
  models <- list()

  # Run multistart loop
  for (i in 1:numMultiStarts) {

    # Print run number
    if (numMultiStarts == 1) {
      message("Running Model...")
    } else {
      message("Running Multistart ", i, " of ", numMultiStarts, "...")
    }

    # Attempt to estimate model
    startTime <- proc.time()
    startPars <- getStartPars(modelInputs, i)
    model <- modelTemplate
    result <- NULL
    tryCatch(
      {
        result <- runModel(modelInputs, startPars)
      },
      error = function(e) {}
    )

    # Add result values to model
    if (!is.null(result)) {
      model$coef <- result$solution
      # -1 for (+) rather than (-) LL
      model$logLik     <- as.numeric(-1*result$objective)
      model$iterations <- result$iterations
      model$status     <- result$status
      model$message    <- result$message
    }

    model$startPars <- startPars
    model$multistartNumber <- i
    model$time <- proc.time() - startTime
    models[[i]] <- model
  }
  return(models)
}

makeModelTemplate <- function(modelInputs) {
  # Make default values to return if the model fails
  pars <- modelInputs$parList$all
  coefNA <- rep(NA, length(pars))
  result <- structure(list(
    coef              = coefNA,
    logLik            = NA,
    nullLogLik        = NA,
    gradient          = NA,
    hessian           = NA,
    startPars         = NA,
    multistartNumber  = NA,
    multistartSummary = NULL,
    time              = NA,
    iterations        = NA,
    message           = "Generic failure code.",
    status            = -1,
    call              = modelInputs$call,
    inputs            = modelInputs$inputs,
    X                 = modelInputs$X,
    price             = modelInputs$price,
    choice            = modelInputs$choice,
    obsID             = modelInputs$obsID,
    weights           = modelInputs$weights,
    clusterIDs        = modelInputs$clusterIDs,
    numObs            = sum(modelInputs$choice),
    numParams         = length(pars),
    freq              = modelInputs$freq,
    modelType         = modelInputs$modelType,
    weightsUsed       = modelInputs$weightsUsed,
    numClusters       = modelInputs$numClusters,
    parSetup          = modelInputs$parSetup,
    scaleFactors      = NA,
    standardDraws     = modelInputs$standardDraws,
    options           = modelInputs$options
  ),
  class = "logitr"
  )
  return(result)
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
  startPars <- checkStartPars(startPars, modelInputs)
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

runModel <- function(modelInputs, startPars) {
  model <- nloptr::nloptr(
    x0 = startPars,
    eval_f = modelInputs$evalFuncs$objective,
    modelInputs = modelInputs,
    opts = modelInputs$options
  )
  return(model)
}
