# ============================================================================
# Functions for computing other information about model after estimation
# ============================================================================

appendModelInfo <- function(model, modelInputs) {
  if (is_logitr_fail(model)) {
    # This run failed to converge - return the "blank" model summary
    return(model)
  }
  # Compute variables
  coef <- getModelCoefs(model, modelInputs)
  gradient <- getModelGradient(model, modelInputs)
  hessian <- getModelHessian(model, modelInputs)
  covariance <- getModelCovariance(hessian)
  se <- getModelStandErrs(covariance)
  logLik <- as.numeric(model$logLik)
  nullLogLik <- -1 * modelInputs$evalFuncs$negLL(coef * 0, modelInputs)
  numObs <- sum(modelInputs$choice)
  options <- append(modelInputs$options, model$options)
  result <- structure(list(
    coef             = coef,
    standErrs        = se,
    logLik           = logLik,
    nullLogLik       = nullLogLik,
    gradient         = gradient,
    hessian          = hessian,
    covariance       = covariance,
    numObs           = numObs,
    numParams        = length(coef),
    startPars        = model$startPars,
    multistartNumber = model$multistartNumber,
    time             = model$time,
    iterations       = model$iterations,
    message          = model$message,
    status           = model$status,
    modelSpace       = modelInputs$modelSpace,
    standardDraws    = NA,
    randParSummary   = NA,
    parSetup         = modelInputs$parSetup,
    weightsUsed      = modelInputs$weightsUsed,
    options          = options
  ),
  class = "logitr"
  )
  # If MXL model, attached draws and summary of parameter distributions
  if (modelInputs$modelType == "mxl") {
    result$standardDraws <- modelInputs$standardDraws
    result$randParSummary <- getRandParSummary(coef, modelInputs)
  }
  return(result)
}

getUnscaledPars <- function(model, modelInputs) {
  pars <- model$solution
  muNames <- modelInputs$parNameList$mu
  sigmaNames <- modelInputs$parNameList$sigma
  names(pars) <- c(muNames, sigmaNames)
  return(pars)
}

getModelCoefs <- function(model, modelInputs) {
  pars <- getUnscaledPars(model, modelInputs)
  if (modelInputs$options$scaleInputs) {
    scaleFactors <- getModelScaleFactors(model, modelInputs)
    pars <- pars / scaleFactors
  }
  # Make sigmas positive
  sigmaNames <- modelInputs$parNameList$sigma
  pars[sigmaNames] <- abs(pars[sigmaNames])
  return(pars)
}

getModelGradient <- function(model, modelInputs) {
  pars <- getUnscaledPars(model, modelInputs)
  gradient <- -1 * modelInputs$evalFuncs$negGradLL(pars, modelInputs)
  if (modelInputs$options$scaleInputs) {
    scaleFactors <- getModelScaleFactors(model, modelInputs)
    gradient <- gradient / scaleFactors
  }
  names(gradient) <- names(pars)
  return(gradient)
}

getModelHessian <- function(model, modelInputs) {
  pars <- getUnscaledPars(model, modelInputs)
  hessian <- modelInputs$evalFuncs$hessLL(pars, modelInputs)
  if (modelInputs$options$scaleInputs) {
    scaleFactors <- getModelScaleFactors(model, modelInputs)
    sf <- matrix(scaleFactors, ncol = 1)
    sfMat <- sf %*% t(sf)
    hessian <- hessian * sfMat
  }
  parNames <- c(modelInputs$parNameList$mu, modelInputs$parNameList$sigma)
  colnames(hessian) <- parNames
  row.names(hessian) <- parNames
  return(hessian)
}

getModelScaleFactors <- function(model, modelInputs) {
  scaleFactors <- modelInputs$scaleFactors
  if (modelInputs$modelSpace == "wtp") {
    lambdaID <- which(grepl("lambda", names(scaleFactors)) == T)
    nonLambdaID <- which(grepl("lambda", names(scaleFactors)) == F)
    lambdaSF <- scaleFactors[lambdaID]
    scaleFactors[nonLambdaID] <- scaleFactors[nonLambdaID] / lambdaSF
  }
  if (modelInputs$modelType == "mnl") {
    return(scaleFactors)
  } else {
    parNames <- c(modelInputs$parNameList$mu, modelInputs$parNameList$sigma)
    mxlScaleFactors <- rep(0, length(parNames))
    for (i in seq_len(length(scaleFactors))) {
      scaleFactor <- scaleFactors[i]
      factorIDs <- which(grepl(names(scaleFactor), parNames))
      mxlScaleFactors[factorIDs] <- scaleFactor
      names(mxlScaleFactors)[factorIDs] <- parNames[factorIDs]
    }
    return(mxlScaleFactors)
  }
}

getModelCovariance <- function(hessian) {
  covariance <- hessian*NA
  tryCatch(
    {
      covariance <- abs(solve(hessian))
    },
    error = function(e) {}
  )
  return(covariance)
}

getModelStandErrs <- function(covariance) {
  se <- covariance[1,]*NA
  tryCatch(
    {
      se <- diag(sqrt(covariance))
    },
    error = function(e) {}
  )
  return(se)
}

getRandParSummary <- function(coef, modelInputs) {
  parSetup <- modelInputs$parSetup
  numDraws <- 10^4
  randParIDs <- getRandParIDs(parSetup)
  standardDraws <- getStandardDraws(parSetup, numDraws)
  betaDraws <- makeBetaDraws(coef, parSetup, 10^4, standardDraws)
  randParSummary <- apply(betaDraws, 2, summary)
  # Add names to summary
  distName <- rep("", length(parSetup))
  distName[getNormParIDs(parSetup)] <- "normal"
  distName[getLogNormParIDs(parSetup)] <- "log-normal"
  summaryNames <- paste(names(parSetup), " (", distName, ")", sep = "")
  colnames(randParSummary) <- summaryNames
  randParSummary <- t(randParSummary[, randParIDs])
  return(as.data.frame(randParSummary))
}
