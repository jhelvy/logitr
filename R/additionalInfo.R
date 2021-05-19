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
  covariance <- getModelCovariance(model, modelInputs, hessian)
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
    modelType        = modelInputs$modelType,
    modelSpace       = modelInputs$modelSpace,
    priceName        = modelInputs$priceName,
    parNames         = modelInputs$parNames,
    randPars         = modelInputs$randPars,
    parSetup         = modelInputs$parSetup,
    weightsUsed      = modelInputs$weightsUsed,
    clusterName      = modelInputs$clusterName,
    numClusters      = modelInputs$numClusters,
    robust           = modelInputs$robust,
    standardDraws    = NA,
    randParSummary   = NA,
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
    gradient <- gradient * scaleFactors
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

getModelCovariance <- function(model, modelInputs, hessian) {
  clusterID <- modelInputs$clusterIDs
  if (is.null(clusterID) | modelInputs$robust == FALSE) {
    return(getModelCovarianceNonRobust(hessian))
  }
  return(getModelCovarianceRobust(model, modelInputs, hessian))

}

getModelCovarianceRobust <- function(model, modelInputs, hessian) {
  i <- 0
  gradientList <- c()
  clusterID <- modelInputs$clusterIDs
  for (tempID in sort(unique(clusterID))) {
    indices <- which(clusterID == tempID)
    tempModelInputs <- getClusterModelInputs(modelInputs, indices)
    tempGradient <- getModelGradient(model, tempModelInputs)
    gradientList <- c(gradientList, tempGradient)
    i <- i + 1
  }
  gradMat <- matrix(gradientList, nrow = i, length(tempGradient), byrow = TRUE)
  gradMean <- colMeans(gradMat)
  gradMeans <- c()
  for (tempID in sort(unique(clusterID))) {
    gradMeans <- c(gradMeans, gradMean)
  }
  gradMeanMat <- matrix(
    gradMeans, nrow = i, length(tempGradient), byrow = TRUE)

  diffMat <- gradMat - gradMeanMat

  M <- t(diffMat) %*% diffMat
  smallSampleCorrection <- (i / (i - 1))
  M <- smallSampleCorrection * M
  D <- getModelCovarianceNonRobust(hessian)
  if (any(is.na(D))) {
    return(D) # If there are NAs the next line will error
  }
  covar <- D %*% M %*% D
  return(covar)
}

getModelCovarianceNonRobust <- function(hessian) {
  covariance <- hessian*NA
  tryCatch(
    {
      covariance <- solve(-1*hessian)
    },
    error = function(e) {}
  )
  return(covariance)
}

getClusterModelInputs <- function (modelInputs, indices) {
  modelInputs$X <- modelInputs$X[indices, ]
  #Cast to matrix in cases where there is 1 independent variable
  if(!is.matrix(modelInputs$X)){
    modelInputs$X <- as.matrix(modelInputs$X)
  }
  modelInputs$choice <- modelInputs$choice[indices]
  modelInputs$price <- modelInputs$price[indices]
  modelInputs$weights <- modelInputs$weights[indices]
  modelInputs$obsID <- modelInputs$obsID[indices]
  modelInputs$clusterIDs <- modelInputs$clusterIDs[indices]
  return(modelInputs)
}

getModelStandErrs <- function(covariance) {
  se <- covariance[1,]*NA
  tryCatch(
    {
      se <- diag(sqrt(abs(covariance)))
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
