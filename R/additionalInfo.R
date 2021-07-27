# ============================================================================
# Functions for computing other information about model after estimation
# ============================================================================

appendModelInfo <- function(model, modelInputs, multistartSummary) {
  parsUnscaled <- model$coef
  names(parsUnscaled) <- c(modelInputs$parList$mu, modelInputs$parList$sigma)
  scaleFactors <- NA
  if (modelInputs$inputs$scaleInputs) {
    scaleFactors <- updateScaleFactors(modelInputs)
  }
  coef       <- getCoefs(parsUnscaled, scaleFactors, modelInputs)
  gradient   <- getGradient(parsUnscaled, scaleFactors, modelInputs)
  hessian    <- getHessian(parsUnscaled, scaleFactors, modelInputs)
  nullLogLik <- -1 * modelInputs$evalFuncs$negLL(coef * 0, modelInputs)
  model$coef <- coef
  model$gradient <- gradient
  model$hessian <- hessian
  model$nullLogLik <- nullLogLik
  model$scaleFactors <- scaleFactors
  model$result <- NULL
  return(model)
}

updateScaleFactors <- function(modelInputs) {
  scaleFactors <- modelInputs$scaleFactors
  if (modelInputs$inputs$modelSpace == "wtp") {
    lambdaID <- which(grepl("lambda", names(scaleFactors)) == T)
    nonLambdaID <- which(grepl("lambda", names(scaleFactors)) == F)
    lambdaSF <- scaleFactors[lambdaID]
    scaleFactors[nonLambdaID] <- scaleFactors[nonLambdaID] / lambdaSF
  }
  if (modelInputs$modelType == "mnl") {
    return(scaleFactors)
  } else {
    parNames <- c(modelInputs$parList$mu, modelInputs$parList$sigma)
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

getCoefs <- function(parsUnscaled, scaleFactors, modelInputs) {
  if (modelInputs$inputs$scaleInputs) {
    pars <- parsUnscaled / scaleFactors
  }
  # Make sigmas positive
  sigmaNames <- modelInputs$parList$sigma
  pars[sigmaNames] <- abs(pars[sigmaNames])
  return(pars)
}

getGradient <- function(parsUnscaled, scaleFactors, modelInputs) {
  gradient <- -1 * modelInputs$evalFuncs$negGradLL(parsUnscaled, modelInputs)
  if (modelInputs$inputs$scaleInputs) {
    gradient <- gradient * scaleFactors
  }
  return(gradient)
}

getHessian <- function(parsUnscaled, scaleFactors, modelInputs) {
  hessian <- modelInputs$evalFuncs$hessLL(parsUnscaled, modelInputs)
  if (modelInputs$inputs$scaleInputs) {
    sf <- matrix(scaleFactors, ncol = 1)
    sfMat <- sf %*% t(sf)
    hessian <- hessian * sfMat
  }
  parNames <- c(modelInputs$parList$mu, modelInputs$parList$sigma)
  colnames(hessian) <- parNames
  row.names(hessian) <- parNames
  return(hessian)
}
