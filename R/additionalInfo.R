# ============================================================================
# Functions for computing other information about model after estimation
# ============================================================================

appendModelInfo <- function(model, modelInputs) {
  parsUnscaled <- model$coef
  parNames <- c(modelInputs$parList$mu, modelInputs$parList$sigma)
  names(parsUnscaled) <- parNames
  scaleFactors <- model$data$scaleFactors
  if (model$fail) {
    coef <- parsUnscaled*NA
    gradient <- matrix(coef, ncol = 1)
    row.names(gradient) <- parNames
    hessian <- matrix(NA, nrow = length(parNames), ncol = length(parNames))
    row.names(hessian) <- parNames
    colnames(hessian) <- parNames
    nullLogLik <- NA
  } else {
    coef       <- getCoefs(parsUnscaled, scaleFactors, modelInputs)
    gradient   <- getGradient(parsUnscaled, scaleFactors, modelInputs)
    hessian    <- getHessian(parsUnscaled, scaleFactors, modelInputs)
    nullLogLik <- -1 * modelInputs$evalFuncs$negLL(coef * 0, modelInputs)
  }
  model$coef <- coef
  model$gradient <- gradient
  model$hessian <- hessian
  model$nullLogLik <- nullLogLik
  model$scaleFactors <- scaleFactors
  model$result <- NULL
  model$fail <- NULL
  return(model)
}

getCoefs <- function(parsUnscaled, scaleFactors, modelInputs) {
  pars <- parsUnscaled / scaleFactors
  sigmaNames <- modelInputs$parList$sigma
  pars[sigmaNames] <- abs(pars[sigmaNames]) # Make sigmas positive
  return(pars)
}

getGradient <- function(parsUnscaled, scaleFactors, modelInputs) {
  gradient <- -1 * modelInputs$evalFuncs$negGradLL(parsUnscaled, modelInputs)
  return(gradient * scaleFactors)
}

getHessian <- function(parsUnscaled, scaleFactors, modelInputs) {
  parNames <- c(modelInputs$parList$mu, modelInputs$parList$sigma)
  if (any(is.na(parsUnscaled))) {
    # Model failed - return a matrix of NA values
    hessian <- matrix(NA, nrow = length(parNames), ncol = length(parNames))
  } else {
    hessian <- modelInputs$evalFuncs$hessLL(parsUnscaled, modelInputs)
    if (modelInputs$inputs$scaleInputs) {
      sf <- matrix(scaleFactors, ncol = 1)
      sfMat <- sf %*% t(sf)
      hessian <- hessian * sfMat
    }
  }
  colnames(hessian) <- parNames
  row.names(hessian) <- parNames
  return(hessian)
}
