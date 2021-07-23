# ============================================================================
# Logit and log-likelihood functions
# The log-likelihood function is given as the negative log-likelihood
# because the optimization performs a minimization
# ============================================================================

# ============================================================================
# MNL logit and log-likelihood functions for both Preference and WTP Spaces
# ============================================================================

# Logit fraction using data.table package (faster, but requires data.table)
# Returns the logit fraction for mnl (homogeneous) models
getMnlLogit <- function(V, obsID) {
    data = data.table::data.table(V = V, obsID = obsID)
    data[, expV:=exp(V.V1)]
    data[, sumExpV:=sum(expV), by=obsID]
    data[, logit:=expV/sumExpV]
    return(data$logit)
}

mnlNegLL <- function(choice, logit, weights) {
  negLL <- -1 * sum(weights * choice * log(logit))
  return(negLL)
}

# Returns a list containing the negative log-likelihood and it's gradient
# Primary objective function for the nloptr optimizer.
mnlNegLLAndGradLL <- function(pars, modelInputs) {
  logitFuncs <- modelInputs$logitFuncs
  X <- modelInputs$X
  p <- modelInputs$price
  obsID <- modelInputs$obsID
  choice <- modelInputs$choice
  weights <- modelInputs$weights
  V <- logitFuncs$getMnlV(pars, X, p)
  logit <- logitFuncs$getMnlLogit(V, obsID)
  negLL <- logitFuncs$mnlNegLL(choice, logit, weights)
  negGradLL <- logitFuncs$mnlNegGradLL(p, X, pars, choice, logit, weights)
  return(list("objective" = negLL, "gradient" = negGradLL))
}

getMnlNegLL <- function(pars, modelInputs) {
  X <- modelInputs$X
  p <- modelInputs$price
  logitFuncs <- modelInputs$logitFuncs
  obsID <- modelInputs$obsID
  choice <- modelInputs$choice
  weights <- modelInputs$weights
  V <- logitFuncs$getMnlV(pars, X, p)
  logit <- logitFuncs$getMnlLogit(V, obsID)
  negLL <- logitFuncs$mnlNegLL(choice, logit, weights)
  return(negLL)
}

getMnlNegGradLL <- function(pars, modelInputs) {
  logitFuncs <- modelInputs$logitFuncs
  X <- modelInputs$X
  p <- modelInputs$price
  obsID <- modelInputs$obsID
  choice <- modelInputs$choice
  weights <- modelInputs$weights
  V <- logitFuncs$getMnlV(pars, X, p)
  logit <- logitFuncs$getMnlLogit(V, obsID)
  negGradLL <- logitFuncs$mnlNegGradLL(p, X, pars, choice, logit, weights)
  return(negGradLL)
}

getMnlHessLL <- function(pars, modelInputs) {
  hessLL <- modelInputs$logitFuncs$mnlHessLL(pars, modelInputs)
  return(hessLL)
}

# ============================================================================
# MXL logit and log-likelihood functions for both Preference and WTP Spaces
# ============================================================================
# The log-likelihood function is given here as the negative log-likelihood
# because the optim function performs a minimization

# Returns the logit fraction for all the draws in a mxl (heterogeneous) models
getMxlLogit <- function(VDraws, obsID) {
  numDraws <- ncol(VDraws)
  expVDraws <- exp(VDraws)
  sumExpVDraws <- rowsum(expVDraws, group = obsID, reorder = FALSE)
  repTimes <- rep(as.numeric(table(obsID)), each = numDraws)
  sumExpVDrawsMat <- matrix(rep(sumExpVDraws, times = repTimes),
    ncol = numDraws, byrow = FALSE
  )
  logitDraws <- expVDraws / sumExpVDrawsMat
  return(logitDraws)
}

mxlNegLL <- function(choice, pHat, weights) {
  negLL <- -1 * sum(weights * choice * log(pHat))
  return(negLL)
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
mxlNegLLAndGradLL <- function(pars, modelInputs) {
  logitFuncs <- modelInputs$logitFuncs
  X <- modelInputs$X
  p <- modelInputs$price
  obsID <- modelInputs$obsID
  choice <- modelInputs$choice
  weights <- modelInputs$weights
  parSetup <- modelInputs$parSetup
  numDraws <- modelInputs$options$numDraws
  standardDraws <- modelInputs$standardDraws
  betaDraws <- makeBetaDraws(pars, parSetup, numDraws, standardDraws)
  VDraws <- logitFuncs$getMxlV(betaDraws, X, p)
  logitDraws <- logitFuncs$getMxlLogit(VDraws, obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  negLL <- logitFuncs$mxlNegLL(choice, pHat, weights)
  negGradLL <- logitFuncs$mxlNegGradLL(
    X, parSetup, obsID, choice,
    standardDraws, betaDraws, VDraws, logitDraws, pHat,
    weights
  )
  return(list("objective" = negLL, "gradient" = negGradLL))
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
getMxlNegLL <- function(pars, modelInputs) {
  X <- modelInputs$X
  p <- modelInputs$price
  logitFuncs <- modelInputs$logitFuncs
  obsID <- modelInputs$obsID
  choice <- modelInputs$choice
  weights <- modelInputs$weights
  parSetup <- modelInputs$parSetup
  numDraws <- modelInputs$options$numDraws
  standardDraws <- modelInputs$standardDraws
  betaDraws <- makeBetaDraws(pars, parSetup, numDraws, standardDraws)
  VDraws <- logitFuncs$getMxlV(betaDraws, X, p)
  logitDraws <- logitFuncs$getMxlLogit(VDraws, obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  negLL <- logitFuncs$mxlNegLL(choice, pHat, weights)
  return(negLL)
}

getMxlNegGradLL <- function(pars, modelInputs) {
  logitFuncs <- modelInputs$logitFuncs
  X <- modelInputs$X
  p <- modelInputs$price
  obsID <- modelInputs$obsID
  choice <- modelInputs$choice
  weights <- modelInputs$weights
  parSetup <- modelInputs$parSetup
  numDraws <- modelInputs$options$numDraws
  standardDraws <- modelInputs$standardDraws
  betaDraws <- makeBetaDraws(pars, parSetup, numDraws, standardDraws)
  VDraws <- logitFuncs$getMxlV(betaDraws, X, p)
  logitDraws <- logitFuncs$getMxlLogit(VDraws, obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  negGradLL <- logitFuncs$mxlNegGradLL(
    X, parSetup, obsID, choice,
    standardDraws, betaDraws, VDraws, logitDraws, pHat,
    weights
  )
  return(negGradLL)
}

# ============================================================================
# Numerical log-likelihood functions for both Preference and WTP Spaces
# ============================================================================

mnlNegLLAndNumericGradLL <- function(pars, modelInputs) {
  negLL <- getMnlNegLL(pars, modelInputs)
  negGradLL <- getNumericNegGradLL(pars, modelInputs)
  return(list("objective" = negLL, "gradient" = negGradLL))
}

mxlNegLLAndNumericGradLL <- function(pars, modelInputs) {
  negLL <- getMxlNegLL(pars, modelInputs)
  negGradLL <- getNumericNegGradLL(pars, modelInputs)
  return(list("objective" = negLL, "gradient" = negGradLL))
}

getNumericNegGradLL <- function(pars, modelInputs) {
  return(nloptr::nl.jacobian(
    x0 = pars,
    fn = modelInputs$evalFuncs$negLL,
    modelInputs = modelInputs
  ))
}

getNumericHessLL <- function(pars, modelInputs) {
  return(-1 * nloptr::nl.jacobian(
    x0 = pars,
    fn = modelInputs$evalFuncs$negGradLL,
    modelInputs = modelInputs
  ))
}

# ============================================================================
# Preference Space Logit Functions - MNL models
# ============================================================================

getMnlV_pref <- function(pars, X, p) {
  V <- X %*% as.numeric(pars)
  return(V)
}

mnlNegGradLL_pref <- function(p, X, pars, choice, logit, weights) {
  weightedLogit <- weights * (choice - logit)
  negGradLL <- -1 * (t(X) %*% weightedLogit)
  return(negGradLL)
}

# Returns the hessian of the log-likelihood at the given pars
mnlHessLL_pref <- function(pars, modelInputs) {
  X <- modelInputs$X
  p <- modelInputs$p
  obsID <- modelInputs$obsID
  choice <- modelInputs$choice
  weights <- modelInputs$weights
  parSetup <- modelInputs$parSetup
  V <- getMnlV_pref(pars, X, p)
  logit <- getMnlLogit(V, obsID)
  diffMat <- getDiffMatByObsID_pref(logit, X, obsID)
  logitMat <- repmat(matrix(logit), 1, ncol(X))
  weightsMat <- matrix(rep(weights, length(parSetup)),
    ncol = ncol(X), byrow = F
  )
  hessLL <- -1 * (t(diffMat) %*% (weightsMat * logitMat * diffMat))
  return(hessLL)
}

# Used in computing the hessian
getDiffMatByObsID_pref <- function(logit, X, obsID) {
  diffMat <- matrix(0, nrow = length(obsID), ncol = ncol(X))
  for (id in sort(unique(obsID))) {
    indices <- which(obsID == id)
    tempX <- X[indices, ]
    tempLogit <- logit[indices]
    diffMat[indices, ] <- tempX - repmat(t(tempLogit) %*% tempX, nrow(tempX), 1)
  }
  return(diffMat)
}

# ============================================================================
# Preference Space Logit Functions - MXL models
# ============================================================================

# Returns the observed utility
getMxlV_pref <- function(betaDraws, X, p) {
  VDraws <- X %*% t(betaDraws)
  return(VDraws)
}

# Computes the gradient of the negative likelihood for a mixed logit model
mxlNegGradLL_pref <- function(X, parSetup, obsID, choice, standardDraws,
                              betaDraws, VDraws, logitDraws, pHat, weights) {
  randParIDs <- getRandParIDs(parSetup)
  numDraws <- nrow(standardDraws)
  numBetas <- length(parSetup)
  logNormParIDs <- getLogNormParIDs(parSetup)
  repTimes <- rep(as.numeric(table(obsID)), each = 2 * numBetas)
  # Compute the gradient of V for all parameters
  grad <- matrix(0, nrow = nrow(X), ncol = 2 * numBetas)
  for (i in 1:numDraws) {
    Xtemp <- X
    draws <- standardDraws[i, ]
    logit <- logitDraws[, i]
    drawsMat <- matrix(rep(draws, nrow(X)), ncol = numBetas, byrow = TRUE)
    logitMat <- matrix(rep(logit, numBetas), ncol = numBetas, byrow = FALSE)
    logitMat <- cbind(logitMat, logitMat)
    if (length(logNormParIDs) > 0) {
      beta <- betaDraws[i, ]
      betaMat <- matrix(rep(beta, nrow(X)), ncol = numBetas, byrow = TRUE)
      Xtemp[, logNormParIDs] <- Xtemp[, logNormParIDs] *
        betaMat[, logNormParIDs]
    }
    partial_mu <- Xtemp
    partial_sigma <- Xtemp * drawsMat
    partial <- cbind(partial_mu, partial_sigma)
    temp <- rowsum(logitMat * partial, group = obsID, reorder = FALSE)
    tempMat <- matrix(rep(temp, times = repTimes),
      ncol = ncol(partial),
      byrow = F
    )
    grad <- grad + logitMat * (partial - tempMat)
  }
  weightsMat <- matrix(rep(weights, numBetas), ncol = ncol(X), byrow = F)
  weightsMat <- cbind(weightsMat, weightsMat)
  grad <- weightsMat * (grad / numDraws)
  pHatInvChosen <- matrix(rep(choice * (1 / pHat), 2 * numBetas),
    ncol = 2 * numBetas,
    byrow = F
  )
  grad <- colSums(pHatInvChosen * grad, na.rm = TRUE)
  negGradLL <- -1 * grad[c(1:numBetas, numBetas + randParIDs)]
  return(negGradLL)
}

# ============================================================================
# WTP Space Logit Functions - MNL models
# ============================================================================

# Returns the observed utility
getMnlV_wtp <- function(pars, X, p) {
  lambda <- as.numeric(pars[1])
  beta <- as.numeric(pars[2:length(pars)])
  V <- lambda * ((X %*% beta) - p)
  return(V)
}

# Returns the negative gradient of the log-likelihood
mnlNegGradLL_wtp <- function(p, X, pars, choice, logit, weights) {
  lambda <- as.numeric(pars[1])
  beta <- as.numeric(pars[2:length(pars)])
  weightedLogit <- weights * (choice - logit)
  gradLLLambda <- t((X %*% beta) - p) %*% weightedLogit
  gradLLBeta <- lambda * (t(X) %*% weightedLogit)
  negGradLL <- -1 * c(gradLLLambda, gradLLBeta)
  return(negGradLL)
}

# Returns the negative hessian of the log-likelihood
mnlHessLL_wtp <- function(pars, modelInputs) {
  lambda <- as.numeric(pars[1])
  beta <- as.numeric(pars[2:length(pars)])
  X <- modelInputs$X
  p <- modelInputs$price
  choice <- modelInputs$choice
  obsID <- modelInputs$obsID
  weights <- modelInputs$weights
  parSetup <- modelInputs$parSetup
  V <- getMnlV_wtp(pars, X, p)
  logit <- getMnlLogit(V, obsID)
  diffMat <- getDiffMatByObsID_wtp(lambda, beta, p, X, logit, obsID)
  logitMat <- repmat(matrix(logit), 1, ncol(diffMat))
  weightsMat <- matrix(rep(weights, length(parSetup)),
    ncol = ncol(X), byrow = F
  )
  hessLL <- -1 * (t(diffMat) %*% (weightsMat * logitMat * diffMat))
  return(hessLL)
}

# Used in computing the hessian
getDiffMatByObsID_wtp <- function(lambda, beta, p, X, logit, obsID) {
  diffMatLambda <- matrix(0, nrow = length(obsID), ncol = 1)
  diffMatBeta <- matrix(0, nrow = length(obsID), ncol = ncol(X))
  for (id in sort(unique(obsID))) {
    indices <- which(obsID == id)
    tempP <- as.matrix(p[indices])
    tempX <- as.matrix(X[indices, ])
    tempLogit <- logit[indices]
    tempMat <- (tempX %*% beta) - tempP
    diffMatLambda[indices, ] <- tempMat - repmat(
      t(tempLogit) %*% tempMat,
      nrow(tempP), 1
    )
    diffMatBeta[indices, ] <- lambda * tempX - repmat(
      t(tempLogit) %*% (lambda * tempX),
      nrow(tempX), 1
    )
  }
  return(cbind(diffMatLambda, diffMatBeta))
}

# ============================================================================
# WTP Space Logit Functions - MXL models
# ============================================================================

# Returns the observed utility
getMxlV_wtp <- function(betaDraws, X, p) {
  numDraws <- nrow(betaDraws)
  lambdaDraws <- matrix(
    rep(betaDraws[, 1], nrow(X)), ncol = numDraws, byrow = T)
  gammaDraws <- matrix(betaDraws[, 2:ncol(betaDraws)], nrow = numDraws)
  pMat <- matrix(rep(p, numDraws), ncol = numDraws, byrow = F)
  return(lambdaDraws * (X %*% t(gammaDraws) - pMat))
}

mxlNegGradLL_wtp <- function(X, parSetup, obsID, choice, standardDraws,
                             betaDraws, VDraws, logitDraws, pHat, weights) {
  stdDraws_lambda <- standardDraws[, 1]
  stdDraws_gamma <- standardDraws[, 2:ncol(standardDraws)]
  randParIDs <- getRandParIDs(parSetup)
  numDraws <- nrow(standardDraws)
  numBetas <- ncol(X)
  numPars <- numBetas + 1 # +1 for lambda par
  xParSetup <- parSetup[which(names(parSetup) != "lambda")]
  xLogNormIDs <- which(xParSetup == "ln")
  repTimes <- rep(as.numeric(table(obsID)), each = 2 * numPars)
  lambdaDraws <- matrix(
    rep(betaDraws[, 1], nrow(X)), ncol = numDraws, byrow = T)
  gammaDraws <- matrix(betaDraws[, 2:ncol(betaDraws)], nrow = numDraws)
  # Compute the gradient of V for all parameters
  grad <- matrix(0, nrow = nrow(X), ncol = 2 * numPars)
  for (i in 1:numDraws) {
    Xtemp <- X
    lambda <- lambdaDraws[, i]
    v <- VDraws[, i]
    draws_lambda <- stdDraws_lambda[i]
    draws_gamma <- stdDraws_gamma[i, ]
    logit <- logitDraws[, i]
    lambdaMat <- matrix(rep(lambda, numBetas), ncol = numBetas, byrow = T)
    drawsMat_lambda <- matrix(rep(draws_lambda, nrow(X)), ncol = 1, byrow = T)
    drawsMat_gamma <- matrix(
      rep(draws_gamma, nrow(X)), ncol = numBetas, byrow = T)
    logitMat <- matrix(rep(logit, numPars), ncol = numPars, byrow = F)
    logitMat <- cbind(logitMat, logitMat)
    if (length(xLogNormIDs) > 0) {
      gamma <- gammaDraws[i, ]
      gammaMat <- matrix(rep(gamma, nrow(X)), ncol = numBetas, byrow = T)
      Xtemp[, xLogNormIDs] <- Xtemp[, xLogNormIDs] * gammaMat[, xLogNormIDs]
    }
    gamma_partial_mu <- lambdaMat * Xtemp
    gamma_partial_sigma <- gamma_partial_mu * drawsMat_gamma
    lambda_partial_mu <- v / lambda
    if (parSetup["lambda"] == "ln") {
      lambda_partial_mu <- v
    }
    lambda_partial_sigma <- lambda_partial_mu * drawsMat_lambda
    partial_mu <- cbind(lambda_partial_mu, gamma_partial_mu)
    partial_sigma <- cbind(lambda_partial_sigma, gamma_partial_sigma)
    partial <- cbind(partial_mu, partial_sigma)
    temp <- rowsum(logitMat * partial, group = obsID, reorder = FALSE)
    tempMat <- matrix(rep(temp, times = repTimes),
      ncol = ncol(partial),
      byrow = F
    )
    grad <- grad + logitMat * (partial - tempMat)
  }
  weightsMat <- matrix(rep(weights, numPars), ncol = numPars, byrow = F)
  weightsMat <- cbind(weightsMat, weightsMat)
  grad <- weightsMat * (grad / numDraws)
  pHatInvChosen <- matrix(rep(choice * (1 / pHat), 2 * numPars),
    ncol = 2 * numPars,
    byrow = F
  )
  grad <- colSums(pHatInvChosen * grad, na.rm = TRUE)
  negGradLL <- -1 * grad[c(1:numPars, numPars + randParIDs)]
  return(negGradLL)
}
