# # ============================================================================
# Logit and log-likelihood functions
#
# The log-likelihood function is given as the negative log-likelihood
# because the optimization performs a minimization
#
# object "mi" is the "modelInputs" object
# ============================================================================

negLL <- function(choice, logit, weights) {
  return(-1 * sum(weights * choice * log(logit)))
}

# ============================================================================
# MNL logit and log-likelihood functions for both Preference and WTP Spaces
# ============================================================================

# Returns the logit fraction for mnl (fixed parameter) models
getMnlLogit <- function(V, obsID, repTimes) {
  expV <- exp(V)
  sumExpV <- rowsum(expV, group = obsID, reorder = FALSE)
  sumExpVMat <- matrix(rep(sumExpV, times = repTimes), ncol = 1)
  return(expV / sumExpVMat)
}

# Returns a list containing the negative log-likelihood and it's gradient
# Primary objective function for the nloptr optimizer.
mnlNegLLAndGradLL <- function(pars, mi) {
  V <- mi$logitFuncs$getMnlV(pars, mi$X, mi$price)
  logit <- getMnlLogit(V, mi$obsID, mi$repTimes)
  return(list(
    objective = negLL(mi$choice, logit, mi$weights),
    gradient = mi$logitFuncs$mnlNegGradLL(
      pars, mi$X, mi$price, mi$choice, logit, mi$weights)
  ))
}

getMnlNegLL <- function(pars, mi) {
  V <- mi$logitFuncs$getMnlV(pars, mi$X, mi$price)
  logit <- getMnlLogit(V, mi$obsID, mi$repTimes)
  return(negLL(mi$choice, logit, mi$weights))
}

getMnlNegGradLL <- function(pars, mi) {
  V <- mi$logitFuncs$getMnlV(pars, mi$X, mi$price)
  logit <- getMnlLogit(V, mi$obsID, mi$repTimes)
  return(mi$logitFuncs$mnlNegGradLL(
    pars, mi$X, mi$price, mi$choice, logit, mi$weights))
}

getMnlHessLL <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  return(getNumericHessLL(pars, mi))
}

# ============================================================================
# Preference Space Logit Functions - MNL models
# ============================================================================

getMnlV_pref <- function(pars, X, p) {
  return(X %*% pars)
}

mnlNegGradLL_pref <- function(pars, X, p, choice, logit, weights) {
  weightedDiff <- weights * (choice - logit)
  return(-1 * (t(X) %*% weightedDiff))
}

mnlHessLL_pref <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  return(getNumericHessLL(pars, mi))
}

# ============================================================================
# WTP Space Logit Functions - MNL models
# ============================================================================

# Returns the observed utility
getMnlV_wtp <- function(pars, X, p) {
  lambda <- pars[1]
  beta <- pars[2:length(pars)]
  return(lambda * ((X %*% beta) - p))
}

# Returns the negative gradient of the log-likelihood
mnlNegGradLL_wtp <- function(pars, X, p, choice, logit, weights) {
  lambda <- pars[1]
  beta <- pars[2:length(pars)]
  weightedDiff <- weights * (choice - logit)
  gradLLLambda <- t((X %*% beta) - p) %*% weightedDiff
  gradLLBeta <- lambda * (t(X) %*% weightedDiff)
  return(-1 * c(gradLLLambda, gradLLBeta))
}

mnlHessLL_wtp <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  return(getNumericHessLL(pars, mi))
}

# ============================================================================
# MXL logit and log-likelihood functions for both Preference and WTP Spaces
# ============================================================================

# Returns the logit fraction for all the draws in a mxl (heterogeneous) models
getMxlLogit <- function(VDraws, obsID) {
  numDraws <- ncol(VDraws)
  expVDraws <- exp(VDraws)
  sumExpVDraws <- rowsum(expVDraws, group = obsID, reorder = FALSE)
  repTimes <- rep(as.numeric(table(obsID)), times = numDraws)
  sumExpVDrawsMat <- matrix(
    rep(sumExpVDraws, times = repTimes), ncol = numDraws, byrow = FALSE)
  return(expVDraws / sumExpVDrawsMat)
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
mxlNegLLAndGradLL <- function(pars, mi) {
  betaDraws <- makeBetaDraws(
    pars, mi$parSetup, mi$inputs$numDraws, mi$standardDraws)
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, mi$X, mi$price)
  logitDraws <- getMxlLogit(VDraws, mi$obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(list(
    objective = negLL(mi$choice, pHat, mi$weights),
    gradient = mi$logitFuncs$mxlNegGradLL(
      mi$X, mi$parSetup, mi$obsID, mi$choice, mi$standardDraws, betaDraws,
      VDraws, logitDraws, pHat, mi$weights)
  ))
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
getMxlNegLL <- function(pars, mi) {
  betaDraws <- makeBetaDraws(
    pars, mi$parSetup, mi$inputs$numDraws, mi$standardDraws)
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, mi$X, mi$price)
  logitDraws <- getMxlLogit(VDraws, mi$obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(negLL(mi$choice, pHat, mi$weights))
}

getMxlNegGradLL <- function(pars, mi) {
  betaDraws <- makeBetaDraws(
    pars, mi$parSetup, mi$inputs$numDraws, mi$standardDraws)
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, mi$X, mi$price)
  logitDraws <- getMxlLogit(VDraws, mi$obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(logitFuncs$mxlNegGradLL(
    mi$X, mi$parSetup, mi$obsID, mi$choice, mi$standardDraws, betaDraws,
    VDraws, logitDraws, pHat, mi$weights
  ))
}

getMxlHessLL <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  return(getNumericHessLL(pars, mi))
}

# ============================================================================
# Preference Space Logit Functions - MXL models
# ============================================================================

# Returns the observed utility
getMxlV_pref <- function(betaDraws, X, p) {
  return(X %*% t(betaDraws))
}

# Computes the gradient of the negative likelihood for a mixed logit model
mxlNegGradLL_pref <- function(
  X, parSetup, obsID, choice, standardDraws, betaDraws, VDraws, logitDraws,
  pHat, weights
) {
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
  pHatInvChosen <- matrix(
    rep(choice * (1 / pHat), 2 * numBetas),
    ncol = 2 * numBetas, byrow = F
  )
  grad <- colSums(pHatInvChosen * grad, na.rm = TRUE)
  negGradLL <- -1 * grad[c(1:numBetas, numBetas + randParIDs)]
  return(negGradLL)
}

mxlHessLL_pref <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  return(getNumericHessLL(pars, mi))
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

mxlHessLL_wtp <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  return(getNumericHessLL(pars, mi))
}

# ============================================================================
# Numerical log-likelihood functions for both Preference and WTP Spaces
# ============================================================================

negLLAndNumericGradLL <- function(pars, mi) {
  return(list(
    objective = mi$evalFuncs$negLL(pars, mi),
    gradient = getNumericNegGradLL(pars, mi)
  ))
}

getNumericNegGradLL <- function(pars, mi) {
  return(nloptr::nl.jacobian(
    x0 = pars,
    fn = mi$evalFuncs$negLL,
    mi = mi
  ))
}

getNumericHessLL <- function(pars, mi) {
  return(-1 * nloptr::nl.jacobian(
    x0 = pars,
    fn = mi$evalFuncs$negGradLL,
    mi = mi
  ))
}
