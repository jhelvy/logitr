# # ============================================================================
# Logit and log-likelihood functions
#
# The log-likelihood function is given as the negative log-likelihood
# because the optimization performs a minimization
#
# object "mi" is the "modelInputs" object
# ============================================================================

# ============================================================================
# MNL logit and log-likelihood functions for both Preference and WTP Spaces
# ============================================================================

negLL <- function(logit, weights) {
  return(-1 * sum(weights * log(logit)))
}

# The configuration here is P = 1 / (1 + sumExpV),
# where sumExpV equals sum_j^(J != j*) exp(V - V*),
# where * indicates the chosen alternative

getMnlLogit <- function(expV, obsID) {
  return(1 / (1 + rowsum(expV, group = obsID, reorder = FALSE)))
}

mnlNegLLAndGradLL <- function(pars, mi) {
  d <- mi$data_diff
  V <- mi$logitFuncs$getMnlV(pars, d$X, d$price)
  expV <- exp(V)
  logit <- getMnlLogit(expV, d$obsID)
  return(list(
    objective = negLL(logit, d$weights),
    gradient = mi$logitFuncs$mnlNegGradLL(
      pars, V, expV, d$X, d$obsID, logit, d$weights)
  ))
}

getMnlNegLL <- function(pars, mi) {
  d <- mi$data_diff
  V <- mi$logitFuncs$getMnlV(pars, d$X, d$price)
  expV <- exp(V)
  logit <- getMnlLogit(expV, d$obsID)
  return(negLL(logit, d$weights))
}

getMnlNegGradLL <- function(pars, mi) {
  d <- mi$data_diff
  V <- mi$logitFuncs$getMnlV(pars, d$X, d$price)
  expV <- exp(V)
  logit <- getMnlLogit(expV, d$obsID)
  return(mi$logitFuncs$mnlNegGradLL(
    pars, V, expV, d$X, d$obsID, logit, d$weights))
}

getMnlHessLL <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  return(getNumericHessLL(pars, mi))
}

# ============================================================================
# Preference Space Logit Functions - MNL models
# ============================================================================

getMnlV_pref <- function(pars, X, price) {
  return(X %*% pars)
}

mnlNegGradLL_pref <- function(pars, V, expV, X, obsID, logit, weights) {
  X_temp <- rowsum(X*expV[,rep(1, ncol(X))], group = obsID, reorder = FALSE)
  return(t(X_temp) %*% (weights * logit))
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
  omega <- pars[2:length(pars)]
  return(lambda * ((X %*% omega) - p))
}

mnlNegGradLL_wtp <- function(pars, V, expV, X, obsID, logit, weights) {
  lambda <- pars[1]
  X_temp <- rowsum(
    cbind((V / lambda), lambda*X) * expV[,rep(1, (ncol(X) + 1))],
    group = obsID, reorder = FALSE)
  return(t(X_temp) %*% (weights * logit))
}

mnlHessLL_wtp <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  return(getNumericHessLL(pars, mi))
}

# ============================================================================
# MXL logit and log-likelihood functions for both Preference and WTP Spaces
# ============================================================================

# Returns the logit fraction for all the draws in a mxl (heterogeneous) models
getMxlLogit <- function(VDraws, obsID, repTimesMxl, numDraws) {
  expVDraws <- exp(VDraws)
  sumExpVDraws <- rowsum(expVDraws, group = obsID, reorder = FALSE)
  sumExpVDrawsMat <- matrix(
    rep(sumExpVDraws, times = repTimesMxl), ncol = numDraws, byrow = FALSE)
  return(expVDraws / sumExpVDrawsMat)
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
mxlNegLLAndGradLL <- function(pars, mi) {
  betaDraws <- makeBetaDraws(
    pars, mi$parIDs, mi$inputs$numDraws, mi$standardDraws)
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, mi$X, mi$price)
  logitDraws <- getMxlLogit(
    VDraws, mi$obsID, mi$repTimesMxl, mi$inputs$numDraws)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(list(
    objective = negLL(mi$choice, pHat, mi$weights),
    gradient = mi$logitFuncs$mxlNegGradLL(
      mi$X, mi$parSetup, mi$obsID, mi$choice, mi$standardDraws, betaDraws,
      VDraws, logitDraws, pHat, mi$weights, mi$inputs$numDraws, mi$numBetas,
      mi$repTimesMxlGrad, mi$parIDs)
  ))
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
getMxlNegLL <- function(pars, mi) {
  betaDraws <- makeBetaDraws(
    pars, mi$parIDs, mi$inputs$numDraws, mi$standardDraws)
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, mi$X, mi$price)
  logitDraws <- getMxlLogit(
    VDraws, mi$obsID, mi$repTimesMxl, mi$inputs$numDraws)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(negLL(mi$choice, pHat, mi$weights))
}

getMxlNegGradLL <- function(pars, mi) {
  betaDraws <- makeBetaDraws(
    pars, mi$parIDs, mi$inputs$numDraws, mi$standardDraws)
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, mi$X, mi$price)
  logitDraws <- getMxlLogit(
    VDraws, mi$obsID, mi$repTimesMxl, mi$inputs$numDraws)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(mi$logitFuncs$mxlNegGradLL(
    mi$X, mi$parSetup, mi$obsID, mi$choice, mi$standardDraws, betaDraws,
    VDraws, logitDraws, pHat, mi$weights, mi$inputs$numDraws, mi$numBetas,
    mi$repTimesMxlGrad, mi$parIDs)
  )
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
  pHat, weights, numDraws, numBetas, repTimesMxlGrad, parIDs
) {
  lnID <- parIDs$logNormal
  hasLnIDs <- length(lnID) > 0
  lnIDs <- c(lnID, lnID + numBetas)
  X2 <- repmat(X, 1, 2)
  grad <- matrix(0, nrow = nrow(X), ncol = 2 * numBetas)
  for (i in 1:numDraws) {
    Xtemp <- X2
    if (hasLnIDs) {
      betaMat <- repmat(matrix(betaDraws[i, lnID], nrow = 1), nrow(X), 2)
      Xtemp[, lnIDs] <- Xtemp[, lnIDs] * betaMat
    }
    drawsMat <- repmat(
      matrix(c(rep(1, numBetas), standardDraws[i, ]), nrow = 1), nrow(X), 1)
    partial <- Xtemp * drawsMat
    logitMat <- repmat(matrix(logitDraws[, i]), 1, 2*numBetas)
    sumsMat <- rowsum(logitMat * partial, group = obsID, reorder = FALSE)
    sumsMat <- matrix(
      rep(sumsMat, times = repTimesMxlGrad),
      ncol = ncol(partial),
      byrow = F
    )
    grad <- grad + logitMat * (partial - sumsMat)
  }
  weightsMat <- repmat(matrix(weights), 1, 2*numBetas)
  pHatInvChosen <- repmat(matrix(choice / pHat), 1, 2*numBetas)
  grad <- colSums(pHatInvChosen * weightsMat * (grad / numDraws), na.rm = TRUE)
  negGradLL <- -1 * grad[c(1:numBetas, numBetas + parIDs$random)]
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

mxlNegGradLL_wtp <- function(
  X, parSetup, obsID, choice, standardDraws, betaDraws, VDraws, logitDraws,
  pHat, weights, numDraws, numBetas, repTimesMxlGrad, parIDs
) {
  numPars <- numBetas
  numBetas <- numBetas - 1 # subtract lambda par
  draws_lambda <- standardDraws[, 1]
  draws_gamma <- standardDraws[, 2:ncol(standardDraws)]
  numPars <- numBetas + 1 # +1 for lambda par
  xlnID <- parIDs$logNormal - 1
  hasLnIDs <- length(xlnID) > 0
  lambdaDraws <- repmat(matrix(betaDraws[, 1], nrow = 1), nrow(X), 1)
  gammaDraws <- betaDraws[, 2:ncol(betaDraws)]
  lambda_partial_mu_draws <- VDraws / lambdaDraws
  if (parSetup["lambda"] == "ln") {
    lambda_partial_mu_draws <- VDraws
  }
  grad <- matrix(0, nrow = nrow(X), ncol = 2 * numPars)
  for (i in 1:numDraws) {
    Xtemp <- X
    if (hasLnIDs) {
      gammaMat <- repmat(matrix(gammaDraws[i, xlnID], nrow = 1), nrow(X), 1)
      Xtemp[, xlnID] <- Xtemp[, xlnID] * gammaMat
    }
    drawsMat_lambda <- repmat(matrix(draws_lambda[i]), nrow(X), 1)
    drawsMat_gamma <- repmat(matrix(draws_gamma[i, ], nrow = 1), nrow(X), 1)
    lambdaMat <- repmat(matrix(lambdaDraws[, i]), 1, numBetas)
    gamma_partial_mu <- lambdaMat * Xtemp
    gamma_partial_sigma <- gamma_partial_mu * drawsMat_gamma
    lambda_partial_mu <- lambda_partial_mu_draws[,i]
    lambda_partial_sigma <- lambda_partial_mu * drawsMat_lambda
    partial <- cbind(
      lambda_partial_mu, gamma_partial_mu,
      lambda_partial_sigma,  gamma_partial_sigma
    )
    logitMat <- repmat(matrix(logitDraws[, i]), 1, 2*numPars)
    sumsMat <- rowsum(logitMat * partial, group = obsID, reorder = FALSE)
    sumsMat <- matrix(
      rep(sumsMat, times = repTimesMxlGrad),
      ncol = ncol(partial),
      byrow = F
    )
    grad <- grad + logitMat * (partial - sumsMat)
  }
  weightsMat <- repmat(matrix(weights), 1, 2*numPars)
  pHatInvChosen <- repmat(matrix(choice / pHat), 1, 2*numPars)
  grad <- colSums(pHatInvChosen * weightsMat * (grad / numDraws), na.rm = TRUE)
  negGradLL <- -1 * grad[c(1:numPars, numPars + parIDs$random)]
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
