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

getLogit <- function(expV, obsID) {
  return(1 / (1 + rowsum(expV, group = obsID, reorder = FALSE)))
}

mnlNegLLAndGradLL <- function(pars, mi) {
  d <- mi$data_diff
  V <- mi$logitFuncs$getMnlV(pars, d$X, d$price)
  expV <- exp(V)
  logit <- getLogit(expV, d$obsID)
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
  logit <- getLogit(expV, d$obsID)
  return(negLL(logit, d$weights))
}

getMnlNegGradLL <- function(pars, mi) {
  d <- mi$data_diff
  V <- mi$logitFuncs$getMnlV(pars, d$X, d$price)
  expV <- exp(V)
  logit <- getLogit(expV, d$obsID)
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

# Returns the negative log-likelihood of an mxl (heterogeneous) model
mxlNegLLAndGradLL <- function(pars, mi) {
  d <- mi$data_diff
  betaDraws <- makeBetaDraws(
    pars, mi$parIDs, mi$inputs$numDraws, mi$standardDraws)
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, d$X, d$price)
  expVDraws <- exp(VDraws)
  logitDraws <- getLogit(expVDraws, d$obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(list(
    objective = negLL(pHat, d$weights),
    gradient = mi$logitFuncs$mxlNegGradLL(
      betaDraws, VDraws, expVDraws, logitDraws, pHat, mi$partials, d$obsID,
      mi$parIDs, d$weights, mi$numBetas, mi$nrowX, mi$inputs$numDraws,
      mi$inputs$randPrice)
  ))
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
getMxlNegLL <- function(pars, mi) {
  d <- mi$data_diff
  betaDraws <- makeBetaDraws(
    pars, mi$parIDs, mi$inputs$numDraws, mi$standardDraws)
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, d$X, d$price)
  expVDraws <- exp(VDraws)
  logitDraws <- getLogit(expVDraws, d$obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(negLL(pHat, d$weights))
}

getMxlNegGradLL <- function(pars, mi) {
  d <- mi$data_diff
  betaDraws <- makeBetaDraws(
    pars, mi$parIDs, mi$inputs$numDraws, mi$standardDraws)
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, d$X, d$price)
  expVDraws <- exp(VDraws)
  logitDraws <- getLogit(expVDraws, d$obsID)
  pHat <- rowMeans(logitDraws, na.rm = T)
  return(mi$logitFuncs$mxlNegGradLL(
      betaDraws, VDraws, expVDraws, logitDraws, pHat, mi$partials, d$obsID,
      mi$parIDs, d$weights, mi$numBetas, mi$nrowX, mi$inputs$numDraws,
      mi$inputs$randPrice)
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

mxlNegGradLL_pref <- function(
  betaDraws, VDraws, expVDraws, logitDraws, pHat, partials, obsID, parIDs,
  weights, numBetas, nrowX, numDraws, randPrice
) {
  # First, adjust partials for any log-normal parameters
  partials <- updatePartials(partials, parIDs, betaDraws, nrowX, numBetas)
  # Now compute the gradient
  return(computeMxlNegGradLL(
    expVDraws, logitDraws, partials, obsID, weights, pHat, numDraws))
}

mxlHessLL_pref <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  # Note that this is only used post-estimation, so there are only marginal
  # benefits in terms of speed improvements from specifying an analytic hessian
  return(getNumericHessLL(pars, mi))
}

# Updates each of the partials matrices based on if the parameter follows
# a log-normal distribution
updatePartials <- function(partials, parIDs, betaDraws, nrowX, numBetas) {
  if (length(parIDs$logNormal) > 0) {
    numFixed <- length(parIDs$fixed)
    for (id in parIDs$logNormal) {
      id_sigma <- id + numBetas - numFixed
      betaMat <- repmat(matrix(betaDraws[,id], nrow = 1), nrowX, 1)
      partials[[id]] <- partials[[id]]*betaMat
      partials[[id_sigma]] <- partials[[id_sigma]]*betaMat
    }
  }
  return(partials)
}

computeMxlNegGradLL <- function(
  expVDraws, logitDraws, partials, obsID, weights, pHat, numDraws
) {
  logitDrawsSq <- logitDraws^2
  grads <- lapply(partials, function(x) {
    rowSums(
      logitDrawsSq * rowsum(x*expVDraws, group = obsID, reorder = FALSE)
    )
  })
  grad <- matrix(unlist(grads), ncol = length(partials), byrow = FALSE)
  return(t(grad) %*% (weights / (pHat * numDraws)))
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
  betaDraws, VDraws, expVDraws, logitDraws, pHat, partials, obsID, parIDs,
  weights, numBetas, nrowX, numDraws, randPrice
) {
  # First, adjust partials for any log-normal parameters
  partials <- updatePartials(partials, parIDs, betaDraws, nrowX, numBetas)
  # Now adjust the partials for the lambda and omega parameters
  lambdaDraws <- repmat(matrix(betaDraws[,1], nrow = 1), nrowX, 1)
  partial_lambda_mu <- VDraws / lambdaDraws
  partials[[1]] <- partial_lambda_mu
  if (!is.null(randPrice)) {
    lambda_sigmaID <- parIDs$lambdaIDs[2]
    partials[[lambda_sigmaID]] <- partials[[lambda_sigmaID]]*partial_lambda_mu
  }
  for (id in parIDs$omegaIDs) {
    partials[[id]] <- partials[[id]]*lambdaDraws
  }
  # Now compute the gradient
  return(computeMxlNegGradLL(
    expVDraws, logitDraws, partials, obsID, weights, pHat, numDraws))
}

mxlHessLL_wtp <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  # Note that this is only used post-estimation, so there are only marginal
  # benefits in terms of speed improvements from specifying an analytic hessian
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
