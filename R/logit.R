# ============================================================================
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
  # Note that this is only used post-estimation, so there are only marginal
  # benefits in terms of speed improvements from specifying an analytic hessian
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
  # Note that this is only used post-estimation, so there are only marginal
  # benefits in terms of speed improvements from specifying an analytic hessian
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
  # Note that this is only used post-estimation, so there are only marginal
  # benefits in terms of speed improvements from specifying an analytic hessian
  return(getNumericHessLL(pars, mi))
}

# ============================================================================
# MXL logit and log-likelihood functions for both Preference and WTP Spaces
# ============================================================================

# Returns the negative log-likelihood of an mxl (heterogeneous) model
mxlNegLLAndGradLL <- function(pars, mi) {
  d <- mi$data_diff
  betaDraws <- makeBetaDraws(
    pars, mi$parIDs, mi$n, mi$standardDraws, mi$inputs$correlation
  )
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, d$X, d$price, mi$n)
  expVDraws <- exp(VDraws)
  logitDraws <- getLogit(expVDraws, d$obsID)
  logitDrawsPanel <- logitDraws
  if (mi$panel) {
    logitDrawsPanel <- exp(rowsum(log(logitDraws), d$panelID))
    pHat <- rowMeans(logitDrawsPanel, na.rm = T)
  } else {
    pHat <- rowMeans(logitDraws, na.rm = T)
  }
  return(list(
    objective = negLL(pHat, d$weights),
    gradient = mi$logitFuncs$mxlNegGradLL(
      betaDraws, VDraws, expVDraws, logitDraws, logitDrawsPanel, pHat,
      mi$partials, d$obsID, d$panelID, mi$parIDs, d$weights, mi$n,
      mi$inputs$randPrice, mi$panel)
  ))
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
getMxlNegLL <- function(pars, mi) {
  d <- mi$data_diff
  betaDraws <- makeBetaDraws(
    pars, mi$parIDs, mi$n, mi$standardDraws, mi$inputs$correlation
  )
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, d$X, d$price, mi$n)
  expVDraws <- exp(VDraws)
  logitDraws <- getLogit(expVDraws, d$obsID)
  logitDrawsPanel <- logitDraws
  if (mi$panel) {
    logitDrawsPanel <- exp(rowsum(log(logitDraws), d$panelID))
    pHat <- rowMeans(logitDrawsPanel, na.rm = T)
  } else {
    pHat <- rowMeans(logitDraws, na.rm = T)
  }
  return(negLL(pHat, d$weights))
}

getMxlNegGradLL <- function(pars, mi) {
  d <- mi$data_diff
  betaDraws <- makeBetaDraws(
    pars, mi$parIDs, mi$n, mi$standardDraws, mi$inputs$correlation
  )
  VDraws <- mi$logitFuncs$getMxlV(betaDraws, d$X, d$price, mi$n)
  expVDraws <- exp(VDraws)
  logitDraws <- getLogit(expVDraws, d$obsID)
  logitDrawsPanel <- logitDraws
  if (mi$panel) {
    logitDrawsPanel <- exp(rowsum(log(logitDraws), d$panelID))
    pHat <- rowMeans(logitDrawsPanel, na.rm = T)
  } else {
    pHat <- rowMeans(logitDraws, na.rm = T)
  }
  return(mi$logitFuncs$mxlNegGradLL(
      betaDraws, VDraws, expVDraws, logitDraws, logitDrawsPanel, pHat,
      mi$partials, d$obsID, d$panelID, mi$parIDs, d$weights, mi$n,
      mi$inputs$randPrice, mi$panel)
  )
}

getMxlHessLL <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  # Note that this is only used post-estimation, so there are only marginal
  # benefits in terms of speed improvements from specifying an analytic hessian
  return(getNumericHessLL(pars, mi))
}

# Updates each of the partials matrices based on if the parameter follows
# a log-normal or censored-normal distribution
updatePartials <- function(partials, parIDs, betaDraws, n) {
  # Log-normal
  lnIDs <- parIDs$ln
  if (length(lnIDs) > 0) {
    for (i in 1:length(lnIDs)) {
      ids <- parIDs$partial_lnIDs[[i]]
      betaMat <- repmat(matrix(betaDraws[,lnIDs[i]], nrow = 1), n$rowX, 1)
      for (j in ids) {
        partials[[j]] <- partials[[j]]*betaMat
      }
    }
  }
  return(partials)
}

computeMxlNegGradLL <- function(
  expVDraws, logitDraws, logitDrawsPanel, partials, obsID, panelID, weights,
  pHat, n, panel
) {
  if (panel) {
    grads <- lapply(partials, function(x) {
      rowSums(logitDrawsPanel * rowsum(
        logitDraws * rowsum(x*expVDraws, group = obsID, reorder = FALSE),
        group = panelID)
      )
    })
  } else {
    grads <- lapply(partials, function(x) {
      rowSums(logitDraws^2 * rowsum(
        x*expVDraws, group = obsID, reorder = FALSE)
      )
    })
  }
  grad <- matrix(unlist(grads), ncol = length(partials), byrow = FALSE)
  return(t(grad) %*% (weights / (pHat * n$draws)))
}

# ============================================================================
# Preference Space Logit Functions - MXL models
# ============================================================================

# Returns the observed utility
getMxlV_pref <- function(betaDraws, X, p, n) {
  return(X %*% t(betaDraws))
}

mxlNegGradLL_pref <- function(
  betaDraws, VDraws, expVDraws, logitDraws, logitDrawsPanel, pHat, partials,
  obsID, panelID, parIDs, weights, n, randPrice, panel
) {
  # First, adjust partials for any log-normal parameters
  partials <- updatePartials(partials, parIDs, betaDraws, n)
  # Now compute the gradient
  return(computeMxlNegGradLL(
    expVDraws, logitDraws, logitDrawsPanel, partials, obsID, panelID, weights,
    pHat, n, panel)
  )
}

mxlHessLL_pref <- function(pars, mi) {
  # Need to define analytic hessian - use numeric approximation for now
  # Note that this is only used post-estimation, so there are only marginal
  # benefits in terms of speed improvements from specifying an analytic hessian
  return(getNumericHessLL(pars, mi))
}

# ============================================================================
# WTP Space Logit Functions - MXL models
# ============================================================================

# Returns the observed utility
getMxlV_wtp <- function(betaDraws, X, p, n) {
  lambdaDraws <- matrix(
    rep(betaDraws[, 1], nrow(X)), ncol = n$draws, byrow = T)
  gammaDraws <- matrix(betaDraws[, 2:ncol(betaDraws)], nrow = n$draws)
  pMat <- matrix(rep(p, n$draws), ncol = n$draws, byrow = F)
  return(lambdaDraws * (X %*% t(gammaDraws) - pMat))
}

mxlNegGradLL_wtp <- function(
  betaDraws, VDraws, expVDraws, logitDraws, logitDrawsPanel, pHat, partials,
  obsID, panelID, parIDs, weights, n, randPrice, panel
) {
  # First, adjust partials for any log-normal parameters
  partials <- updatePartials(partials, parIDs, betaDraws, n)
  # Now adjust the partials for the lambda and omega parameters
  lambdaDraws <- repmat(matrix(betaDraws[,1], nrow = 1), n$rowX, 1)
  partial_lambda_mean <- VDraws / lambdaDraws
  partials[[1]] <- partial_lambda_mean
  if (!is.null(randPrice)) {
    lambda_sdID <- parIDs$lambdaIDs[2]
    partials[[lambda_sdID]] <- partials[[lambda_sdID]]*partial_lambda_mean
    # Account for if lambda is log-normally distributed
    if (length(parIDs$ln) > 0) {
      if (parIDs$ln[1] == 1) {
        partials[[1]] <- partials[[1]]*lambdaDraws
      }
    }
  }
  for (id in parIDs$omegaIDs) {
    partials[[id]] <- partials[[id]]*lambdaDraws
  }
  # Account for lambda in correlated pars
  if (!is.null(randPrice)) {
    for (id in parIDs$lambdaOffDiag) {
      partials[[id]] <- partials[[id]] / lambdaDraws * partial_lambda_mean
    }
  }
  # Now compute the gradient
  return(computeMxlNegGradLL(
    expVDraws, logitDraws, logitDrawsPanel, partials, obsID, panelID, weights,
    pHat, n, panel)
  )
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
