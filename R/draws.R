# ============================================================================
# Functions for taking draws for mixed logit models
# ============================================================================

# Returns shifted normal draws for each parameter
makeBetaDraws <- function(pars, parIDs, n, standardDraws, correlation) {
  pars_mean <- pars[seq_len(n$vars)]
  pars_sd <- pars[(n$vars + 1):n$pars]
  # First scale the draws according to the covariance matrix
  if (correlation) {
    lowerMat <- matrix(0, n$parsRandom, n$parsRandom)
    lowerMat[lower.tri(lowerMat, diag = TRUE)] <- pars_sd
  } else {
    lowerMat <- diag(pars_sd, ncol = length(pars_sd))
  }
  scaledDraws <- standardDraws
  scaledDraws[,parIDs$random] <- scaledDraws[,parIDs$random] %*% lowerMat
  # Now shift the draws according to the means
  meanMat <- matrix(rep(pars_mean, n$draws), ncol = n$vars, byrow = TRUE)
  betaDraws <- meanMat + scaledDraws
  # log-normal draws: Exponentiate
  if (length(parIDs$logNormal) > 0) {
    betaDraws[, parIDs$logNormal] <- exp(betaDraws[, parIDs$logNormal])
  }
  # Censored normal draws: Censor
  if (length(parIDs$cNormal) > 0) {
    betaDraws[, parIDs$cNormal] <- pmax(betaDraws[, parIDs$cNormal], 0)
  }
  return(betaDraws)
}

getStandardDraws <- function(parIDs, numDraws) {
  numBetas <- length(parIDs$fixed) + length(parIDs$random)
  draws <- as.matrix(randtoolbox::halton(numDraws, numBetas, normal = TRUE))
  draws[, parIDs$fixed] <- 0 * draws[, parIDs$fixed]
  return(draws)
}

getUncertaintyDraws <- function(model, numDraws) {
  coefs <- stats::coef(model)
  covariance <- stats::vcov(model)
  draws <- data.frame(MASS::mvrnorm(numDraws, coefs, covariance))
  colnames(draws) <- names(coefs)
  return(draws)
}
