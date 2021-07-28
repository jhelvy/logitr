# ============================================================================
# Functions for taking draws for mixed logit models
# ============================================================================

# Returns shifted normal draws for each parameter
makeBetaDraws <- function(pars, parIDs, numDraws, standardDraws) {
  numBetas <- length(parIDs$fixed) + length(parIDs$random)
  pars_mu <- as.numeric(pars[seq_len(numBetas)])
  muMat <- matrix(rep(pars_mu, numDraws), ncol = length(pars_mu), byrow = T)
  pars_sigma <- rep(0, numBetas)
  pars_sigma[parIDs$random] <- as.numeric(pars[(numBetas + 1):length(pars)])
  sigmaMat <- matrix(
    rep(pars_sigma, numDraws),
    ncol = length(pars_sigma),
    byrow = TRUE
  )
  # Shift draws by mu and sigma
  betaDraws <- muMat + standardDraws * sigmaMat
  # Exponentiate draws for those with logN distribution
  if (length(parIDs$logNormal) > 0) {
    betaDraws[, parIDs$logNormal] <- exp(betaDraws[, parIDs$logNormal])
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
