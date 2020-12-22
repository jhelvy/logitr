# ============================================================================
# Functions for taking draws for mixed logit models
# ============================================================================

# Returns shifted normal draws for each parameter
makeBetaDraws <- function(pars, parSetup, numDraws, standardDraws) {
  muMat <- getMuMat(pars, parSetup, numDraws)
  sigmaMat <- getSigmaMat(pars, parSetup, numDraws)
  # Shift draws by mu and sigma
  betaDraws <- muMat + standardDraws * sigmaMat
  # Exponentiate draws for those with logN distribution
  logNormParIDs <- getLogNormParIDs(parSetup)
  if (length(logNormParIDs) > 0) {
    betaDraws[, logNormParIDs] <- exp(betaDraws[, logNormParIDs])
  }
  return(betaDraws)
}

getMuMat <- function(pars, parSetup, numDraws) {
  pars_mu <- as.numeric(pars[seq_len(length(parSetup))])
  muMat <- matrix(rep(pars_mu, numDraws), ncol = length(pars_mu), byrow = T)
  return(muMat)
}

getSigmaMat <- function(pars, parSetup, numDraws) {
  numPars <- length(parSetup)
  pars_sigma <- rep(0, length(parSetup))
  randParIDs <- getRandParIDs(parSetup)
  pars_sigma[randParIDs] <- as.numeric(pars[(numPars + 1):length(pars)])
  sigmaMat <- matrix(rep(pars_sigma, numDraws),
    ncol = length(pars_sigma),
    byrow = T
  )
  return(sigmaMat)
}

getStandardDraws <- function(parSetup, numDraws) {
  numBetas <- length(parSetup)
  draws <- randtoolbox::halton(numDraws, numBetas, normal = TRUE)
  fixedParIDs <- getFixedParIDs(parSetup)
  draws[, fixedParIDs] <- rep(0, numDraws)
  return(draws)
}

getUncertaintyDraws <- function(model, numDraws) {
  varcov <- abs(solve(as.matrix(model$hessian)))
  draws <- data.frame(MASS::mvrnorm(numDraws, model$coef, varcov))
  return(draws)
}
