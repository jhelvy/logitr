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
  pars.mu <- as.numeric(pars[1:length(parSetup)])
  muMat <- matrix(rep(pars.mu, numDraws), ncol = length(pars.mu), byrow = T)
  return(muMat)
}

getSigmaMat <- function(pars, parSetup, numDraws) {
  numPars <- length(parSetup)
  pars.sigma <- rep(0, length(parSetup))
  randParIDs <- getRandParIDs(parSetup)
  pars.sigma[randParIDs] <- as.numeric(pars[(numPars + 1):length(pars)])
  sigmaMat <- matrix(rep(pars.sigma, numDraws),
    ncol = length(pars.sigma),
    byrow = T
  )
  return(sigmaMat)
}

getStandardDraws <- function(parSetup, numDraws, drawType = "halton") {
  numBetas <- length(parSetup)
  # Use Halton draws by default
  draws <- getHaltonDraws(numDraws, numBetas)
  if (drawType == "normal") {
    draws <- getNormalDraws(numDraws, numBetas)
  }
  fixedParIDs <- getFixedParIDs(parSetup)
  draws[, fixedParIDs] <- rep(0, numDraws)
  return(draws)
}

# Returns a matrix of numDraws x numBetas random standard normal draws
getNormalDraws <- function(numDraws, numBetas) {
  N <- numDraws * numBetas
  draws <- stats::rnorm(N, 0, 1)
  badDraws <- which(stats::pnorm(draws) < 0.001 | stats::pnorm(draws) > 0.999)
  # Make sure that none of the draws are too extreme in the tail. It
  # causes the likelihood function to crash because the utility value
  # becomes too close to zero, and then taking log(0) crashes.
  while (length(badDraws) > 0) {
    draws[badDraws] <- stats::rnorm(length(badDraws), 0, 1)
    badDraws <- which(stats::pnorm(draws) < 0.001 | stats::pnorm(draws) > 0.999)
  }
  drawsMatrix <- matrix(draws, ncol = numBetas)
  return(drawsMatrix)
}

# Returns a matrix of numDraws x numBetas random standard normal draws
# approximated using Halton draws
# *Function coped and modified from the mlogit package on CRAN
getHaltonDraws <- function(R, Ka, halton = NA) {
  # R = numDraws, Ka = numBetas
  # Create the matrix of random numbers
  if (!is.null(halton)) {
    length.halton <- rep(R, Ka)
    prime <- c(
      2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
      47, 53, 59, 61, 71, 73, 79, 83, 89, 97, 101, 103,
      107, 109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167,
      173, 179, 181, 191, 193, 197, 199
    )
    drop.halton <- rep(100, Ka)
    if (!is.na(halton) && !is.null(halton$prime)) {
      if (length(halton$prime) != Ka) {
        stop("wrong number of prime numbers indicated")
      }
      else {
        prime <- halton$prime
      }
      if (!is.na(halton) && !is.null(halton$drop)) {
        if (!length(halton$drop) %in% c(1, Ka)) stop("wrong number of drop indicated")
        if (length(halton$drop) == 1) {
          drop.halton <- rep(halton$drop, Ka)
        }
        else {
          drop.halton <- halton$drop
        }
      }
    }
    random.nb <- numeric(0)
    i <- 0
    for (i in 1:Ka) {
      random.nb <- cbind(random.nb, stats::qnorm(halton(prime[i], R, drop.halton[i])))
    }
  }
  else {
    random.nb <- matrix(stats::rnorm(R * Ka), ncol = Ka, nrow = R)
  }
  return(random.nb)
}

halton <- function(prime = 3, length = 100, drop = 10) {
  halt <- 0
  t <- 0
  while (length(halt) < length + drop) {
    t <- t + 1
    halt <- c(halt, rep(halt, prime - 1) + rep(seq(1, prime - 1, 1) / prime^t, each = length(halt)))
  }
  return(halt[(drop + 1):(length + drop)])
}

getUncertaintyDraws <- function(model, numDraws) {
  varcov <- abs(solve(as.matrix(model$hessian)))
  draws <- data.frame(mvrnorm(numDraws, model$coef, varcov))
  return(draws)
}

# mvrnorm function
# *Copied from the MASS package on CRAN
mvrnorm <- function(n = 1, mu, Sigma, tol = 1e-6, empirical = FALSE, EISPACK = FALSE) {
  p <- length(mu)
  if (!all(dim(Sigma) == c(p, p))) stop("incompatible arguments")
  if (EISPACK) stop("'EISPACK' is no longer supported by R", domain = NA)
  eS <- eigen(Sigma, symmetric = TRUE)
  ev <- eS$values
  if (!all(ev >= -tol * abs(ev[1L]))) stop("'Sigma' is not positive definite")
  X <- matrix(stats::rnorm(p * n), n)
  if (empirical) {
    X <- scale(X, TRUE, FALSE) # remove means
    X <- X %*% svd(X, nu = 0)$v # rotate to PCs
    X <- scale(X, FALSE, TRUE) # rescale PCs to unit variance
  }
  X <- drop(mu) + eS$vectors %*% diag(sqrt(pmax(ev, 0)), p) %*% t(X)
  nm <- names(mu)
  if (is.null(nm) && !is.null(dn <- dimnames(Sigma))) nm <- dn[[1L]]
  dimnames(X) <- list(nm, NULL)
  if (n == 1) drop(X) else t(X)
}
