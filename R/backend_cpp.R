# ============================================================================
# Compiled ("cpp") backend: routes the MXL log-likelihood + gradient through
# the fused Rcpp kernel in src/mxl_backend.cpp. Same math and results as the
# native R path (validated to machine precision), but faster and memory-flat.
#
# Coverage so far: preference- and WTP-space MXL, uncorrelated heterogeneity,
# normal / log-normal / censored-normal distributions, fixed + random
# parameters, panel and non-panel data, weights. Unsupported model features
# fall back with a clear error via cppSupported() so results are never
# silently wrong.
# ============================================================================

# Which models the cpp backend can currently handle. All MXL model types are
# now supported (preference and WTP space, uncorrelated and correlated, normal
# / log-normal / censored-normal); this remains as an extension point.
cppSupported <- function(modelSpace, correlation) {
  invisible(TRUE)
}

# Build the covariance factor used for betaDraws: a diagonal matrix of the sd
# values when uncorrelated, or the lower-triangular Cholesky when correlated.
cppCholFactor <- function(mi, pars_sd) {
  nVars <- mi$n$vars
  rIDs <- mi$parIDs$r
  chol <- matrix(0, nVars, nVars)
  if (isTRUE(mi$inputs$correlation)) {
    nR <- length(rIDs)
    L <- matrix(0, nR, nR)
    L[lower.tri(L, diag = TRUE)] <- pars_sd
    chol[rIDs, rIDs] <- L
  } else {
    sdFull <- numeric(nVars)
    if (length(rIDs) > 0) sdFull[rIDs] <- pars_sd
    diag(chol) <- sdFull
  }
  chol
}

# Build the C++ kernel arguments from the modelInputs + current parameters
cppPrep <- function(pars, mi) {
  nVars <- mi$n$vars
  parIDs <- mi$parIDs
  d <- mi$data_diff
  rIDs <- parIDs$r
  mean <- pars[seq_len(nVars)]
  pars_sd <- if (length(rIDs) > 0) pars[(nVars + 1):mi$n$pars] else numeric(0)

  # Distribution codes: 0 normal/fixed, 1 log-normal, 2 censored-normal
  dist <- integer(nVars)
  if (length(parIDs$ln) > 0) dist[parIDs$ln] <- 1L
  if (length(parIDs$cn) > 0) dist[parIDs$cn] <- 2L

  panel <- mi$panel
  panelID <- if (panel) as.integer(d$panelID) else integer(0)
  nPanel <- if (panel) length(unique(d$panelID)) else 0L

  numThreads <- mi$inputs$numThreads
  if (is.null(numThreads)) numThreads <- 1L

  common <- list(
    draws = mi$standardDraws, mean = as.numeric(mean), dist = dist,
    obsID = as.integer(d$obsID), panelID = panelID,
    weights = as.numeric(d$weights),
    nObs = as.integer(mi$n$obs), nPanel = nPanel, nPars = as.integer(mi$n$pars),
    numThreads = as.integer(numThreads)
  )

  chol <- cppCholFactor(mi, pars_sd)
  spec <- buildPartialSpec(mi)   # xcol/dcol: 1-based var positions (dcol 0 = ones)
  dcolDraw <- as.integer(ifelse(spec$dcol == 0L, -1L, spec$dcol - 1L))

  if (mi$modelSpace == "wtp") {
    # Classify each gradient slot. xcol == 1 is the scale (lambda, the ones
    # column of the augmented design): slot 1 is the lambda mean, any other is
    # a lambda sd / off-diagonal. xcol >= 2 are the WTP (omega) coefficients.
    nPars <- mi$n$pars
    useQ <- integer(nPars); facIdx <- integer(nPars)
    mulLambda <- integer(nPars); xcolX <- integer(nPars)
    # Every lambda slot (mean, sd, off-diagonals) uses fac of the scale
    # parameter: d(lambda)/d(raw) per draw, which is 1 for a normal or fixed
    # scale, lambda for log-normal, and the censoring indicator 1{raw > 0}
    # for censored-normal. This is the chain-rule factor for the mean slot
    # too (each sd/off-diagonal slot additionally picks up its draw column
    # via dcol).
    isLambda <- spec$xcol == 1L
    useQ[isLambda] <- 1L
    facIdx[isLambda] <- 0L
    omega <- !isLambda
    facIdx[omega] <- spec$xcol[omega] - 1L   # 0-based beta index of the gamma
    xcolX[omega] <- spec$xcol[omega] - 2L    # 0-based X column of the gamma
    mulLambda[omega] <- 1L
    lambdaRandom <- as.integer(1L %in% rIDs)
    return(c(common, list(
      X = d$X, price = as.numeric(d$scalePar), chol = chol,
      useQ = useQ, facIdx = facIdx, mulLambda = mulLambda,
      xcolX = xcolX, dcol = dcolDraw, lambdaRandom = lambdaRandom)))
  }

  # Preference space: (xcol, dcol) partial spec, 0-based
  xcol <- as.integer(spec$xcol - 1L)
  c(common, list(X = d$X, chol = chol, xcol = xcol, dcol = dcolDraw))
}

mxlNegLLAndGradLL_cpp <- function(pars, mi) {
  a <- cppPrep(pars, mi)
  if (mi$modelSpace == "wtp") {
    mxl_negll_grad_wtp_cpp(
      a$X, a$price, a$draws, a$mean, a$chol, a$dist,
      a$useQ, a$facIdx, a$mulLambda, a$xcolX, a$dcol, a$lambdaRandom,
      a$obsID, a$panelID, a$weights, a$nObs, a$nPanel, a$nPars, a$numThreads)
  } else {
    mxl_negll_grad_pref_cpp(
      a$X, a$draws, a$mean, a$chol, a$dist, a$xcol, a$dcol,
      a$obsID, a$panelID, a$weights, a$nObs, a$nPanel, a$nPars, a$numThreads)
  }
}

getMxlNegLL_cpp <- function(pars, mi) mxlNegLLAndGradLL_cpp(pars, mi)$objective
getMxlNegGradLL_cpp <- function(pars, mi) mxlNegLLAndGradLL_cpp(pars, mi)$gradient

# evalFuncs for the cpp backend. MNL is already fast, so it uses the cpu path;
# only MXL is routed through the compiled kernel.
setEvalFunctions_cpp <- function(modelType, useAnalyticGrad) {
  if (modelType == "mnl") return(setEvalFunctions_cpu("mnl", useAnalyticGrad))
  if (useAnalyticGrad) {
    return(list(
      objective = mxlNegLLAndGradLL_cpp,
      negLL     = getMxlNegLL_cpp,
      negGradLL = getMxlNegGradLL_cpp,
      hessLL    = getNumericHessLL
    ))
  }
  list(
    objective = negLLAndNumericGradLL,
    negLL     = getMxlNegLL_cpp,
    negGradLL = getNumericNegGradLL,
    hessLL    = getNumericHessLL
  )
}
