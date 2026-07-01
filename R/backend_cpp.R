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

# Which models the cpp backend can currently handle
cppSupported <- function(modelSpace, correlation) {
  if (isTRUE(correlation) && modelSpace == "wtp") {
    stop('The "cpp" backend does not yet support correlated WTP-space models. ',
         'Use backend = "cpu" for correlated WTP-space models.')
  }
  invisible(TRUE)
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

  common <- list(
    draws = mi$standardDraws, mean = as.numeric(mean), dist = dist,
    obsID = as.integer(d$obsID), panelID = panelID,
    weights = as.numeric(d$weights),
    nObs = as.integer(mi$n$obs), nPanel = nPanel, nPars = as.integer(mi$n$pars)
  )

  if (mi$modelSpace == "wtp") {
    # sd per variable (0 for fixed) and the 0-based gradient index of each
    # variable's sd slot (-1 if fixed), in random-parameter order.
    sdFull <- numeric(nVars); if (length(rIDs) > 0) sdFull[rIDs] <- pars_sd
    sdPos <- rep(-1L, nVars)
    if (length(rIDs) > 0) sdPos[rIDs] <- nVars + seq_along(rIDs) - 1L
    return(c(common, list(
      X = d$X, price = as.numeric(d$scalePar), sdFull = sdFull, sdPos = sdPos)))
  }

  # Preference space: covariance factor (diagonal of sds, or lower-triangular
  # Cholesky when correlated), plus the (xcol, dcol) partial spec.
  chol <- matrix(0, nVars, nVars)
  if (isTRUE(mi$inputs$correlation)) {
    nR <- length(rIDs)
    L <- matrix(0, nR, nR)
    L[lower.tri(L, diag = TRUE)] <- pars_sd
    chol[rIDs, rIDs] <- L
  } else {
    sdFull <- numeric(nVars); if (length(rIDs) > 0) sdFull[rIDs] <- pars_sd
    diag(chol) <- sdFull
  }
  spec <- buildPartialSpec(mi)
  xcol <- as.integer(spec$xcol - 1L)
  dcol <- as.integer(ifelse(spec$dcol == 0L, -1L, spec$dcol - 1L))
  c(common, list(X = d$X, chol = chol, xcol = xcol, dcol = dcol))
}

mxlNegLLAndGradLL_cpp <- function(pars, mi) {
  a <- cppPrep(pars, mi)
  if (mi$modelSpace == "wtp") {
    mxl_negll_grad_wtp_cpp(
      a$X, a$price, a$draws, a$mean, a$sdFull, a$dist, a$sdPos,
      a$obsID, a$panelID, a$weights, a$nObs, a$nPanel, a$nPars)
  } else {
    mxl_negll_grad_pref_cpp(
      a$X, a$draws, a$mean, a$chol, a$dist, a$xcol, a$dcol,
      a$obsID, a$panelID, a$weights, a$nObs, a$nPanel, a$nPars)
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
