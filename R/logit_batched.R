# ============================================================================
# Draw-batched (streaming) MXL log-likelihood and gradient
#
# The simulated log-likelihood and its analytic gradient are sums/means over
# the draw dimension, so they decompose exactly into a single streaming pass
# over batches of draws. This keeps peak memory bounded by the batch size
# instead of the full number of draws, which is what makes large draw counts
# (e.g. numDraws = 10000) feasible.
#
# Key fact used here: every stored "partial" matrix is rank-1, equal to the
# outer product of one column of the (augmented) design matrix and one column
# of the standard draws (or the ones-vector for mean parameters). So the only
# constants needed are X and standardDraws (already stored on modelInputs); the
# rowX x batch slice of each partial is formed on the fly, never materialized
# at full width. The math is identical to the non-streaming path.
# ============================================================================

# Compact spec mapping each partial to (X column, draw column). A draw column
# of 0 denotes the ones-vector, i.e. a mean-parameter partial.
buildPartialSpec <- function(mi) {
  nV <- mi$n$vars
  rr <- mi$parIDs$r
  nPars <- mi$n$pars
  xcol <- integer(nPars)
  dcol <- integer(nPars)
  xcol[seq_len(nV)] <- seq_len(nV) # mean parameters -> X column j, ones draws
  if (mi$inputs$correlation) {
    xcol[mi$parIDs$sdDiag] <- rr
    dcol[mi$parIDs$sdDiag] <- rr
    if (length(rr) > 1) {
      combos <- utils::combn(seq_along(rr), 2)
      for (i in seq_len(ncol(combos))) {
        pos <- mi$parIDs$sdOffDiag[i]
        xcol[pos] <- rr[combos[1, i]]
        dcol[pos] <- rr[combos[2, i]]
      }
    }
  } else {
    xcol[nV + seq_along(rr)] <- rr
    dcol[nV + seq_along(rr)] <- rr
  }
  list(xcol = xcol, dcol = dcol)
}

# Split 1:numDraws into contiguous batches of size batchSize
getDrawBatches <- function(numDraws, batchSize) {
  if (is.null(batchSize) || batchSize >= numDraws) return(list(seq_len(numDraws)))
  split(seq_len(numDraws), ceiling(seq_len(numDraws) / batchSize))
}

# Form the batch slice of every partial from constants (memoryless)
formPartials <- function(spec, Xaug, standardDraws_batch) {
  ones <- rep(1, nrow(standardDraws_batch))
  lapply(seq_along(spec$xcol), function(i) {
    dc <- spec$dcol[i]
    outer(Xaug[, spec$xcol[i]], if (dc == 0L) ones else standardDraws_batch[, dc])
  })
}

# Adjust a batch's partials for log-normal / censored-normal distributions and,
# in WTP space, for the scale (lambda) and omega parameters. Mirrors
# updatePartials() + the adjustments in mxlNegGradLL_wtp(), applied per batch.
adjustPartialsBatch <- function(mi, partials, betaDraws, VDraws, n_b) {
  parIDs <- mi$parIDs
  partials <- updatePartials(partials, parIDs, betaDraws, n_b)
  if (mi$modelSpace == "wtp") {
    randScale <- mi$inputs$randScale
    lambdaDraws <- repmat(matrix(betaDraws[, 1], nrow = 1), n_b$rowX, 1)
    partial_lambda_mean <- VDraws / lambdaDraws
    # 0 / 0 = NaN where a censored-normal lambda is zero; the correct partial
    # there is q * 1{raw > 0} = 0 (mirrors mxlNegGradLL_wtp)
    partial_lambda_mean[!is.finite(partial_lambda_mean)] <- 0
    partials[[1]] <- partial_lambda_mean
    if (!is.null(randScale)) {
      lambda_sdID <- parIDs$lambdaIDs[2]
      partials[[lambda_sdID]] <- partials[[lambda_sdID]] * partial_lambda_mean
      if (length(parIDs$ln) > 0) {
        if (parIDs$ln[1] == 1) partials[[1]] <- partials[[1]] * lambdaDraws
      }
    }
    for (id in parIDs$omegaIDs) partials[[id]] <- partials[[id]] * lambdaDraws
    if (!is.null(randScale)) {
      for (id in parIDs$lambdaOffDiag) {
        partials[[id]] <- partials[[id]] / lambdaDraws * partial_lambda_mean
        partials[[id]][!is.finite(partials[[id]])] <- 0
      }
    }
  }
  partials
}

# Core streaming pass. Returns the negative LL and (optionally) its gradient.
streamMxl <- function(pars, mi, gradient = TRUE) {
  d <- mi$data_diff
  R <- mi$n$draws
  panel <- mi$panel
  batches <- getDrawBatches(R, mi$batchPlan$batchSize)
  spec <- mi$partialSpec
  Xaug <- if (mi$modelSpace == "wtp") cbind(1, d$X) else d$X

  nUnit <- if (panel) length(unique(d$panelID)) else length(d$weights)
  sumP <- numeric(nUnit)                         # accumulates sum over draws
  A <- if (gradient) matrix(0, nUnit, mi$n$pars) else NULL

  n_b <- mi$n
  for (cols in batches) {
    n_b$draws <- length(cols)
    sd_b <- mi$standardDraws[cols, , drop = FALSE]
    betaDraws <- makeBetaDraws(pars, mi$parIDs, n_b, sd_b, mi$inputs$correlation)
    VDraws <- mi$logitFuncs$getMxlV(betaDraws, d$X, d$scalePar, n_b)
    expV <- exp(VDraws)
    logit <- getLogit(expV, d$obsID)             # nObs x batch
    if (panel) {
      logitPanel <- exp(rowsum(log(logit), d$panelID))
      sumP <- sumP + rowSums(logitPanel)
    } else {
      sumP <- sumP + rowSums(logit)
    }
    if (gradient) {
      part_b <- formPartials(spec, Xaug, sd_b)
      part_b <- adjustPartialsBatch(mi, part_b, betaDraws, VDraws, n_b)
      if (panel) {
        grads_b <- lapply(part_b, function(x) {
          rowSums(logitPanel * rowsum(
            logit * rowsum(x * expV, group = d$obsID, reorder = FALSE),
            group = d$panelID))
        })
      } else {
        grads_b <- lapply(part_b, function(x) {
          rowSums(logit^2 * rowsum(x * expV, group = d$obsID, reorder = FALSE))
        })
      }
      A <- A + matrix(unlist(grads_b), ncol = mi$n$pars)
    }
  }

  pHat <- sumP / R
  objective <- negLL(pHat, d$weights)
  grad <- if (gradient) as.vector(t(A) %*% (d$weights / (pHat * R))) else NULL
  list(objective = objective, gradient = grad)
}

# ---- evalFuncs wrappers (same interface as the non-streaming MXL path) ----

mxlNegLLAndGradLL_batched <- function(pars, mi) {
  streamMxl(pars, mi, gradient = TRUE)
}

getMxlNegLL_batched <- function(pars, mi) {
  streamMxl(pars, mi, gradient = FALSE)$objective
}

getMxlNegGradLL_batched <- function(pars, mi) {
  streamMxl(pars, mi, gradient = TRUE)$gradient
}

# Assembles the batched evalFuncs list, honoring useAnalyticGrad
setEvalFunctions_batched <- function(useAnalyticGrad) {
  if (useAnalyticGrad) {
    return(list(
      objective = mxlNegLLAndGradLL_batched,
      negLL     = getMxlNegLL_batched,
      negGradLL = getMxlNegGradLL_batched,
      hessLL    = getNumericHessLL
    ))
  }
  list(
    objective = negLLAndNumericGradLL,
    negLL     = getMxlNegLL_batched,
    negGradLL = getNumericNegGradLL,
    hessLL    = getNumericHessLL
  )
}

# Decide whether to stream and at what batch size. numDrawsBatch = NULL means
# auto: keep the (faster) stored-partials path when the partials fit a memory
# budget, otherwise stream with a batch size that keeps peak memory in budget.
getBatchPlan <- function(numDrawsBatch, n, maxPartialsBytes = 1e9) {
  numDraws <- n$draws
  if (!is.null(numDrawsBatch)) {
    stream <- numDrawsBatch < numDraws
    return(list(stream = stream,
                batchSize = if (stream) as.integer(numDrawsBatch) else numDraws))
  }
  perDrawBytes <- n$pars * n$rowX * 8
  if (perDrawBytes * numDraws <= maxPartialsBytes) {
    return(list(stream = FALSE, batchSize = numDraws))
  }
  batchSize <- max(1L, as.integer(floor(maxPartialsBytes / perDrawBytes)))
  list(stream = TRUE, batchSize = min(batchSize, numDraws))
}

# Inform the user when draw-batched streaming is engaged, since it is a
# different (memory-bounded) evaluation path than the default.
notifyStreaming <- function(batchPlan, n) {
  message(
    "Estimating with draw-batched streaming (numDraws = ", n$draws,
    ", batch size = ", batchPlan$batchSize, ").\n",
    "  This keeps memory bounded for large draw counts. Set 'numDrawsBatch' ",
    "to control the\n  batch size, or to a value >= numDraws to disable ",
    "streaming."
  )
}
