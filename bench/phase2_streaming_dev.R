# ============================================================================
# Phase 2 dev: streaming (draw-batched) MXL log-likelihood + gradient.
#
# Validates that a single streaming pass over draw-batches reproduces the
# current (stored-partials) analytic LL + gradient, for pref/WTP x panel x
# ln/cn x correlation. Correctness first; the memoryless outer-product slicing
# is validated separately (partials are rank-1: partial_i = outer(Xcol, drawcol)).
# ============================================================================

suppressMessages(devtools::load_all(".", quiet = TRUE))
ns <- asNamespace("logitr")
lf <- function(name) get(name, envir = ns)

# ---- Streaming objective: mirrors mxlNegLLAndGradLL but sums over draw-batches
mxl_batched <- function(pars, mi, batchSize = NULL) {
  d <- mi$data_diff
  R <- mi$n$draws
  if (is.null(batchSize) || batchSize >= R) batchSize <- R
  batchList <- split(seq_len(R), ceiling(seq_len(R) / batchSize))
  panel <- mi$panel
  space <- mi$modelSpace

  spec  <- build_partial_spec(mi)
  Xaug  <- if (space == "wtp") cbind(1, d$X) else d$X
  nUnit <- if (panel) length(unique(d$panelID)) else length(d$weights)
  sumP  <- numeric(nUnit)              # accumulates sum_d of (panel prod | logit)
  A     <- matrix(0, nUnit, mi$n$pars) # accumulates gradient numerator

  n_b <- mi$n
  for (cols in batchList) {
    Rb <- length(cols)
    n_b$draws <- Rb
    sd_b <- mi$standardDraws[cols, , drop = FALSE]
    betaDraws <- lf("makeBetaDraws")(pars, mi$parIDs, n_b, sd_b, mi$inputs$correlation)
    VDraws  <- mi$logitFuncs$getMxlV(betaDraws, d$X, d$scalePar, n_b)
    expV    <- exp(VDraws)
    logit   <- lf("getLogit")(expV, d$obsID)          # nObs x Rb

    if (panel) {
      LP <- exp(rowsum(log(logit), d$panelID))         # nPanel x Rb
      sumP <- sumP + rowSums(LP)
    } else {
      sumP <- sumP + rowSums(logit)
    }

    # Form this batch's partials from constants (memoryless), then adjust
    part_b <- form_partials(spec, Xaug, sd_b)
    part_b <- adjust_partials(space, part_b, betaDraws, VDraws, mi$parIDs,
                              n_b, mi$inputs$randScale)

    if (panel) {
      grads_b <- lapply(part_b, function(x)
        rowSums(LP * rowsum(logit * rowsum(x * expV, group = d$obsID, reorder = FALSE),
                            group = d$panelID)))
    } else {
      grads_b <- lapply(part_b, function(x)
        rowSums(logit^2 * rowsum(x * expV, group = d$obsID, reorder = FALSE)))
    }
    A <- A + matrix(unlist(grads_b), ncol = length(part_b))
  }

  pHat <- sumP / R
  objective <- -sum(d$weights * log(pHat))
  gradient  <- as.vector(t(A) %*% (d$weights / (pHat * R)))
  list(objective = objective, gradient = gradient)
}

# ---- Compact partial spec: each partial i is outer(X_aug[,xcol], drawcol),
#      where dcol==0 means the ones-vector (mean-parameter partials).
build_partial_spec <- function(mi) {
  nV <- mi$n$vars
  rr <- mi$parIDs$r
  xcol <- integer(mi$n$pars)
  dcol <- integer(mi$n$pars)
  xcol[seq_len(nV)] <- seq_len(nV)          # mean pars
  dcol[seq_len(nV)] <- 0L
  if (mi$inputs$correlation) {
    xcol[mi$parIDs$sdDiag] <- rr
    dcol[mi$parIDs$sdDiag] <- rr
    combos <- utils::combn(seq_along(rr), 2)
    for (i in seq_len(ncol(combos))) {
      pos <- mi$parIDs$sdOffDiag[i]
      xcol[pos] <- rr[combos[1, i]]
      dcol[pos] <- rr[combos[2, i]]
    }
  } else {
    xcol[nV + seq_along(rr)] <- rr
    dcol[nV + seq_along(rr)] <- rr
  }
  list(xcol = xcol, dcol = dcol)
}

# Form the batch slice of every partial from constants (no full matrix stored)
form_partials <- function(spec, Xaug, sd_b) {
  ones <- rep(1, nrow(sd_b))
  lapply(seq_along(spec$xcol), function(i) {
    dc <- spec$dcol[i]
    outer(Xaug[, spec$xcol[i]], if (dc == 0L) ones else sd_b[, dc])
  })
}

# ---- Per-batch partial adjustment (factored from mxlNegGradLL_pref/_wtp)
adjust_partials <- function(space, partials, betaDraws, VDraws, parIDs, n, randScale) {
  partials <- lf("updatePartials")(partials, parIDs, betaDraws, n)
  if (space == "wtp") {
    lambdaDraws <- lf("repmat")(matrix(betaDraws[, 1], nrow = 1), n$rowX, 1)
    partial_lambda_mean <- VDraws / lambdaDraws
    partials[[1]] <- partial_lambda_mean
    if (!is.null(randScale)) {
      lambda_sdID <- parIDs$lambdaIDs[2]
      partials[[lambda_sdID]] <- partials[[lambda_sdID]] * partial_lambda_mean
      if (length(parIDs$ln) > 0) if (parIDs$ln[1] == 1)
        partials[[1]] <- partials[[1]] * lambdaDraws
    }
    for (id in parIDs$omegaIDs) partials[[id]] <- partials[[id]] * lambdaDraws
    if (!is.null(randScale)) for (id in parIDs$lambdaOffDiag)
      partials[[id]] <- partials[[id]] / lambdaDraws * partial_lambda_mean
  }
  partials
}

# ---------------------------------------------------------------------------
# Validation harness
# ---------------------------------------------------------------------------
gmi <- function(panelID = NULL, correlation = FALSE, ...) getModelInputs(
  data = yogurt, outcome = "choice",
  obsID = "obsID", panelID = panelID, correlation = correlation,
  randScale = NULL, weights = NULL, clusterID = NULL, robust = FALSE,
  startValBounds = c(-1, 1), startVals = NULL, numMultiStarts = 1, useAnalyticGrad = TRUE,
  scaleInputs = TRUE, standardDraws = NULL, drawType = "halton", numDraws = 60, numCores = 1,
  vcov = FALSE, predict = FALSE, call = NULL,
  options = list(print_level = 0, xtol_rel = 1e-6, xtol_abs = 1e-6, ftol_rel = 1e-6,
                 ftol_abs = 1e-6, maxeval = 1000, algorithm = "NLOPT_LD_LBFGS"), ...)

check <- function(label, mi) {
  pars <- stats::setNames(rep(0.2, length(mi$parNames$all)), mi$parNames$all)
  # (a) memoryless partial spec reconstructs the stored partials exactly
  spec <- build_partial_spec(mi)
  Xaug <- if (mi$modelSpace == "wtp") cbind(1, mi$data_diff$X) else mi$data_diff$X
  recon <- form_partials(spec, Xaug, mi$standardDraws)
  d_part <- max(mapply(function(a, b) max(abs(a - b)), mi$partials, recon))
  # (b) streaming LL+grad parity vs current path
  ref  <- mi$evalFuncs$objective(pars, mi)               # current path
  one  <- mxl_batched(pars, mi, batchSize = NULL)         # 1 batch (all draws)
  many <- mxl_batched(pars, mi, batchSize = 7)            # streamed in chunks of 7
  d_grd1 <- max(abs(ref$gradient - one$gradient))
  d_objM <- abs(ref$objective - many$objective)
  d_grdM <- max(abs(ref$gradient - many$gradient))
  cat(sprintf("%-28s spec %.0e | 1-batch grad %.2e | streamed obj %.2e grad %.2e\n",
              label, d_part, d_grd1, d_objM, d_grdM))
}

suppressMessages({
  check("pref non-panel",
        gmi(pars = c("price","feat","brand"), randPars = c(feat="n"), scalePar = NULL))
  check("pref panel",
        gmi(pars = c("price","feat","brand"), panelID = "id", randPars = c(feat="n"), scalePar = NULL))
  check("pref panel + ln",
        gmi(pars = c("price","feat","brand"), panelID = "id", randPars = c(feat="ln"), scalePar = NULL))
  check("pref panel + cn",
        gmi(pars = c("price","feat","brand"), panelID = "id", randPars = c(feat="cn"), scalePar = NULL))
  check("pref panel + correlation",
        gmi(pars = c("price","feat","brand"), panelID = "id",
            randPars = c(feat="n", brand="n"), scalePar = NULL, correlation = TRUE))
  check("wtp panel",
        gmi(pars = c("feat","brand"), panelID = "id", randPars = c(feat="n"), scalePar = "price"))
})
cat("\n(all diffs should be ~1e-10 or smaller)\n")
