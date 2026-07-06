# ============================================================================
# Synthetic choice-data generator for the acceleration benchmark harness
#
# Produces logitr-format long data with controllable size knobs so we can
# independently scale the things that drive MXL cost:
#   - nResp  : number of respondents (panel units)
#   - nTask  : choice tasks per respondent  (nResp * nTask = N choice sets)
#   - nAlt   : alternatives per task (J)
#   - nFixed : number of fixed parameters
#   - nRand  : number of random (normal) parameters
#
# rowX in the differenced design = N * (J - 1), which together with numDraws
# sets the size of the (rowX x numDraws) matrices that dominate MXL cost.
# ============================================================================

make_synth_data <- function(
  nResp  = 100,
  nTask  = 8,
  nAlt   = 3,
  nFixed = 2,
  nRand  = 3,
  seed   = 42
) {
  set.seed(seed)
  N     <- nResp * nTask          # total choice sets
  nRows <- N * nAlt               # total long rows
  K     <- nFixed + nRand

  # Attribute columns: continuous attributes drawn iid
  X <- matrix(stats::rnorm(nRows * K), nrow = nRows, ncol = K)
  colnames(X) <- paste0("x", seq_len(K))

  # "True" tastes: fixed betas constant, random betas vary by respondent
  beta_fixed <- stats::runif(nFixed, -1, 1)
  resp_means <- stats::runif(nRand, -1, 1)
  resp_sds   <- stats::runif(nRand, 0.2, 1)

  respID <- rep(seq_len(nResp), each = nTask * nAlt)
  beta_rand_resp <- sapply(seq_len(nRand), function(k) {
    stats::rnorm(nResp, resp_means[k], resp_sds[k])[respID / (nTask * nAlt)]
  })
  # Build per-row random betas by expanding respondent draws over their rows
  beta_rand_by_row <- matrix(0, nrow = nRows, ncol = nRand)
  draws_resp <- matrix(
    stats::rnorm(nResp * nRand), nrow = nResp
  ) * rep(resp_sds, each = nResp) + rep(resp_means, each = nResp)
  for (k in seq_len(nRand)) {
    beta_rand_by_row[, k] <- draws_resp[respID, k]
  }

  # Utility = Xf %*% beta_fixed + Xr * beta_rand (row-wise) + gumbel noise
  V <- as.vector(X[, seq_len(nFixed), drop = FALSE] %*% beta_fixed)
  if (nRand > 0) {
    Xr <- X[, (nFixed + 1):K, drop = FALSE]
    V <- V + rowSums(Xr * beta_rand_by_row)
  }
  eps <- -log(-log(stats::runif(nRows)))   # standard Gumbel
  U   <- V + eps

  obsID <- rep(seq_len(N), each = nAlt)
  panelID <- rep(seq_len(nResp), each = nTask * nAlt)

  # Choose the max-utility alt within each task
  choice <- integer(nRows)
  split_idx <- split(seq_len(nRows), obsID)
  for (idx in split_idx) {
    choice[idx[which.max(U[idx])]] <- 1L
  }

  data.frame(
    obsID   = obsID,
    panelID = panelID,
    altID   = rep(seq_len(nAlt), times = N),
    choice  = choice,
    X,
    check.names = FALSE
  )
}
