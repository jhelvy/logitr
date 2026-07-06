# ============================================================================
# Phase 0 acceleration benchmark + profiling harness for logitr MXL estimation
#
# Goals
#   1. Establish the CPU baseline for end-to-end MXL estimation across a sweep
#      of the knobs that drive cost (numDraws, rowX, nRand, correlation, panel).
#   2. Attribute the per-iteration cost of the simulated log-likelihood +
#      analytic gradient to its components (betaDraws, V = X %*% t(beta),
#      exp, getLogit, panel product, gradient loop) so we know what a faster
#      backend should actually target and where the GPU crossover is.
#
# Usage:
#   Rscript bench/accel_benchmark.R            # full sweep
#   Rscript bench/accel_benchmark.R quick      # tiny sweep (smoke test)
#
# Output: bench/results/accel_benchmark_<timestamp>.csv  (+ console summary)
# ============================================================================

suppressWarnings(suppressMessages({
  # Prefer the in-development sources so we profile the working tree, not an
  # installed copy. Fall back to the installed package.
  if (requireNamespace("devtools", quietly = TRUE) &&
      file.exists("DESCRIPTION")) {
    devtools::load_all(".", quiet = TRUE)
  } else {
    library(logitr)
  }
}))
source(file.path("bench", "synth_data.R"))

ns <- asNamespace("logitr")
lf <- function(name) get(name, envir = ns)   # reach internal functions

# ---------------------------------------------------------------------------
# Build a modelInputs object the same way logitr() does, so we profile the
# real internal hot path (mi$evalFuncs$objective == mxlNegLLAndGradLL).
# ---------------------------------------------------------------------------
build_mi <- function(data, pars, randPars, panel, correlation, numDraws) {
  default_options <- list(
    print_level = 0, xtol_rel = 1e-6, xtol_abs = 1e-6,
    ftol_rel = 1e-6, ftol_abs = 1e-6, maxeval = 1000, algorithm = "NLOPT_LD_LBFGS"
  )
  lf("getModelInputs")(
    data            = data,
    outcome         = "choice",
    obsID           = "obsID",
    pars            = pars,
    randPars        = randPars,
    scalePar        = NULL,
    randScale       = NULL,
    weights         = NULL,
    panelID         = if (panel) "panelID" else NULL,
    clusterID       = NULL,
    robust          = FALSE,
    startValBounds  = c(-1, 1),
    startVals       = NULL,
    numMultiStarts  = 1,
    useAnalyticGrad = TRUE,
    scaleInputs     = TRUE,
    standardDraws   = NULL,
    drawType        = "halton",
    numDraws        = numDraws,
    numCores        = 1,
    vcov            = FALSE,
    predict         = FALSE,
    correlation     = correlation,
    call            = match.call(),
    options         = default_options
  )
}

# Median wall time (seconds) of evaluating fn() over `reps` runs
time_median <- function(fn, reps = 11L) {
  ts <- numeric(reps)
  for (i in seq_len(reps)) ts[i] <- system.time(fn())[["elapsed"]]
  stats::median(ts)
}

# ---------------------------------------------------------------------------
# Attribute a single LL+grad evaluation to its components. We re-create the
# inner steps of mxlNegLLAndGradLL() so each piece can be timed in isolation.
# ---------------------------------------------------------------------------
profile_components <- function(mi, reps = 21L) {
  pars <- mi$parNames$all
  startVals <- stats::setNames(rep(0.1, length(pars)), pars)
  d <- mi$data_diff

  # Precompute stage inputs once (mirrors mxlNegLLAndGradLL internals)
  betaDraws <- lf("makeBetaDraws")(
    startVals, mi$parIDs, mi$n, mi$standardDraws, mi$inputs$correlation)
  VDraws    <- mi$logitFuncs$getMxlV(betaDraws, d$X, d$scalePar, mi$n)
  expVDraws <- exp(VDraws)
  logitDraws <- lf("getLogit")(expVDraws, d$obsID)
  if (mi$panel) {
    logitDrawsPanel <- exp(rowsum(log(logitDraws), d$panelID))
    pHat <- rowMeans(logitDrawsPanel, na.rm = TRUE)
  } else {
    logitDrawsPanel <- logitDraws
    pHat <- rowMeans(logitDraws, na.rm = TRUE)
  }

  t_beta <- time_median(function()
    lf("makeBetaDraws")(startVals, mi$parIDs, mi$n, mi$standardDraws,
                        mi$inputs$correlation), reps)
  t_V <- time_median(function()
    mi$logitFuncs$getMxlV(betaDraws, d$X, d$scalePar, mi$n), reps)
  t_exp <- time_median(function() exp(VDraws), reps)
  t_logit <- time_median(function() lf("getLogit")(expVDraws, d$obsID), reps)
  t_panel <- time_median(function() {
    if (mi$panel) {
      lp <- exp(rowsum(log(logitDraws), d$panelID)); rowMeans(lp, na.rm = TRUE)
    } else rowMeans(logitDraws, na.rm = TRUE)
  }, reps)
  t_grad <- time_median(function()
    mi$logitFuncs$mxlNegGradLL(
      betaDraws, VDraws, expVDraws, logitDraws, logitDrawsPanel, pHat,
      mi$partials, d$obsID, d$panelID, mi$parIDs, d$weights, mi$n,
      mi$inputs$randScale, mi$panel), reps)
  t_full <- time_median(function()
    mi$evalFuncs$objective(startVals, mi), reps)

  list(
    rowX       = mi$n$rowX,
    numDraws   = mi$n$draws,
    t_beta     = t_beta,
    t_V        = t_V,
    t_exp      = t_exp,
    t_logit    = t_logit,
    t_panel    = t_panel,
    t_grad     = t_grad,
    t_full     = t_full,
    # memory footprint of the stored partials (the part that scales worst)
    partials_mb = as.numeric(utils::object.size(mi$partials)) / 1024^2
  )
}

# ---------------------------------------------------------------------------
# Sweep configuration
# ---------------------------------------------------------------------------
args <- commandArgs(trailingOnly = TRUE)
quick <- length(args) && args[1] == "quick"

if (quick) {
  grid <- expand.grid(
    nResp = 100, nTask = 8, nAlt = 3, nFixed = 2, nRand = 3,
    numDraws = c(50, 200), correlation = FALSE, panel = TRUE,
    stringsAsFactors = FALSE)
} else {
  grid <- expand.grid(
    nResp       = c(300, 1000),
    nTask       = 10,
    nAlt        = c(3, 5),
    nFixed      = 2,
    nRand       = c(3, 6),
    numDraws    = c(100, 500, 2000),
    correlation = c(FALSE, TRUE),
    panel       = TRUE,
    stringsAsFactors = FALSE
  )
}

# End-to-end fits get very slow at large draws (2000+ draws x many random
# pars can take 5-30+ min each). Above this draw count we SKIP the full fit
# and rely on the per-eval component profile + a projected iteration count,
# so the harness stays usable in the large-draw regime we most care about.
max_fit_draws <- 500L

cat(sprintf("Running %d configurations (full fits only when draws <= %d)...\n",
            nrow(grid), max_fit_draws))
results <- vector("list", nrow(grid))

# Incremental CSV so an interrupt never loses completed rows
ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
outfile <- file.path("bench", "results", sprintf("accel_benchmark_%s.csv", ts))
wrote_header <- FALSE

for (i in seq_len(nrow(grid))) {
  g <- grid[i, ]
  cat(sprintf("[%d/%d] nResp=%d nAlt=%d nRand=%d draws=%d corr=%s ... ",
              i, nrow(grid), g$nResp, g$nAlt, g$nRand, g$numDraws, g$correlation))

  data <- make_synth_data(
    nResp = g$nResp, nTask = g$nTask, nAlt = g$nAlt,
    nFixed = g$nFixed, nRand = g$nRand)
  pars <- paste0("x", seq_len(g$nFixed + g$nRand))
  randPars <- stats::setNames(rep("n", g$nRand),
                              paste0("x", (g$nFixed + 1):(g$nFixed + g$nRand)))

  comp <- tryCatch({
    mi <- build_mi(data, pars, randPars, g$panel, g$correlation, g$numDraws)
    profile_components(mi)
  }, error = function(e) { cat("MI/profile error:", conditionMessage(e), "\n"); NULL })

  # End-to-end single-start fit (what users feel) -- gated on draw count
  if (g$numDraws <= max_fit_draws) {
    t_fit <- tryCatch(
      system.time(suppressWarnings(suppressMessages(
        logitr(data, "choice", "obsID",
               pars = pars, randPars = randPars,
               panelID = if (g$panel) "panelID" else NULL,
               correlation = g$correlation, numDraws = g$numDraws,
               numMultiStarts = 1, numCores = 1)
      )))[["elapsed"]],
      error = function(e) NA_real_)
  } else {
    t_fit <- NA_real_   # skipped: too slow in the large-draw regime
  }

  if (is.null(comp)) { cat("skipped\n"); next }
  row <- cbind(g, as.data.frame(comp), t_fit_e2e = t_fit)
  results[[i]] <- row
  # Append immediately so nothing is lost on interrupt
  utils::write.table(row, outfile, sep = ",", row.names = FALSE,
                     col.names = !wrote_header, append = wrote_header)
  wrote_header <- TRUE
  cat(sprintf("full-eval=%.4fs  grad share=%.0f%%  fit=%s\n",
              comp$t_full, 100 * comp$t_grad / comp$t_full,
              if (is.na(t_fit)) "skipped" else sprintf("%.2fs", t_fit)))
}

res <- do.call(rbind, results[!vapply(results, is.null, logical(1))])

# ---------------------------------------------------------------------------
# Summarize: per-iteration component shares + memory + GPU-crossover signal
# ---------------------------------------------------------------------------
res$share_V    <- res$t_V    / res$t_full
res$share_exp  <- res$t_exp  / res$t_full
res$share_grad <- res$t_grad / res$t_full
res$matmul_elems <- res$rowX * res$numDraws   # size of the dominant matrices

cat("\n==== Component cost shares (median single LL+grad eval) ====\n")
print(round(
  res[, c("rowX", "numDraws", "matmul_elems",
          "share_V", "share_exp", "share_grad",
          "partials_mb", "t_full", "t_fit_e2e")],
  4), row.names = FALSE)
cat(sprintf("\nSaved: %s\n", outfile))
cat("\nReading guide:\n",
    "  share_grad high  -> the hand-written gradient loop dominates ->",
    "Rust/threaded-CPU backend pays off more than GPU.\n",
    "  share_V/exp high -> dense matmul+exp dominate ->",
    "GPU (torch/xlogit) pays off as matmul_elems grows.\n",
    "  partials_mb large -> stored-partials memory is the scaling wall ->",
    "batch over draws (xlogit-style) in any new backend.\n", sep = "")
