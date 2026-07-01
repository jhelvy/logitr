# ============================================================================
# Phase 3 spike: does a compiled kernel meaningfully beat logitr's R path on
# the hot MXL negLL + gradient? Worst case = all parameters random normal
# (maximizes the 2K-partial gradient loop). Validates parity, then benchmarks.
#
#   Rscript bench/phase3_spike.R           # default size
#   Rscript bench/phase3_spike.R 20000 8 500   # rowX~ nObs*(J-1), K, R
# ============================================================================

suppressMessages(devtools::load_all(".", quiet = TRUE))
Rcpp::sourceCpp("bench/phase3_spike.cpp")
source("bench/synth_data.R")

args  <- commandArgs(trailingOnly = TRUE)
nObs  <- if (length(args) >= 1) as.integer(args[1]) else 6000L  # choice sets
K     <- if (length(args) >= 2) as.integer(args[2]) else 6L     # all random
R     <- if (length(args) >= 3) as.integer(args[3]) else 500L   # draws
nAlt  <- 4L

data <- make_synth_data(nResp = nObs, nTask = 1, nAlt = nAlt,
                        nFixed = 0, nRand = K, seed = 1)
pars_names <- paste0("x", seq_len(K))

mi <- suppressMessages(getModelInputs(
  data = data, outcome = "choice", obsID = "obsID",
  pars = pars_names, randPars = stats::setNames(rep("n", K), pars_names),
  scalePar = NULL, randScale = NULL, weights = NULL, panelID = NULL,
  clusterID = NULL, robust = FALSE, startValBounds = c(-1, 1), startVals = NULL,
  numMultiStarts = 1, useAnalyticGrad = TRUE, scaleInputs = TRUE,
  standardDraws = NULL, drawType = "halton", numDraws = R, numCores = 1,
  vcov = FALSE, predict = FALSE, correlation = FALSE, call = NULL,
  options = list(print_level = 0, xtol_rel = 1e-6, xtol_abs = 1e-6,
                 ftol_rel = 1e-6, ftol_abs = 1e-6, maxeval = 1000,
                 algorithm = "NLOPT_LD_LBFGS")))

d <- mi$data_diff
pars <- stats::setNames(rep(0.1, length(mi$parNames$all)), mi$parNames$all)

# Inputs for the C++ kernel (raw arrays straight off the modelInputs)
cpp_args <- list(
  X = d$X, draws = mi$standardDraws, pars = as.numeric(pars),
  obsID = as.integer(d$obsID), weights = as.numeric(d$weights),
  nObs = length(d$weights))

# ---- Parity ----------------------------------------------------------------
ref <- mi$evalFuncs$objective(pars, mi)                 # logitr R path
cpp <- do.call(mxl_negll_grad_cpp, cpp_args)
cat(sprintf("Size: nObs=%d rowX=%d K=%d R=%d\n", cpp_args$nObs, nrow(d$X), K, R))
cat(sprintf("Parity: d_objective=%.2e  d_gradient=%.2e\n",
            abs(ref$objective - cpp$objective),
            max(abs(ref$gradient - cpp$gradient))))

# ---- Benchmark (median elapsed of several evals) ---------------------------
timeit <- function(fn, reps = 7L) {
  ts <- vapply(seq_len(reps), function(i) system.time(fn())[["elapsed"]], numeric(1))
  stats::median(ts)
}
t_R   <- timeit(function() mi$evalFuncs$objective(pars, mi))
t_cpp <- timeit(function() do.call(mxl_negll_grad_cpp, cpp_args))
cat(sprintf("\nR path (stored partials): %.4f s/eval\n", t_R))
cat(sprintf("C++ fused (single-thread): %.4f s/eval\n", t_cpp))
cat(sprintf("Speedup: %.1fx\n", t_R / t_cpp))
cat(sprintf("Stored partials memory: %.0f MB   |   C++ peak ~ rowX+nObs vectors\n",
            as.numeric(object.size(mi$partials)) / 1024^2))
