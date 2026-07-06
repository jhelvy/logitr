# ============================================================================
# Large-draw demonstration: per-evaluation cost of the MXL simulated
# log-likelihood + gradient as the number of draws grows, comparing the native
# R "cpu" backend (memory-safe streaming, but slow) against the compiled "cpp"
# backend (single-threaded and multithreaded).
#
# The per-eval time is what drives total fit time (the optimizer calls it many
# times), so it isolates the backend speedup without waiting for full fits at
# 10,000 draws.
#
# NOTE: build the cpp backend with a real -O2 install before timing, NOT
# devtools::load_all (which compiles -O0). E.g.:
#   R CMD INSTALL --library=/tmp/lgr .
#   Rscript --vanilla -e '.libPaths("/tmp/lgr"); source("bench/large_draws_demo.R")'
# ============================================================================

library(logitr)

# Reach the internal modelInputs builder + eval functions
gmi <- get("getModelInputs", envir = asNamespace("logitr"))

build <- function(backend, numDraws, numThreads = 1) {
  gmi(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n", price = "n"),
    scalePar = NULL, randScale = NULL, weights = NULL, clusterID = NULL,
    robust = FALSE, startValBounds = c(-1, 1), startVals = NULL,
    numMultiStarts = 1, useAnalyticGrad = TRUE, scaleInputs = TRUE,
    standardDraws = NULL, drawType = "sobol", numDraws = numDraws, numCores = 1,
    vcov = FALSE, predict = FALSE, correlation = FALSE, call = NULL,
    options = list(print_level = 0, xtol_rel = 1e-6, xtol_abs = 1e-6,
                   ftol_rel = 1e-6, ftol_abs = 1e-6, maxeval = 1000,
                   algorithm = "NLOPT_LD_LBFGS"),
    backend = backend, numThreads = numThreads)
}

time_eval <- function(mi, reps = 5L) {
  p <- stats::setNames(rep(0.1, length(mi$parNames$all)), mi$parNames$all)
  mi$evalFuncs$objective(p, mi)  # warm up
  ts <- vapply(seq_len(reps),
               function(i) system.time(mi$evalFuncs$objective(p, mi))[["elapsed"]],
               numeric(1))
  stats::median(ts)
}

nThreads <- max(1, parallel::detectCores() - 1)
cat(sprintf("Using %d threads for the cpp multithreaded column.\n\n", nThreads))
cat(sprintf("%8s | %10s %10s %12s | %8s %8s\n",
            "numDraws", "cpu (s)", "cpp1 (s)", sprintf("cpp%d (s)", nThreads),
            "cpp1 vs", "cppN vs"))
cat(strrep("-", 66), "\n")

for (nd in c(100, 500, 2000, 10000)) {
  t_cpu  <- suppressMessages(time_eval(build("cpu", nd)))
  t_cpp1 <- suppressMessages(time_eval(build("cpp", nd, 1)))
  t_cppN <- suppressMessages(time_eval(build("cpp", nd, nThreads)))
  cat(sprintf("%8d | %10.4f %10.4f %12.4f | %6.1fx %7.1fx\n",
              nd, t_cpu, t_cpp1, t_cppN, t_cpu / t_cpp1, t_cpu / t_cppN))
}
cat("\n(cpu = streamed R path; cpp1 = compiled 1 thread; cppN = compiled N threads)\n")
cat("Per-eval time scales ~linearly with draws, so these ratios carry over to\n")
cat("total fit time. At 10,000 draws the cpu path is slow; cpp+threads is fast.\n")
