# ============================================================================
# Phase 4 spike: within the many-multistarts regime, is it faster to run many
# single-threaded models at once, or fewer multi-threaded models at once --
# and does the answer change as the number of draws grows?
#
# Simulates a K-iteration multistart and completes all K fits under different
# (workers x threads-per-worker) splits that each use ~all cores, at several
# draw counts. Uses a PSOCK cluster (separate processes, TBB-safe) so the
# nesting is faithful to how a mirai-based multistart would behave.
#
# IMPORTANT: run against a real -O2 install of the dev package, e.g.:
#   rm -f src/*.o src/*.so
#   R CMD INSTALL --library=/tmp/lgr_ps .
#   Rscript bench/parallel_spike.R /tmp/lgr_ps
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
libpath <- if (length(args) >= 1) args[1] else .libPaths()[1]
library(logitr, lib.loc = libpath)

numCores  <- parallel::detectCores()
K         <- 20                  # number of multistart iterations to complete
drawGrid  <- c(500, 1000, 2000)  # does the ranking hold as draws grow?

# All splits with workers * threads ~= numCores
splits <- list(
  c(workers = numCores,        threads = 1),
  c(workers = numCores %/% 2,  threads = 2),
  c(workers = numCores %/% 5,  threads = 5),
  c(workers = 1,               threads = numCores)
)

run_split <- function(workers, threads, numDraws) {
  workers <- as.integer(unname(workers)); threads <- as.integer(unname(threads))
  fit_args <- list(
    data = logitr::yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"),
    randPars = c(price = "n", feat = "n", brand = "n"),
    numDraws = numDraws, numMultiStarts = 1, backend = "cpp")
  cl <- parallel::makeCluster(workers, type = "PSOCK")
  on.exit(parallel::stopCluster(cl))
  parallel::clusterExport(cl, c("libpath", "fit_args", "threads"),
                          envir = environment())
  parallel::clusterEvalQ(cl, { library(logitr, lib.loc = libpath) })
  fit_one <- function(i) {
    m <- suppressMessages(suppressWarnings(do.call(
      logitr::logitr, c(fit_args, numThreads = threads, numCores = 1))))
    as.numeric(stats::logLik(m))
  }
  t <- system.time(res <- parallel::parLapplyLB(cl, seq_len(K), fit_one))[["elapsed"]]
  list(time = t, ll = unlist(res)[1])
}

run_draws <- function(numDraws) {
  cat(sprintf("\n=== numDraws = %d (cores = %d, K = %d fits) ===\n",
              numDraws, numCores, K))
  cat(sprintf("%8s %8s | %10s %14s %10s\n",
              "workers", "threads", "total (s)", "sec/fit", "vs best"))
  cat(strrep("-", 56), "\n")
  results <- lapply(splits, function(s) run_split(s["workers"], s["threads"], numDraws))
  times <- vapply(results, function(r) r$time, numeric(1))
  best <- min(times)
  for (i in seq_along(splits)) {
    s <- splits[[i]]
    cat(sprintf("%8d %8d | %10.2f %14.3f %9.2fx\n",
                s["workers"], s["threads"], times[i], times[i] / K, times[i] / best))
  }
  cat(sprintf("logLik sanity: %.4f\n", results[[1]]$ll))
}

for (nd in drawGrid) run_draws(nd)
cat("\nLower total time = better. Watch whether (cores,1) stays best as draws\n")
cat("grow: if a batched split overtakes it at high draws, memory-bandwidth\n")
cat("contention from many concurrent models is the cause.\n")
