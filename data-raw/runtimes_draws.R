# ============================================================================
# Large-draw scaling benchmark: how logitr's full-fit estimation time for a
# preference-space mixed logit model scales as the number of random draws grows
# to 10,000, comparing the three logitr backends:
#
#   cpu           - native R path (memory-safe streaming, single-threaded)
#   cpp (1 core)  - compiled backend, single thread
#   cpp (N cores) - compiled backend, all cores
#
# This is a logitr-ONLY figure: the point is to show that logitr scales to draw
# counts the other packages cannot reach in tractable time, and that the
# compiled + multithreaded backend is what makes it fast. The head-to-head
# comparison against other packages lives in data-raw/runtimes.R.
#
# IMPORTANT: install the current version of logitr first as a real (-O2) build,
# so the compiled "cpp" backend is used and timed. Do NOT use
# devtools::load_all() (it compiles -O0 and makes cpp look slow):
#     R CMD INSTALL .        # from the package root
# then run this script from the package root:
#     Rscript data-raw/runtimes_draws.R
#
# The cpu line at 10,000 draws is genuinely slow (that is the point of the
# figure); expect this full run to take a while. Set BENCH_QUICK=1 for a fast
# smoke test, or BENCH_CPU_MAX=<draws> to cap the (slow) cpu line at a lower
# draw count while still running cpp all the way to 10,000.
# ============================================================================

suppressPackageStartupMessages({
  library(logitr)
  library(dplyr)
})
set.seed(1234)

# ---- Configuration ---------------------------------------------------------
numDraws <- if (nzchar(Sys.getenv("BENCH_QUICK"))) {
  c(100)
} else {
  c(100, 500, 1000, 2500, 5000, 10000)
}
maxCores <- parallel::detectCores()
# Cap for the slow cpu line only (cpp always runs the full sweep). Default: no
# cap (run cpu at every draw count).
cpuMax <- suppressWarnings(as.integer(Sys.getenv("BENCH_CPU_MAX")))
if (is.na(cpuMax)) cpuMax <- Inf

# ---- Record versions + machine info ---------------------------------------
benchmark_info_draws <- list(
  date = as.character(Sys.Date()),
  r_version = R.version.string,
  platform = R.version$platform,
  cores = maxCores,
  logitr_version = as.character(packageVersion("logitr"))
)
cat("Benchmark environment:\n")
str(benchmark_info_draws)

# ---- Data prep (matches data-raw/runtimes.R for a comparable model) --------
start_pars <- c(
  price = 0,
  feat = 0,
  brandhiland = 0,
  brandweight = 0,
  brandyoplait = 0,
  sd_feat = 0.1,
  sd_brandhiland = 0.1,
  sd_brandweight = 0.1,
  sd_brandyoplait = 0.1
)
# Use half of the yogurt data, same subset as the head-to-head benchmark
data_logitr <- subset(logitr::yogurt, logitr::yogurt$id <= 50)

# ---- Timing helper + record collector -------------------------------------
timed_eval <- function(expr) {
  start <- Sys.time()
  suppressWarnings(suppressMessages(force(expr)))
  as.numeric(difftime(Sys.time(), start, units = "sec"))
}
records <- list()
record <- function(config, nd, time) {
  records[[length(records) + 1]] <<-
    data.frame(
      config = config,
      time_sec = time,
      numDraws = nd,
      stringsAsFactors = FALSE
    )
}

fit_logitr <- function(nd, backend, numThreads) {
  logitr(
    data = data_logitr,
    outcome = "choice",
    obsID = "obsID",
    panelID = "id",
    pars = c("price", "feat", "brand"),
    randPars = c(feat = "n", brand = "n"),
    startVals = start_pars,
    numDraws = nd,
    backend = backend,
    numThreads = numThreads
  )
}

# ---- Run the sweep ---------------------------------------------------------
cppLabel <- sprintf("cpp (%d cores)", maxCores)
for (nd in numDraws) {
  cat("\n== ", nd, " draws ==\n", sep = "")

  # cpu: native R path (single-threaded), capped by BENCH_CPU_MAX
  if (nd <= cpuMax) {
    t <- timed_eval(fit_logitr(nd, backend = "cpu", numThreads = 1))
    record("cpu", nd, t)
    cat(sprintf("  cpu            : %8.2f s\n", t))
  } else {
    cat("  cpu            :   (skipped, > BENCH_CPU_MAX)\n")
  }

  # cpp, single thread
  t <- timed_eval(fit_logitr(nd, backend = "cpp", numThreads = 1))
  record("cpp (1 core)", nd, t)
  cat(sprintf("  cpp (1 core)   : %8.2f s\n", t))

  # cpp, all cores
  t <- timed_eval(fit_logitr(nd, backend = "cpp", numThreads = maxCores))
  record(cppLabel, nd, t)
  cat(sprintf("  %-14s : %8.2f s\n", cppLabel, t))
}

# ---- Assemble, save, and export --------------------------------------------
runtimes_draws <- tibble::as_tibble(do.call(rbind, records))
runtimes_draws$config <- factor(
  runtimes_draws$config,
  levels = c("cpu", "cpp (1 core)", cppLabel)
)

if (nzchar(Sys.getenv("BENCH_QUICK"))) {
  cat("\nBENCH_QUICK: smoke run complete (not saving). Results:\n")
  print(as.data.frame(runtimes_draws))
  quit(save = "no")
}

readr::write_csv(runtimes_draws, "data-raw/runtimes_draws.csv")
saveRDS(benchmark_info_draws, "data-raw/benchmark_info_draws.rds")
usethis::use_data(runtimes_draws, overwrite = TRUE)

cat("\nDone. Saved data-raw/runtimes_draws.csv and data/runtimes_draws.rda\n")
cat("Machine / version (put this in the benchmark vignette):\n")
str(benchmark_info_draws)
