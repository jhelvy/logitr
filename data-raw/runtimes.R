# ============================================================================
# Benchmark: estimation speed of a preference-space mixed logit model across
# R packages, used to generate the exported `runtimes` data frame.
#
# This is the local counterpart to data-raw/logitr_benchmark.ipynb (the Google
# Colab notebook). It follows the same structure but (1) runs locally so it can
# use many cores, (2) benchmarks logitr at several thread counts to show how it
# scales, and (3) records the installed version of every package and the machine
# it was run on.
#
# IMPORTANT: install the current version of logitr first, as a real (-O2) build,
# so the compiled "cpp" backend is used and timed:
#     R CMD INSTALL .        # from the package root
# then run this script from the package root:
#     Rscript data-raw/runtimes.R
#
# All compared packages must be installed (mlogit, gmnl, apollo, mixl,
# fastDummies). See the benchmark vignette for installation notes.
# ============================================================================

suppressPackageStartupMessages({
  library(logitr)
  library(mlogit)
  library(gmnl)
  library(mixl)
  library(dplyr)
  library(tidyr)
  library(forcats)
})
# NOTE: apollo is NOT loaded here. It maintains fragile global state and breaks
# when run after other packages' estimations, so it is benchmarked separately in
# data-raw/runtimes_apollo.R (run in a fresh subprocess below).
set.seed(1234)

# ---- Configuration ---------------------------------------------------------
# Set BENCH_QUICK=1 in the environment for a fast smoke run (a single draw
# count) to check that everything works before the full benchmark.
numDraws <- if (nzchar(Sys.getenv("BENCH_QUICK"))) {
  c(50)
} else {
  c(50, 250, 500, 1000, 2000)
}
maxCores <- parallel::detectCores()
# Core counts benchmarked for every package that can run in parallel (logitr,
# mixl, apollo). Every package is run single-threaded (1 core) for a fair
# head-to-head; the parallelizable ones also run at these higher counts.
# mlogit and gmnl are single-threaded only.
coreCounts <- sort(unique(c(1, maxCores %/% 2, maxCores))) # e.g. 1, 5, 10

# ---- Record versions + machine info ---------------------------------------
benchPkgs <- c("logitr", "mixl", "mlogit", "gmnl", "apollo")
versions <- vapply(benchPkgs, function(p) as.character(packageVersion(p)), "")
benchmark_info <- list(
  date = as.character(Sys.Date()),
  r_version = R.version.string,
  platform = R.version$platform,
  cores = maxCores,
  versions = versions
)
cat("Benchmark environment:\n")
str(benchmark_info)

# ---- Common settings + data prep (ported from the notebook) ----------------
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
# Use half of the yogurt data to keep the slower packages tractable
yogurt <- subset(logitr::yogurt, logitr::yogurt$id <= 50)

## logitr
yogurt <- fastDummies::dummy_cols(yogurt, "brand")
data_logitr <- yogurt

## mlogit / gmnl
data_mlogit <- mlogit.data(
  data = yogurt,
  shape = "long",
  choice = "choice",
  id.var = "id",
  alt.var = "alt",
  chid.var = "obsID"
)
data_gmnl <- data_mlogit

## wide-format data (used by mixl below; apollo is benchmarked separately)
yogurt_price <- yogurt %>%
  select(id, obsID, price, brand) %>%
  mutate(price = -1 * price) %>%
  pivot_wider(names_from = "brand", values_from = "price") %>%
  rename(
    price_dannon = dannon,
    price_hiland = hiland,
    price_weight = weight,
    price_yoplait = yoplait
  )
yogurt_feat <- yogurt %>%
  select(id, obsID, feat, brand) %>%
  pivot_wider(names_from = "brand", values_from = "feat") %>%
  rename(
    feat_dannon = dannon,
    feat_hiland = hiland,
    feat_weight = weight,
    feat_yoplait = yoplait
  )
yogurt_choice <- yogurt %>%
  filter(choice == 1) %>%
  select(id, obsID, choice = alt)
data_apollo <- yogurt_price %>%
  left_join(yogurt_feat, by = c("id", "obsID")) %>%
  left_join(yogurt_choice, by = c("id", "obsID")) %>%
  arrange(id, obsID) %>%
  mutate(av_dannon = 1, av_hiland = 1, av_weight = 1, av_yoplait = 1)
## mixl
data_mixl <- data_apollo
data_mixl$ID <- data_mixl$id
data_mixl$CHOICE <- data_mixl$choice
mixl_model <- "
    feat_RND = @feat + draw_1 * @sd_feat;
    brandhiland_RND = @brandhiland + draw_2 * @sd_brandhiland;
    brandweight_RND = @brandweight + draw_3 * @sd_brandweight;
    brandyoplait_RND = @brandyoplait + draw_4 * @sd_brandyoplait;
    U_1 = @price * $price_dannon + feat_RND * $feat_dannon;
    U_2 = @price * $price_hiland + brandhiland_RND + feat_RND * $feat_hiland;
    U_3 = @price * $price_weight + brandweight_RND + feat_RND * $feat_weight;
    U_4 = @price * $price_yoplait + brandyoplait_RND + feat_RND * $feat_yoplait;
"
mixl_spec <- specify_model(mixl_model, data_mixl)
availabilities <- generate_default_availabilities(data_mixl, 4)

# ---- Timing helper + record collector -------------------------------------
# Suppress the (verbose) model output. This is safe here because apollo -- whose
# bgw optimizer conflicts with suppressMessages()/suppressWarnings() -- is run in
# a separate subprocess below, not through this helper.
timed_eval <- function(expr) {
  start <- Sys.time()
  suppressWarnings(suppressMessages(force(expr)))
  as.numeric(difftime(Sys.time(), start, units = "sec"))
}
records <- list()
record <- function(label, version, nd, time) {
  records[[length(records) + 1]] <<-
    data.frame(
      package = label,
      time_sec = time,
      numDraws = nd,
      version = unname(version),
      stringsAsFactors = FALSE
    )
}

# ---- Estimate every model at every draw count ------------------------------
for (nd in numDraws) {
  cat("\n== Estimating models with", nd, "draws ==\n")

  # logitr (compiled backend) at each thread count
  for (nc in coreCounts) {
    t <- timed_eval(logitr(
      data = data_logitr,
      outcome = "choice",
      obsID = "obsID",
      panelID = "id",
      pars = c("price", "feat", "brand"),
      randPars = c(feat = "n", brand = "n"),
      startVals = start_pars,
      numDraws = nd,
      backend = "cpp",
      numThreads = nc
    ))
    record(sprintf("logitr (%d cores)", nc), versions["logitr"], nd, t)
  }

  # mixl at each thread count. mixl parallelizes via OpenMP: on Linux (e.g. the
  # Docker image or Colab) num_threads scales estimation across cores. Under the
  # default macOS toolchain (Apple clang) OpenMP is unavailable, so num_threads
  # has no effect and the multi-core rows will simply match the 1-core time --
  # which is why the canonical multi-core benchmark should be run on Linux. See
  # the benchmark vignette for details.
  for (nc in coreCounts) {
    t <- timed_eval(estimate(
      mixl_spec,
      start_pars,
      data_mixl,
      availabilities,
      nDraws = nd,
      num_threads = nc
    ))
    record(sprintf("mixl (%d cores)", nc), versions["mixl"], nd, t)
  }

  # mlogit (single-threaded)
  t <- timed_eval(mlogit(
    data = data_mlogit,
    formula = choice ~ price + feat + brand | 0,
    rpar = c(
      feat = "n",
      brandhiland = "n",
      brandweight = "n",
      brandyoplait = "n"
    ),
    haltons = NA,
    panel = TRUE,
    start = start_pars,
    R = nd
  ))
  record("mlogit", versions["mlogit"], nd, t)

  # gmnl (single-threaded)
  t <- timed_eval(gmnl(
    data = data_gmnl,
    formula = choice ~ price + feat + brand | 0,
    ranp = c(
      feat = "n",
      brandhiland = "n",
      brandweight = "n",
      brandyoplait = "n"
    ),
    model = "mixl",
    haltons = NA,
    panel = TRUE,
    start = start_pars,
    R = nd
  ))
  record("gmnl", versions["gmnl"], nd, t)
}

# ---- apollo, run in a fresh subprocess (isolated from the above) ------------
cat("\n== Running apollo in a separate process ==\n")
apollo_script <- "data-raw/runtimes_apollo.R"
apollo_log <- tempfile(fileext = ".log")
apollo_status <- system2(
  file.path(R.home("bin"), "Rscript"),
  args = apollo_script,
  env = if (nzchar(Sys.getenv("BENCH_QUICK"))) {
    "BENCH_QUICK=1"
  } else {
    character(0)
  },
  stdout = apollo_log,
  stderr = apollo_log
) # keep apollo's output quiet
if (apollo_status != 0 || !file.exists("data-raw/runtimes_apollo.csv")) {
  cat(readLines(apollo_log), sep = "\n")
  stop("apollo benchmark failed; see output above")
}
apollo_records <- readr::read_csv(
  "data-raw/runtimes_apollo.csv",
  show_col_types = FALSE
)
for (i in seq_len(nrow(apollo_records))) {
  r <- apollo_records[i, ]
  record(r$package, r$version, r$numDraws, r$time_sec)
}

# ---- Assemble, save, and export --------------------------------------------
runtimes <- tibble::as_tibble(do.call(rbind, records))
runtimes <- runtimes %>% mutate(package = as.factor(package))

if (nzchar(Sys.getenv("BENCH_QUICK"))) {
  cat("\nBENCH_QUICK: smoke run complete (not saving). Results:\n")
  print(as.data.frame(runtimes))
  quit(save = "no")
}

readr::write_csv(runtimes, "data-raw/runtimes.csv")
saveRDS(benchmark_info, "data-raw/benchmark_info.rds")

# Regenerate the shipped data/runtimes.rda only when run from inside the package
# (i.e. a local checkout or clone, where a DESCRIPTION file is present). On a
# bare notebook working directory (e.g. Kaggle) use_data() has no package to
# write to, so skip it -- the CSV is the deliverable to copy back and turn into
# data/runtimes.rda locally with: usethis::use_data(runtimes, overwrite = TRUE).
if (file.exists("DESCRIPTION")) {
  usethis::use_data(runtimes, overwrite = TRUE)
  cat("\nDone. Saved data-raw/runtimes.csv and data/runtimes.rda\n")
} else {
  cat("\nDone. Saved data-raw/runtimes.csv (no package here, so data/runtimes.rda",
      "was skipped -- rebuild it locally from the CSV).\n")
}
cat("Machine / versions (put these in the benchmark vignette):\n")
str(benchmark_info)
