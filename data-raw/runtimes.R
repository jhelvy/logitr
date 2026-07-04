# ============================================================================
# Benchmark: estimation speed of a preference-space mixed logit model across R
# packages (logitr, mixl, mlogit, gmnl, apollo), used to generate the exported
# `runtimes` data frame.
#
# This is a single self-contained script, run locally from the package root
# with `Rscript data-raw/runtimes.R`. It writes data-raw/runtimes.csv and
# regenerates data/runtimes.rda. The shipped results are specific to the
# machine they were produced on (a 10-core Apple M-series Mac); rerunning on
# different hardware will give different absolute times (and different core
# counts in the multi-core rows), though the relative comparison holds.
#
# INSTALL FIRST (run once):
#     install.packages(c("mlogit", "gmnl", "apollo", "mixl", "fastDummies",
#                        "dplyr", "tidyr", "tibble", "readr"))
#     # and install this package itself (the local dev version, compiled with
#     # full optimization -- do not benchmark under devtools::load_all()):
#     #     R CMD INSTALL --preclean .
#
# For a fast smoke test, set QUICK <- TRUE in the options block near the top.
#
# OPENMP ON MACOS (needed for mixl's multi-core rows): Apple's clang ships
# without OpenMP, so mixl silently runs single-threaded regardless of
# num_threads. To enable it: (1) brew install libomp, (2) add to ~/.R/Makevars:
#     CPPFLAGS += -I/opt/homebrew/opt/libomp/include -Xclang -fopenmp
#     LDFLAGS += -L/opt/homebrew/opt/libomp/lib -lomp
# (3) mixl <= 1.3.5 has a thread-safety bug in its code templates that
# segfaults once OpenMP is actually on: v.data.nrows() (an R API call) is
# evaluated inside the '#pragma omp for' loop bound in
# <mixl>/include/mixl/{loglik,utilities,predict}.cpp. Hoist it above the
# '#pragma omp parallel' line (const int data_nrows = v.data.nrows();) in the
# installed package's include/mixl/ templates. Reinstalling mixl reverts this.
# ============================================================================

suppressPackageStartupMessages({
  library(logitr)
  library(mlogit)
  library(gmnl)
  library(mixl)
  library(apollo)
  library(dplyr)
  library(tidyr)
})
set.seed(1234)

# ---- Options (edit these) --------------------------------------------------
# QUICK = TRUE runs a fast smoke test: only the first draw count below, and
# results are printed but not saved. Set to FALSE for the full benchmark.
QUICK <- FALSE

# Draw counts to benchmark.
numDraws <- c(50, 400, 800, 1200, 1600, 2000)

# Core counts to benchmark for the packages that can run in parallel (logitr,
# mixl, apollo). Defaults to 1, half, and all available cores -- e.g. c(1, 2, 4)
# on a 4-core machine. mlogit and gmnl are always single-threaded. Set this
# directly (e.g. coreCounts <- c(1, 2, 4)) if you want specific counts.
maxCores <- parallel::detectCores()
coreCounts <- sort(unique(c(1, maxCores %/% 2, maxCores)))
# ---- (end of options) ------------------------------------------------------

if (QUICK) {
  numDraws <- numDraws[1]
}

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

# ---- Common settings + data prep -------------------------------------------
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

## mlogit / gmnl. Wrapped in case mlogit.data() breaks on a given mlogit version
## (its behavior changed across major versions); if it fails, mlogit and gmnl are
## simply skipped rather than aborting the whole benchmark.
data_mlogit <- tryCatch(
  mlogit.data(
    data = yogurt,
    shape = "long",
    choice = "choice",
    id.var = "id",
    alt.var = "alt",
    chid.var = "obsID"
  ),
  error = function(e) {
    message(
      "mlogit.data() failed (",
      conditionMessage(e),
      "); skipping mlogit and gmnl."
    )
    NULL
  }
)
data_gmnl <- data_mlogit

## wide-format data (used by apollo and mixl)
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

## apollo model definition (estimated inside the loop below)
apollo_draws_n <- list(
  interDrawsType = "halton",
  interNDraws = 50,
  interUnifDraws = c(),
  interNormDraws = c(
    "d_feat",
    "d_brandhiland",
    "d_brandweight",
    "d_brandyoplait"
  ),
  intraDrawsType = "halton",
  intraNDraws = 0,
  intraUnifDraws = c(),
  intraNormDraws = c()
)
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list()
  randcoeff[["b_feat"]] <- feat + d_feat * sd_feat
  randcoeff[["b_brandhiland"]] <- brandhiland + d_brandhiland * sd_brandhiland
  randcoeff[["b_brandweight"]] <- brandweight + d_brandweight * sd_brandweight
  randcoeff[["b_brandyoplait"]] <- brandyoplait +
    d_brandyoplait * sd_brandyoplait
  return(randcoeff)
}
apollo_fixed <- NULL
apollo_beta <- start_pars
apollo_probabilities <- function(
  apollo_beta,
  apollo_inputs,
  functionality = "estimate"
) {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list()
  V <- list()
  V[["dannon"]] <- price * price_dannon + b_feat * feat_dannon
  V[["hiland"]] <- price * price_hiland + b_brandhiland + b_feat * feat_hiland
  V[["weight"]] <- price * price_weight + b_brandweight + b_feat * feat_weight
  V[["yoplait"]] <- price *
    price_yoplait +
    b_brandyoplait +
    b_feat * feat_yoplait
  mnl_settings <- list(
    alternatives = c(dannon = 1, hiland = 2, weight = 3, yoplait = 4),
    avail = list(
      dannon = av_dannon,
      hiland = av_hiland,
      weight = av_weight,
      yoplait = av_yoplait
    ),
    choiceVar = choice,
    utilities = V
  )
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

# ---- Timing helpers + record collector -------------------------------------
# timed_quiet() suppresses the (verbose) model output. NOTE apollo is timed with
# timed_loud() instead, because its bgw optimizer conflicts with
# suppressMessages()/suppressWarnings().
timed_quiet <- function(expr) {
  start <- Sys.time()
  suppressWarnings(suppressMessages(force(expr)))
  as.numeric(difftime(Sys.time(), start, units = "sec"))
}
timed_loud <- function(expr) {
  start <- Sys.time()
  force(expr)
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

# Estimate one model and record its time. If the model errors (e.g. a package
# is broken on a given machine, or a version mismatch), record NA and keep going
# instead of aborting the whole benchmark. `loud = TRUE` skips output suppression
# (needed for apollo, whose optimizer conflicts with suppressMessages()).
run_model <- function(label, version, nd, expr, loud = FALSE) {
  timer <- if (loud) timed_loud else timed_quiet
  t <- tryCatch(
    timer(expr),
    error = function(e) {
      message("  [", label, ", ", nd, " draws] skipped: ", conditionMessage(e))
      NA_real_
    }
  )
  record(label, version, nd, t)
}

# ---- Estimate every model at every draw count ------------------------------
for (nd in numDraws) {
  cat("\n== Estimating models with", nd, "draws ==\n")

  # logitr (compiled backend) at each thread count
  for (nc in coreCounts) {
    run_model(
      sprintf("logitr (%d cores)", nc),
      versions["logitr"],
      nd,
      logitr(
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
      )
    )
  }

  # mixl at each thread count. mixl parallelizes via OpenMP, which works out
  # of the box on Linux. On macOS it needs a one-time setup (see the OPENMP
  # ON MACOS note in the header); without it the multi-core rows just match
  # the 1-core time.
  for (nc in coreCounts) {
    run_model(
      sprintf("mixl (%d cores)", nc),
      versions["mixl"],
      nd,
      estimate(
        mixl_spec,
        start_pars,
        data_mixl,
        availabilities,
        nDraws = nd,
        num_threads = nc
      )
    )
  }

  # mlogit (single-threaded)
  run_model(
    "mlogit",
    versions["mlogit"],
    nd,
    mlogit(
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
    )
  )

  # gmnl (single-threaded)
  run_model(
    "gmnl",
    versions["gmnl"],
    nd,
    gmnl(
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
    )
  )

  # apollo at each core count (timed "loud"; do not wrap in suppressMessages).
  # The whole setup + estimate is wrapped so an apollo failure on one machine
  # does not abort the run.
  apollo_draws_n$interNDraws <- nd
  for (nc in coreCounts) {
    label <- sprintf("apollo (%d cores)", nc)
    t <- tryCatch(
      {
        apollo_initialise() # reset apollo's environment between models
        apollo_control <- list(
          modelName = "MXL_Pref_space",
          modelDescr = "MXL yogurt, pref space",
          indivID = "id",
          mixing = TRUE,
          analyticGrad = TRUE,
          panelData = TRUE,
          nCores = nc,
          outputDirectory = tempdir(),
          noDiagnostics = TRUE
        )
        inputs <- apollo_validateInputs(
          apollo_beta = start_pars,
          apollo_fixed = apollo_fixed,
          database = data_apollo,
          apollo_draws = apollo_draws_n,
          apollo_randCoeff = apollo_randCoeff,
          apollo_control = apollo_control
        )
        timed_loud(apollo_estimate(
          apollo_beta = start_pars,
          apollo_fixed = apollo_fixed,
          apollo_probabilities = apollo_probabilities,
          apollo_inputs = inputs,
          estimate_settings = list(printLevel = 0, silent = TRUE)
        ))
      },
      error = function(e) {
        message(
          "  [",
          label,
          ", ",
          nd,
          " draws] skipped: ",
          conditionMessage(e)
        )
        NA_real_
      }
    )
    record(sprintf("apollo (%d cores)", nc), versions["apollo"], nd, t)
  }
}

# ---- Assemble, save, and export --------------------------------------------
runtimes <- tibble::as_tibble(do.call(rbind, records))
runtimes <- runtimes %>% mutate(package = as.factor(package))

cat("\nResults:\n")
print(as.data.frame(runtimes))

if (QUICK) {
  cat("\nQUICK smoke run complete (results not saved).\n")
} else {
  # Write the CSV next to the other data-raw files if we are in the package,
  # otherwise into the current working directory.
  out_dir <- if (dir.exists("data-raw")) "data-raw" else "."
  readr::write_csv(runtimes, file.path(out_dir, "runtimes.csv"))
  saveRDS(benchmark_info, file.path(out_dir, "benchmark_info.rds"))
  cat("\nSaved", file.path(out_dir, "runtimes.csv"), "\n")

  # Regenerate the shipped data/runtimes.rda when run from inside the
  # package (a DESCRIPTION file is present).
  if (file.exists("DESCRIPTION")) {
    usethis::use_data(runtimes, overwrite = TRUE)
    cat("Saved data/runtimes.rda\n")
  }
}
cat("\nMachine / versions (for the benchmark vignette):\n")
str(benchmark_info)
