# ============================================================================
# apollo portion of the benchmark, isolated in its own script/process.
#
# apollo maintains finicky global state and introspects the call stack, so it
# is fragile when run in the same session as (and after) other packages'
# estimations. Running it on its own here keeps it clean. It is invoked
# automatically by data-raw/runtimes.R (in a fresh R subprocess) and writes its
# results to data-raw/runtimes_apollo.csv, but can also be run directly:
#     Rscript data-raw/runtimes_apollo.R
# ============================================================================

suppressPackageStartupMessages({
  library(apollo); library(logitr); library(dplyr); library(tidyr)
})
set.seed(1234)

numDraws   <- if (nzchar(Sys.getenv("BENCH_QUICK"))) c(50) else
              c(50, 250, 500, 1000, 2000)
maxCores   <- parallel::detectCores()
coreCounts <- sort(unique(c(1, maxCores %/% 2, maxCores)))
version    <- as.character(packageVersion("apollo"))

start_pars <- c(
  price = 0, feat = 0, brandhiland = 0, brandweight = 0, brandyoplait = 0,
  sd_feat = 0.1, sd_brandhiland = 0.1, sd_brandweight = 0.1, sd_brandyoplait = 0.1)
yogurt <- subset(logitr::yogurt, logitr::yogurt$id <= 50)

# Wide-format data for apollo
yp <- yogurt %>% select(id, obsID, price, brand) %>% mutate(price = -1 * price) %>%
  pivot_wider(names_from = "brand", values_from = "price") %>%
  rename(price_dannon = dannon, price_hiland = hiland,
         price_weight = weight, price_yoplait = yoplait)
yf <- yogurt %>% select(id, obsID, feat, brand) %>%
  pivot_wider(names_from = "brand", values_from = "feat") %>%
  rename(feat_dannon = dannon, feat_hiland = hiland,
         feat_weight = weight, feat_yoplait = yoplait)
yc <- yogurt %>% filter(choice == 1) %>% select(id, obsID, choice = alt)
data_apollo <- yp %>% left_join(yf, by = c("id", "obsID")) %>%
  left_join(yc, by = c("id", "obsID")) %>% arrange(id, obsID) %>%
  mutate(av_dannon = 1, av_hiland = 1, av_weight = 1, av_yoplait = 1)

apollo_control_base <- list(
  modelName = "MXL_Pref_space", modelDescr = "MXL yogurt, pref space",
  indivID = "id", mixing = TRUE, analyticGrad = TRUE, panelData = TRUE,
  nCores = 1, outputDirectory = tempdir(), noDiagnostics = TRUE)
apollo_draws_n <- list(
  interDrawsType = "halton", interNDraws = 50, interUnifDraws = c(),
  interNormDraws = c("d_feat", "d_brandhiland", "d_brandweight", "d_brandyoplait"),
  intraDrawsType = "halton", intraNDraws = 0, intraUnifDraws = c(), intraNormDraws = c())
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
  randcoeff <- list()
  randcoeff[["b_feat"]]         <- feat + d_feat * sd_feat
  randcoeff[["b_brandhiland"]]  <- brandhiland + d_brandhiland * sd_brandhiland
  randcoeff[["b_brandweight"]]  <- brandweight + d_brandweight * sd_brandweight
  randcoeff[["b_brandyoplait"]] <- brandyoplait + d_brandyoplait * sd_brandyoplait
  return(randcoeff)
}
apollo_fixed <- NULL
apollo_beta  <- start_pars
apollo_probabilities <- function(apollo_beta, apollo_inputs, functionality = "estimate") {
  apollo_attach(apollo_beta, apollo_inputs)
  on.exit(apollo_detach(apollo_beta, apollo_inputs))
  P <- list(); V <- list()
  V[["dannon"]]  <- price * price_dannon  + b_feat * feat_dannon
  V[["hiland"]]  <- price * price_hiland  + b_brandhiland  + b_feat * feat_hiland
  V[["weight"]]  <- price * price_weight  + b_brandweight  + b_feat * feat_weight
  V[["yoplait"]] <- price * price_yoplait + b_brandyoplait + b_feat * feat_yoplait
  mnl_settings <- list(
    alternatives = c(dannon = 1, hiland = 2, weight = 3, yoplait = 4),
    avail = list(dannon = av_dannon, hiland = av_hiland,
                 weight = av_weight, yoplait = av_yoplait),
    choiceVar = choice, utilities = V)
  P[["model"]] <- apollo_mnl(mnl_settings, functionality)
  P <- apollo_panelProd(P, apollo_inputs, functionality)
  P <- apollo_avgInterDraws(P, apollo_inputs, functionality)
  P <- apollo_prepareProb(P, apollo_inputs, functionality)
  return(P)
}

records <- list()
for (nd in numDraws) {
  apollo_draws_n$interNDraws <- nd
  for (nc in coreCounts) {
    apollo_initialise()   # reset apollo's environment between models
    ctrl <- apollo_control_base; ctrl$nCores <- nc
    inputs <- apollo_validateInputs(
      apollo_beta = start_pars, apollo_fixed = apollo_fixed,
      database = data_apollo, apollo_draws = apollo_draws_n,
      apollo_randCoeff = apollo_randCoeff, apollo_control = ctrl)
    start <- Sys.time()
    m <- apollo_estimate(
      apollo_beta = start_pars, apollo_fixed = apollo_fixed,
      apollo_probabilities = apollo_probabilities, apollo_inputs = inputs,
      estimate_settings = list(printLevel = 0))
    t <- as.numeric(difftime(Sys.time(), start, units = "sec"))
    records[[length(records) + 1]] <- data.frame(
      package = sprintf("apollo (%d cores)", nc), time_sec = t,
      numDraws = nd, version = version, stringsAsFactors = FALSE)
  }
}
runtimes_apollo <- do.call(rbind, records)
readr::write_csv(runtimes_apollo, "data-raw/runtimes_apollo.csv")
cat("apollo benchmark complete; wrote data-raw/runtimes_apollo.csv\n")
