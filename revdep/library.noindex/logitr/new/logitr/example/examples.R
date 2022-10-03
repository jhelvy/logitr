# This file estimates models that can take longer to estimate
# and then saves those objects so that the vignettes build faster

# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library(logitr)
library(mlogit)
library(gmnl)
library(apollo)
library(mixl)
library(dplyr)
library(tidyr)

# Set number of cores to use
numCores <- 2

# Mixed logit vignette

# Multistart MXL model in the Preference Space
set.seed(456)

mxl_pref <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  panelID  = 'id',
  pars     = c('price', 'feat', 'brand'),
  randPars = c(feat = 'n', brand = 'n'),
  numMultiStarts = 10,
  numCores = numCores
)

# Extract the wtp estimates
wtp_mxl_pref <- wtp(mxl_pref, "price")

# Multistart MXL model in the WTP Space
set.seed(6789)

mxl_wtp <- logitr(
  data       = yogurt,
  outcome    = 'choice',
  obsID      = 'obsID',
  panelID    = 'id',
  pars       = c('feat', 'brand'),
  scalePar   = 'price',
  randPars   = c(feat = 'n', brand = 'n'),
  numMultiStarts = 10,
  startVals = wtp_mxl_pref$Estimate,
  numCores = numCores
)

# Compare results
wtpCompare(mxl_pref, mxl_wtp, "price")

# Multistart MXL model in the Preference Space with correlated heterogeneity
set.seed(456)

mxl_pref_cor <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  panelID  = 'id',
  pars     = c('price', 'feat', 'brand'),
  randPars = c(feat = 'n', brand = 'n'),
  numMultiStarts = 10,
  correlation = TRUE,
  numCores = numCores
)

# Convergence vignette

# Set the starting parameters for each package as well as the
# number of draws to use for the simulated log-likelihood
numDraws_wtp <- 50
start_wtp <- c(
    scalePar        = 1,
    feat            = 0,
    brandhiland     = 0,
    brandweight     = 0,
    brandyoplait    = 0,
    sd_feat         = 0.1,
    sd_brandhiland  = 0.1,
    sd_brandweight  = 0.1,
    sd_brandyoplait = 0.1
)


# Take only half of the yogurt data to speed things up:
yogurt <- subset(logitr::yogurt, logitr::yogurt$id <= 50)

# Model preparation

# Several of the pacakges require hand-specifying many settings and
# reformatting the data. These settings are provided here.

## Prep for {gmnl}

# Convert the yogurt data for mlogit and gmnl using dfidx function
data_gmnl <- mlogit.data(
    data     = yogurt,
    shape    = "long",
    choice   = "choice",
    id.var   = 'id',
    alt.var  = 'alt',
    chid.var = 'obsID'
)

# {apollo} settings

# Format the `yogurt` data to a "wide" format for {apollo}
yogurt_price <- yogurt %>%
    select(id, obsID, price, brand) %>%
    mutate(price = -1*price) %>%
    pivot_wider(
        names_from  = 'brand',
        values_from = 'price') %>%
    rename(
        price_dannon  = dannon,
        price_hiland  = hiland,
        price_weight  = weight,
        price_yoplait = yoplait)
yogurt_feat <- yogurt %>%
    select(id, obsID, feat, brand) %>%
    pivot_wider(
        names_from = 'brand',
        values_from = 'feat') %>%
    rename(
        feat_dannon  = dannon,
        feat_hiland  = hiland,
        feat_weight  = weight,
        feat_yoplait = yoplait)
yogurt_choice <- yogurt %>%
    filter(choice == 1) %>%
    select(id, obsID, choice = alt)
data_apollo <- yogurt_price %>%
    left_join(yogurt_feat, by = c('id', 'obsID')) %>%
    left_join(yogurt_choice, by = c('id', 'obsID')) %>%
    arrange(id, obsID) %>%
    mutate(
        av_dannon  = 1,
        av_hiland  = 1,
        av_weight  = 1,
        av_yoplait = 1
    )

# Define the {apollo} probabilities function
apollo_probabilities_wtp <- function(
        apollo_beta, apollo_inputs, functionality = "estimate"
) {

    ### Attach inputs and detach after function exit
    apollo_attach(apollo_beta, apollo_inputs)
    on.exit(apollo_detach(apollo_beta, apollo_inputs))

    ### Create list of probabilities P
    P <- list()

    ### List of utilities: these must use the same names as in mnl_settings,
    #   order is irrelevant
    V <- list()
    V[["dannon"]] <- scalePar * (b_feat * feat_dannon - price_dannon)
    V[["hiland"]] <- scalePar * (b_brandhiland + b_feat * feat_hiland - price_hiland)
    V[["weight"]] <- scalePar * (b_brandweight + b_feat * feat_weight - price_weight)
    V[["yoplait"]] <- scalePar * (b_brandyoplait + b_feat * feat_yoplait - price_yoplait)

    ### Define settings for MNL model component
    mnl_settings <- list(
        alternatives = c(dannon = 1, hiland = 2, weight = 3, yoplait = 4),
        avail = list(
            dannon = av_dannon,
            hiland = av_hiland,
            weight = av_weight,
            yoplait = av_yoplait),
        choiceVar = choice,
        utilities = V
    )

    ### Compute probabilities using MNL model
    P[["model"]] <- apollo_mnl(mnl_settings, functionality)
    ### Take product across observation for same individual
    P <- apollo_panelProd(P, apollo_inputs, functionality)
    ### Average across inter-individual draws
    P = apollo_avgInterDraws(P, apollo_inputs, functionality)
    ### Prepare and return outputs of function
    P <- apollo_prepareProb(P, apollo_inputs, functionality)

    return(P)
}

# Define random parameters function
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
    randcoeff <- list()
    randcoeff[['b_feat']] <- feat + d_feat * sd_feat
    randcoeff[['b_brandhiland']] <- brandhiland + d_brandhiland * sd_brandhiland
    randcoeff[['b_brandweight']] <- brandweight + d_brandweight * sd_brandweight
    randcoeff[['b_brandyoplait']] <- brandyoplait + d_brandyoplait * sd_brandyoplait
    return(randcoeff)
}

# Main control settings
apollo_control_wtp <- list(
    modelName       = "MXL_WTP_space",
    modelDescr      = "MXL model on yogurt choice SP data, in WTP space",
    indivID         = "id",
    mixing          = TRUE,
    analyticGrad    = TRUE,
    panelData       = TRUE,
    nCores          = 1
)

# Set parameters for generating draws
apollo_draws_n <- list(
    interDrawsType = "halton",
    interNDraws    = numDraws_wtp,
    interUnifDraws = c(),
    interNormDraws = c(
        "d_feat", "d_brandhiland", "d_brandweight", "d_brandyoplait"),
    intraDrawsType = "halton",
    intraNDraws    = 0,
    intraUnifDraws = c(),
    intraNormDraws = c()
)

# Set input
apollo_inputs_wtp <- apollo_validateInputs(
    apollo_beta      = start_wtp,
    apollo_fixed     = NULL,
    database         = data_apollo,
    apollo_draws     = apollo_draws_n,
    apollo_randCoeff = apollo_randCoeff,
    apollo_control   = apollo_control_wtp
)

## {mixl} settings

# Format the `yogurt` data to a "wide" format for {mixl}
data_mixl <- data_apollo # Uses the same "wide" format as {apollo}
data_mixl$ID <- data_mixl$id
data_mixl$CHOICE <- data_mixl$choice

# Define the {mixl} utility function
mixl_wtp <- "
    feat_RND = @feat + draw_1 * @sd_feat;
    brandhiland_RND = @brandhiland + draw_2 * @sd_brandhiland;
    brandweight_RND = @brandweight + draw_3 * @sd_brandweight;
    brandyoplait_RND = @brandyoplait + draw_4 * @sd_brandyoplait;
    U_1 = @scalePar * (feat_RND * $feat_dannon - $price_dannon);
    U_2 = @scalePar * (brandhiland_RND + feat_RND * $feat_hiland - $price_hiland);
    U_3 = @scalePar * (brandweight_RND + feat_RND * $feat_weight - $price_weight);
    U_4 = @scalePar * (brandyoplait_RND + feat_RND * $feat_yoplait - $price_yoplait);
"
mixl_spec_wtp <- specify_model(mixl_wtp, data_mixl)
availabilities <- generate_default_availabilities(data_mixl, 4)

# Estimate WTP space models ----

# The same model is now estimated using all five packages.

## {logitr}

model_logitr <- logitr(
    data      = yogurt,
    outcome   = 'choice',
    obsID     = 'obsID',
    panelID   = 'id',
    pars      = c('feat', 'brand'),
    scalePar  = 'price',
    randPars  = c(feat = "n", brand = "n"),
    startVals = start_wtp,
    numDraws  = numDraws_wtp,
    numCores = numCores
)

# {logitr} converges, even without running a multi-start
summary(model_logitr)

# Including a multi-start helps build confidence in the solution reached:
model_logitr10 <- logitr(
    data      = yogurt,
    outcome   = 'choice',
    obsID     = 'obsID',
    panelID   = 'id',
    pars      = c('feat', 'brand'),
    scalePar  = 'price',
    randPars  = c(feat = "n", brand = "n"),
    startVals = start_wtp,
    numDraws  = numDraws_wtp,
    numMultiStarts = 10,
    numCores = numCores
)
summary(model_logitr10)

## {mixl}

# First attempt using same starting points as {logitr}
model_mixl1 <- estimate(
    mixl_spec_wtp, start_wtp,
    data_mixl, availabilities,
    nDraws = numDraws_wtp
)

# {mixl} converges to a local minimum:
c(logLik(model_logitr), logLik(model_mixl1))
cbind(coef(model_logitr), coef(model_mixl1))

# Second attempt using {logitr} solution as starting points
model_mixl2 <- estimate(
    mixl_spec_wtp, coef(model_logitr),
    data_mixl, availabilities,
    nDraws = numDraws_wtp
)

# Again, {mixl} converges to a local minimum:
c(logLik(model_logitr), logLik(model_mixl2))
cbind(coef(model_logitr), coef(model_mixl2))

## {gmnl}

# First attempt using same starting points as {logitr}.
# Note that additional starting parameters must be added as the {gmnl}
# approach to estimating WTP is a slightly different model.
model_gmnl1 <- gmnl(
    data = data_gmnl,
    formula = choice ~ price + feat + brand | 0 | 0 | 0 | 1,
    ranp = c(
        feat = "n", brandhiland = "n", brandweight = "n",
        brandyoplait = "n"),
    fixed = c(TRUE, rep(FALSE, 10), TRUE),
    model = "gmnl",
    method = "bfgs",
    haltons = NA,
    panel = TRUE,
    start = c(start_wtp, 0.1, 0.1, 0),
    R = numDraws_wtp
)

# {gmnl} converges to a local minimum:
c(logLik(model_logitr), logLik(model_gmnl1))
cbind(coef(model_logitr), coef(model_gmnl1))

# Second attempt using {logitr} solution as starting points:
model_gmnl2 <- gmnl(
    data = data_gmnl,
    formula = choice ~ price + feat + brand | 0 | 0 | 0 | 1,
    ranp = c(
        feat = "n", brandhiland = "n", brandweight = "n",
        brandyoplait = "n"),
    fixed = c(TRUE, rep(FALSE, 10), TRUE),
    model = "gmnl",
    method = "bfgs",
    haltons = NA,
    panel = TRUE,
    start = c(round(coef(model_logitr), 2), 0.1, 0.1, 0),
    R = numDraws_wtp
)

# This approach errors

## {apollo}

# First attempt using same starting points as {logitr}:
model_apollo1 <- apollo_estimate(
    apollo_beta          = start_wtp,
    apollo_fixed         = NULL,
    apollo_probabilities = apollo_probabilities_wtp,
    apollo_inputs        = apollo_inputs_wtp,
    estimate_settings    = list(printLevel = 0)
)

# {apollo} converges to a local minimum:
c(logLik(model_logitr), logLik(model_apollo1))
cbind(coef(model_logitr), coef(model_apollo1))

# Second attempt using {logitr} solution as starting points:
model_apollo2 <- apollo_estimate(
    apollo_beta          = coef(model_logitr),
    apollo_fixed         = NULL,
    apollo_probabilities = apollo_probabilities_wtp,
    apollo_inputs        = apollo_inputs_wtp,
    estimate_settings    = list(printLevel = 0)
)

# Again, {apollo} converges to a local minimum:
c(logLik(model_logitr), logLik(model_apollo2))
cbind(coef(model_logitr), coef(model_apollo2))



# Save results from all estimated models ----
saveRDS(mxl_pref, here::here('inst', 'extdata', 'mxl_pref.Rds'))
saveRDS(mxl_wtp,  here::here('inst', 'extdata', 'mxl_wtp.Rds'))
saveRDS(mxl_pref_cor, here::here('inst', 'extdata', 'mxl_pref_cor.Rds'))
saveRDS(model_logitr, here::here('inst', 'extdata', 'model_logitr.Rds'))
saveRDS(model_logitr10, here::here('inst', 'extdata', 'model_logitr10.Rds'))
saveRDS(model_mixl1, here::here('inst', 'extdata', 'model_mixl1.Rds'))
saveRDS(model_mixl2, here::here('inst', 'extdata', 'model_mixl2.Rds'))
saveRDS(model_gmnl1, here::here('inst', 'extdata', 'model_gmnl1.Rds'))
saveRDS(model_apollo1, here::here('inst', 'extdata', 'model_apollo1.Rds'))
saveRDS(model_apollo2, here::here('inst', 'extdata', 'model_apollo2.Rds'))
