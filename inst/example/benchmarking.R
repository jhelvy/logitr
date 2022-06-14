# The code below compares the relative computation time to estimate
# a preference space mixed logit models using the
# {logitr}, {mixl}, {mlogit}, {gmnl}, and {apollo} packages

# Install packages for creating figures
# install.packages(c("tidyverse", "cowplot", "ggrepel", "showtext"))

# Install packages for benchmarking
# install.packages(c("logitr", "mlogit", "gmnl", "apollo", "mixl", "fastDummies"))

# Load libraries for making figures
library(tidyverse)
library(cowplot)
library(ggrepel)
library(showtext)

# Load libraries for estimating models
library(logitr)
library(mlogit)
library(gmnl)
library(apollo)
library(mixl)

set.seed(1234)

# Set main font for plots
font_add_google("Fira Sans Condensed", "firasanscondensed")
showtext_auto() # Automatically use showtext to render text
font_main <- "firasanscondensed"

# Model preparation

# Each package requires specific data formatting, and some like {mixl} and
# {apollo} require that you specify the explicit utility model as a function or
# string. So I encode all of this here prior to estimating any models.

# Set common starting parameters for each model
start_pars <- c(
    price           = 0,
    feat            = 0,
    brandhiland     = 0,
    brandweight     = 0,
    brandyoplait    = 0,
    sd_feat         = 0.1,
    sd_brandhiland  = 0.1,
    sd_brandweight  = 0.1,
    sd_brandyoplait = 0.1
)

## Prep for {logitr}

# Add dummy-coded variables for brand so that the reference level
# for brand can be manually set
yogurt <- fastDummies::dummy_cols(yogurt, "brand")
data_logitr <- yogurt

## Prep for {mlogit}

# Convert the yogurt data for mlogit and gmnl using dfidx function
data_mlogit <- mlogit.data(
    data    = yogurt,
    shape   = "long",
    choice  = "choice",
    id.var  = 'id',
    alt.var = 'alt',
    chid.var = 'obsID'
)

## Prep for {gmnl}

# Same format as mlogit
data_gmnl <- data_mlogit
parallel::detectCores()

## Prep for {apollo}

# {apollo} requires a LOT of settings, including hand-defining the
# probabilities function.

# First, convert the data shape to "wide" format
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

# Set core controls - using both 1 and 3 core options for parallel testing
apollo_control_1 <- list(
    modelName       = "MXL_Pref_space",
    modelDescr      = "MXL model on yogurt choice SP data, in Pref space",
    indivID         = "id",
    mixing          = TRUE,
    analyticGrad    = TRUE,
    outputDirectory = NULL,
    panelData       = TRUE,
    nCores          = 1
)
apollo_control_3 <- apollo_control_1
apollo_control_3$nCores <- 3

# Set parameters for generating draws - using multiple sets of draws
# to test speeds as draws increases
apollo_draws_50 <- list(
    interDrawsType = "halton",
    interNDraws    = 50,
    interUnifDraws = c(),
    interNormDraws = c(
        "d_feat", "d_brandhiland", "d_brandweight", "d_brandyoplait"),
    intraDrawsType = "halton",
    intraNDraws    = 0,
    intraUnifDraws = c(),
    intraNormDraws = c()
)
apollo_draws_200  <- apollo_draws_50
apollo_draws_400  <- apollo_draws_50
apollo_draws_600  <- apollo_draws_50
apollo_draws_800  <- apollo_draws_50
apollo_draws_1000 <- apollo_draws_50
apollo_draws_200$interNDraws  <- 200
apollo_draws_400$interNDraws  <- 400
apollo_draws_600$interNDraws  <- 600
apollo_draws_800$interNDraws  <- 800
apollo_draws_1000$interNDraws <- 1000

# Make lists of draws to access in loop
apollo_draws_list <- list(
    apollo_draws_50, apollo_draws_200, apollo_draws_400,
    apollo_draws_600, apollo_draws_800, apollo_draws_1000
)

# Define random parameters function
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
    randcoeff <- list()
    randcoeff[['b_feat']] <- feat + d_feat * sd_feat
    randcoeff[['b_brandhiland']] <- brandhiland + d_brandhiland * sd_brandhiland
    randcoeff[['b_brandweight']] <- brandweight + d_brandweight * sd_brandweight
    randcoeff[['b_brandyoplait']] <- brandyoplait + d_brandyoplait * sd_brandyoplait
    return(randcoeff)
}

# Set fixed levels
apollo_fixed <- NULL

# Define probabilities function

apollo_probabilities <- function(
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
    V[["dannon"]] <- price * price_dannon + b_feat * feat_dannon
    V[["hiland"]] <- price * price_hiland + b_brandhiland + b_feat * feat_hiland
    V[["weight"]] <- price * price_weight + b_brandweight + b_feat * feat_weight
    V[["yoplait"]] <- price * price_yoplait + b_brandyoplait + b_feat * feat_yoplait

    ### Define settings for MNL model component
    mnl_settings <- list(
        alternatives = c(dannon = 1, hiland = 2, weight = 3, yoplait = 4),
        avail = list(
          dannon  = av_dannon,
          hiland  = av_hiland,
          weight  = av_weight,
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

## Prep for {mixl}

# {mixl} also requires explicitly specifying the model, similar to {apollo}

data_mixl <- data_apollo # Uses the same "wide" format as {apollo}
data_mixl$ID <- data_mixl$id
data_mixl$CHOICE <- data_mixl$choice

# Define model
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

# Estimate models

# The loop below estimates the same model from the same starting points for
# each package. The estimation times are stored in a vector, `time`, and the
# models themselves are stored in a list, `models`.

# Function for timing the evaluation of an expression
timed_eval <- function(expr) {
    start <- Sys.time()
    suppressWarnings({suppressMessages({eval(expr)})})
    time <- as.numeric(difftime(Sys.time(), start, units = 'sec'))
    return(time)
}

numDraws <- c(50, 200, 400, 600, 800, 1000)
models <- list()
time <- c()

for (i in 1:length(numDraws)) {

    cat("Estimating Models With ", numDraws[i], "Draws\n\n")

    # {logitr}
    time_logitr <- timed_eval({
        model_logitr <- logitr(
            data    = data_logitr,
            outcome = 'choice',
            obsID   = 'obsID',
            panelID = 'id',
            pars    = c('price', 'feat', 'brand'),
            randPars = c(feat = "n", brand = "n"),
            startVals = start_pars,
            numDraws = numDraws[i]
        )
    })

    # {mixl} - 1 core
    time_mixl1 <- timed_eval({
        model_mixl1 <- estimate(
            mixl_spec, start_pars,
            data_mixl, availabilities,
            nDraws = numDraws[i],
            num_threads = 1
        )
    })

    # {mixl} - 3 cores
    time_mixl3 <- timed_eval({
        model_mixl3 <- estimate(
            mixl_spec, start_pars,
            data_mixl, availabilities,
            nDraws = numDraws[i],
            num_threads = 3
        )
    })

    # {mlogit}
    time_mlogit <- timed_eval({
        model_mlogit <- mlogit(
            data = data_mlogit,
            formula = choice ~ price + feat + brand | 0,
            rpar = c(
                feat = "n", brandhiland = "n", brandweight = "n",
                brandyoplait = "n"),
            haltons = NA,
            panel = TRUE,
            start = start_pars,
            R = numDraws[i]
        )
    })

    # {gmnl}
    time_gmnl <- timed_eval({
        model_gmnl <- gmnl(
            data = data_gmnl,
            formula = choice ~ price + feat + brand | 0,
            ranp = c(
                feat = "n", brandhiland = "n", brandweight = "n",
                brandyoplait = "n"),
            model = "mixl",
            haltons = NA,
            panel = TRUE,
            start = start_pars,
            R = numDraws[i]
        )
    })

    # {apollo} - 1 core

    # First validate the inputs
    apollo_inputs <- apollo_validateInputs(
        apollo_beta      = start_pars,
        apollo_fixed     = apollo_fixed,
        database         = data_apollo,
        apollo_draws     = apollo_draws_list[[i]],
        apollo_randCoeff = apollo_randCoeff,
        apollo_control   = apollo_control_1
    )

    # Now estimate the model
    time_apollo1 <- timed_eval({
        model_apollo1 <- apollo_estimate(
            apollo_beta          = start_pars,
            apollo_fixed         = apollo_fixed,
            apollo_probabilities = apollo_probabilities,
            apollo_inputs        = apollo_inputs,
            estimate_settings    = list(printLevel = 0)
        )
    })

    # {apollo} - 3 cores

    # First validate the inputs
    apollo_inputs <- apollo_validateInputs(
        apollo_beta      = start_pars,
        apollo_fixed     = apollo_fixed,
        database         = data_apollo,
        apollo_draws     = apollo_draws_list[[i]],
        apollo_randCoeff = apollo_randCoeff,
        apollo_control   = apollo_control_3
    )

    # Now estimate the model
    time_apollo3 <- timed_eval({
        model_apollo3 <- apollo_estimate(
            apollo_beta          = start_pars,
            apollo_fixed         = apollo_fixed,
            apollo_probabilities = apollo_probabilities,
            apollo_inputs        = apollo_inputs,
            estimate_settings    = list(printLevel = 0)
        )
    })

    # Store time results
    time <- c(time,
        time_logitr, time_mixl1, time_mixl3, time_mlogit,
        time_gmnl, time_apollo1, time_apollo3
    )

    # Store model results
    models[[i]] <- list(
        logitr  = model_logitr,
        mixl1   = model_mixl1,
        mixl3   = model_mixl3,
        mlogit  = model_mlogit,
        gmnl    = model_gmnl,
        apollo1 = model_apollo1,
        apollo3 = model_apollo3
    )
}

# Compare results

# First, compare that the models reached similar solutions


# Compare log-likelihoods at solution
logLiks <- c()
for (i in 1:length(numDraws)) {
    logLiks <- c(
        logLiks,
        unlist(lapply(models[[i]], function(x) logLik(x)))
    )
}
packages <- c("logitr", "mixl", "mlogit", "gmnl", "apollo1", "apollo2")
ll_df <- data.frame(
    package = rep(packages, length(numDraws)),
    logLik   = logLiks,
    numDraws = rep(numDraws, each = length(packages))) %>%
    group_by(numDraws) %>%
    pivot_wider(names_from = package, values_from = logLik)
ll_df

# Compare coefficients at solution with 1000 draws (more robust solution)
coefs <- lapply(models[[length(numDraws)]], function(x) coef(x))
as.data.frame(do.call(cbind, coefs))

# All models reached similar solutions.

# Now compare estimation times:

# Construct time results data frame
runtimes <- data.frame(
    package = rep(packages, length(numDraws)),
    time_sec = time,
    numDraws = rep(numDraws, each = length(packages))
)

# Compute speed comparison tables
logitr_time <- runtimes %>%
    filter(package == "logitr") %>%
    rename(time_logitr = time_sec)
temp <- runtimes %>%
    # Compute % time slower than {logitr}
    left_join(select(logitr_time, -package), by = "numDraws") %>%
    mutate(mult = round(time_sec/ time_logitr, 1)) %>%
    select(-time_logitr)
table_time <- temp %>%
    select(-mult) %>%
    pivot_wider(names_from = numDraws, values_from = time_sec)
table_time
table_mult <- temp %>%
    select(-time_sec) %>%
    pivot_wider(names_from = numDraws, values_from = mult)
table_mult

# Visualize estimation times

# Make the figure
plotColors <- c("black", "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00")
fig2 <- runtimes %>%
    ggplot(aes(x = numDraws, y = time_sec, color = package)) +
    geom_line() +
    geom_point() +
    geom_text_repel(
        data = runtimes %>% filter(numDraws == max(numDraws)),
        aes(label = package),
        hjust = 0, nudge_x = 20, direction = "y",
        size = 5
    ) +
    scale_x_continuous(
        limits = c(0, 1100),
        breaks = numDraws,
        labels = scales::comma) +
    scale_y_continuous(limits = c(0, 600)) +
    scale_color_manual(values = plotColors) +
    guides(
        point = guide_legend(override.aes = list(label = "")),
        color = guide_legend(override.aes = list(label = ""))) +
    theme_minimal_hgrid(font_family = font_main, font_size = 16) +
    theme(
        legend.position = "none",
        axis.line.x = element_blank(),
        axis.ticks.x = element_blank()
    ) +
    labs(
        x = "Number of random draws",
        y = "Computation time (seconds)"
    )

# Save run time data frame
write_csv(runtimes, here::here('inst', 'extdata', 'runtimes.csv'))
