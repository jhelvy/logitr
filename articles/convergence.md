# WTP space convergence issues in other packages

Given the non-linear utility specification of WTP space models, these
models often diverge during estimation and can be highly sensitive to
starting parameters. While many other packages support the estimation of
WTP space models, in practice these packages often fail to find
solutions.

Compared to most other packages, {logitr} tends to perform better and
converge more often. It also has a built-in, parallelized multi-start
optimization loop for searching for different local minima from
different random starting points when minimizing the negative
log-likelihood, which improves the odds of converging to a solution for
at least some of the multi-start iterations.

This vignette illustrates the convergence issues, comparing the
performance of {logitr} to the following R packages that support WTP
space models:

- {logitr}
- {mixl}
- {mlogit}
- {gmnl}
- {apollo}

For the same mixed logit WTP space model, only {logitr} is able to
consistently converge to a solution.

## Setup

### Basic settings

First load the required packages:

``` r
library(logitr)
library(mlogit)
library(gmnl)
library(apollo)
library(mixl)
library(dplyr)
library(tidyr)

set.seed(1234)
```

Now set the starting parameters for each package as well as the number
of draws to use for the simulated log-likelihood:

``` r
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
```

Take only half of the yogurt data to speed things up:

``` r
yogurt <- subset(logitr::yogurt, logitr::yogurt$id <= 50)
```

### Package-specific settings

Both {apollo} and {mixl} require that the user hand-specify the model to
be estimated as well as several other settings. These settings are
provided here.

#### Prep for {gmnl}

Convert the yogurt data for into the format required for {gmnl}

``` r
data_gmnl <- mlogit.data(
    data     = yogurt,
    shape    = "long",
    choice   = "choice",
    id.var   = 'id',
    alt.var  = 'alt',
    chid.var = 'obsID'
)
```

#### Prep for {apollo}

Format the `yogurt` data to a “wide” format for {apollo}

``` r
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
```

Define the {apollo} probabilities function

``` r
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
```

Define random parameters function

``` r
apollo_randCoeff <- function(apollo_beta, apollo_inputs) {
    randcoeff <- list()
    randcoeff[['b_feat']] <- feat + d_feat * sd_feat
    randcoeff[['b_brandhiland']] <- brandhiland + d_brandhiland * sd_brandhiland
    randcoeff[['b_brandweight']] <- brandweight + d_brandweight * sd_brandweight
    randcoeff[['b_brandyoplait']] <- brandyoplait + d_brandyoplait * sd_brandyoplait
    return(randcoeff)
}
```

Main control settings for {apollo}

``` r
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
```

#### Prep for {mixl}

Format the `yogurt` data to a “wide” format for {mixl}

``` r
data_mixl <- data_apollo # Uses the same "wide" format as {apollo}
data_mixl$ID <- data_mixl$id
data_mixl$CHOICE <- data_mixl$choice
```

Define the {mixl} utility function

``` r
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
```

## Estimate WTP space models

The same model is now estimated using all five packages. We will see
that only {logitr} is able to converge to a solution, while all other
package either fail to converge or converge to only a local minimum.

### {logitr}

{logitr} converges, even without running a multi-start:

``` r
model_logitr <- logitr(
    data      = yogurt,
    outcome   = 'choice',
    obsID     = 'obsID',
    panelID   = 'id',
    pars      = c('feat', 'brand'),
    scalePar  = 'price',
    randPars  = c(feat = "n", brand = "n"),
    startVals = start_wtp,
    numDraws  = numDraws_wtp
)

summary(model_logitr)
```

    #> =================================================
    #> 
    #> Model estimated on: Sat Oct 01 16:46:57 2022 
    #> 
    #> Using logitr version: 0.8.0 
    #> 
    #> Call:
    #> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("feat", 
    #>     "brand"), scalePar = "price", randPars = c(feat = "n", brand = "n"), 
    #>     panelID = "id", startVals = start_wtp, numDraws = numDraws_wtp, 
    #>     numCores = numCores)
    #> 
    #> Frequencies of alternatives:
    #>        1        2        3        4 
    #> 0.415721 0.029984 0.170989 0.383306 
    #> 
    #> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
    #>                                  
    #> Model Type:           Mixed Logit
    #> Model Space:   Willingness-to-Pay
    #> Model Run:                 1 of 1
    #> Iterations:                    85
    #> Elapsed Time:            0h:0m:4s
    #> Algorithm:         NLOPT_LD_LBFGS
    #> Weights Used?:              FALSE
    #> Panel ID:                      id
    #> Robust?                     FALSE
    #> 
    #> Model Coefficients: 
    #>                   Estimate Std. Error z-value  Pr(>|z|)    
    #> scalePar          0.388098   0.050706  7.6538 1.954e-14 ***
    #> feat              1.738832   0.725107  2.3980   0.01648 *  
    #> brandhiland     -13.536714   1.810701 -7.4760 7.661e-14 ***
    #> brandweight      -4.510068   1.015856 -4.4397 9.010e-06 ***
    #> brandyoplait      6.663869   0.929658  7.1681 7.605e-13 ***
    #> sd_feat           0.491225   1.072013  0.4582   0.64679    
    #> sd_brandhiland    5.730571   1.125803  5.0902 3.577e-07 ***
    #> sd_brandweight   11.420500   1.727799  6.6099 3.847e-11 ***
    #> sd_brandyoplait   8.872470   1.366376  6.4934 8.390e-11 ***
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    #>                                      
    #> Log-Likelihood:          -732.2132421
    #> Null Log-Likelihood:    -1710.6872416
    #> AIC:                     1482.4264842
    #> BIC:                     1528.4886000
    #> McFadden R2:                0.5719771
    #> Adj McFadden R2:            0.5667161
    #> Number of Observations:  1234.0000000
    #> 
    #> Summary of 10k Draws for Random Coefficients: 
    #>              Min.     1st Qu.     Median       Mean   3rd Qu. Max.
    #> feat         -Inf   1.4071720   1.738457   1.738145  2.069629  Inf
    #> brandhiland  -Inf -17.4039453 -13.538553 -13.542027 -9.674388  Inf
    #> brandweight  -Inf -12.2189602  -4.519436  -4.527690  3.182253  Inf
    #> brandyoplait -Inf   0.6670773   6.648678   6.641070 12.632059  Inf

Including a 10-run multi-start helps build confidence in the solution
reached:

``` r
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
    numMultiStarts = 10
)

summary(model_logitr)
```

    #> =================================================
    #> 
    #> Model estimated on: Sun Sep 24 14:02:07 2023 
    #> 
    #> Using logitr version: 1.1.0 
    #> 
    #> Call:
    #> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("feat", 
    #>     "brand"), scalePar = "price", randPars = c(feat = "n", brand = "n"), 
    #>     panelID = "id", startVals = start_wtp, numMultiStarts = 10, 
    #>     numDraws = numDraws_wtp)
    #> 
    #> Frequencies of alternatives:
    #>        1        2        3        4 
    #> 0.415721 0.029984 0.170989 0.383306 
    #> 
    #> Summary Of Multistart Runs:
    #>    Log Likelihood Iterations Exit Status
    #> 1       -732.2132         86           3
    #> 2       -728.0567         50           3
    #> 3       -734.7358         67           3
    #> 4       -725.4327         68           3
    #> 5       -731.0111         71           3
    #> 6       -731.3684         58           3
    #> 7       -749.7861         72           3
    #> 8       -718.0862         95           3
    #> 9       -745.6384         50           3
    #> 10      -722.1385         68           3
    #> 
    #> Use statusCodes() to view the meaning of each status code
    #> 
    #> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
    #>                                  
    #> Model Type:           Mixed Logit
    #> Model Space:   Willingness-to-Pay
    #> Model Run:                8 of 10
    #> Iterations:                    95
    #> Elapsed Time:            0h:0m:2s
    #> Algorithm:         NLOPT_LD_LBFGS
    #> Weights Used?:              FALSE
    #> Panel ID:                      id
    #> Robust?                     FALSE
    #> 
    #> Model Coefficients: 
    #>                   Estimate Std. Error z-value  Pr(>|z|)    
    #> scalePar          0.378986   0.048908  7.7490 9.326e-15 ***
    #> feat              1.746832   0.725116  2.4090   0.01599 *  
    #> brandhiland     -13.708716   1.788570 -7.6646 1.799e-14 ***
    #> brandweight      -9.382873   1.543072 -6.0806 1.197e-09 ***
    #> brandyoplait      4.251573   0.628295  6.7668 1.316e-11 ***
    #> sd_feat          -0.930817   0.828995 -1.1228   0.26151    
    #> sd_brandhiland   -4.841849   0.901030 -5.3737 7.715e-08 ***
    #> sd_brandweight   10.849573   1.583590  6.8513 7.321e-12 ***
    #> sd_brandyoplait   7.813052   1.129764  6.9157 4.657e-12 ***
    #> ---
    #> Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
    #>                                      
    #> Log-Likelihood:          -718.0862026
    #> Null Log-Likelihood:    -1710.6872416
    #> AIC:                     1454.1724052
    #> BIC:                     1500.2346000
    #> McFadden R2:                0.5802352
    #> Adj McFadden R2:            0.5749742
    #> Number of Observations:  1234.0000000
    #> 
    #> Summary of 10k Draws for Random Coefficients: 
    #>              Min.    1st Qu.     Median       Mean    3rd Qu. Max.
    #> feat         -Inf   1.120009   1.747543   1.748135   2.375291  Inf
    #> brandhiland  -Inf -16.972056 -13.707162 -13.704227 -10.441231  Inf
    #> brandweight  -Inf -16.706387  -9.391773  -9.399614  -2.075102  Inf
    #> brandyoplait -Inf  -1.029171   4.238197   4.231497   9.507132  Inf

### {mixl}

First attempt using same starting points as {logitr}

``` r
model_mixl <- estimate(
    mixl_spec_wtp, start_wtp,
    data_mixl, availabilities,
    nDraws = numDraws_wtp
)
```

{mixl} converges to a local minimum:

``` r
c(logLik(model_logitr), logLik(model_mixl))
```

    #> [1]  -718.0862 -1544.8531

``` r
cbind(coef(model_logitr), coef(model_mixl))
```

    #>                        [,1]        [,2]
    #> scalePar          0.3789865 -2270.55731
    #> feat              1.7468317    42.24281
    #> brandhiland     -13.7087157    24.36731
    #> brandweight      -9.3828735   110.99687
    #> brandyoplait      4.2515734  -492.36959
    #> sd_feat          -0.9308170    10.81470
    #> sd_brandhiland   -4.8418491    10.23414
    #> sd_brandweight   10.8495725   334.92761
    #> sd_brandyoplait   7.8130522   915.14924

Second attempt using {logitr} solution as starting points

``` r
model_mixl <- estimate(
    mixl_spec_wtp, coef(model_logitr),
    data_mixl, availabilities,
    nDraws = numDraws_wtp
)
```

Again, {mixl} converges to a local minimum (though it’s closer than the
previous solution):

``` r
c(logLik(model_logitr), logLik(model_mixl))
```

    #> [1] -718.0862 -761.5228

``` r
cbind(coef(model_logitr), coef(model_mixl))
```

    #>                        [,1]          [,2]
    #> scalePar          0.3789865    0.01959803
    #> feat              1.7468317   66.46247619
    #> brandhiland     -13.7087157 -172.32720627
    #> brandweight      -9.3828735 -114.92946558
    #> brandyoplait      4.2515734    6.24519288
    #> sd_feat          -0.9308170  -36.14686337
    #> sd_brandhiland   -4.8418491 -227.05348964
    #> sd_brandweight   10.8495725  198.52035799
    #> sd_brandyoplait   7.8130522  173.44727278

### {gmnl}

First attempt using same starting points as {logitr}. Note that
additional starting parameters must be added as the {gmnl} approach to
estimating WTP is a slightly different model.

``` r
model_gmnl <- gmnl(
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
```

{gmnl} converges to a local minimum:

``` r
c(logLik(model_logitr), logLik(model_gmnl))
```

    #> [1]  -718.0862 -1710.6872

``` r
cbind(coef(model_logitr), coef(model_gmnl))
```

    #>                        [,1]         [,2]
    #> feat              0.3789865    8.1540692
    #> brandhiland       1.7468317    4.4266391
    #> brandweight     -13.7087157   20.9581382
    #> brandyoplait     -9.3828735  -93.0969112
    #> het.(Intercept)   4.2515734 -440.5183170
    #> sd.feat          -0.9308170    2.4208585
    #> sd.brandhiland   -4.8418491    0.1085233
    #> sd.brandweight   10.8495725   53.2376327
    #> sd.brandyoplait   7.8130522   95.0008933
    #> tau               0.3789865  653.9956379

Second attempt using {logitr} solution as starting points:

``` r
model_gmnl <- gmnl(
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
    start = c(coef(model_logitr), 0.1, 0.1, 0),
    R = numDraws_wtp
)
```

Again, {gmnl} converges to a local minimum:

``` r
c(logLik(model_logitr), logLik(model_gmnl))
```

    #> [1] -718.0862 -944.3339

``` r
cbind(coef(model_logitr), coef(model_gmnl))
```

    #>                        [,1]       [,2]
    #> feat              0.3789865   2.321317
    #> brandhiland       1.7468317 -18.785933
    #> brandweight     -13.7087157  -5.977394
    #> brandyoplait     -9.3828735   1.761744
    #> het.(Intercept)   4.2515734   6.168432
    #> sd.feat          -0.9308170   2.830422
    #> sd.brandhiland   -4.8418491   6.358857
    #> sd.brandweight   10.8495725  10.600978
    #> sd.brandyoplait   7.8130522   5.455189
    #> tau               0.3789865   8.230516

### {apollo}

First attempt using same starting points as {logitr}:

``` r
model_apollo <- apollo_estimate(
    apollo_beta          = start_wtp,
    apollo_fixed         = NULL,
    apollo_probabilities = apollo_probabilities_wtp,
    apollo_inputs        = apollo_inputs_wtp,
    estimate_settings    = list(printLevel = 0)
)
```

{apollo} fails to converge and just returns the starting coefficients:

``` r
c(logLik(model_logitr), model_apollo$LLout)
```

    #>                 model 
    #>  -718.0862 -2928.4048 

``` r
cbind(coef(model_logitr), coef(model_apollo))
```

    #>                        [,1]
    #> scalePar          0.3789865
    #> feat              1.7468317
    #> brandhiland     -13.7087157
    #> brandweight      -9.3828735
    #> brandyoplait      4.2515734
    #> sd_feat          -0.9308170
    #> sd_brandhiland   -4.8418491
    #> sd_brandweight   10.8495725
    #> sd_brandyoplait   7.8130522

Second attempt using {logitr} solution as starting points:

``` r
model_apollo <- apollo_estimate(
    apollo_beta          = coef(model_logitr),
    apollo_fixed         = NULL,
    apollo_probabilities = apollo_probabilities_wtp,
    apollo_inputs        = apollo_inputs_wtp,
    estimate_settings    = list(printLevel = 0)
)
```

This time {apollo} converges to a local minimum:

``` r
c(logLik(model_logitr), model_apollo$LLout)
```

    #>               model 
    #> -718.0862 -769.7493 

``` r
cbind(coef(model_logitr), model_apollo$betaStop)
```

    #>                        [,1]          [,2]
    #> scalePar          0.3789865  2.376434e-04
    #> feat              1.7468317  5.213661e+03
    #> brandhiland     -13.7087157 -1.622643e+04
    #> brandweight      -9.3828735 -1.529634e+04
    #> brandyoplait      4.2515734  4.573893e+03
    #> sd_feat          -0.9308170  6.131294e+02
    #> sd_brandhiland   -4.8418491 -9.461200e+03
    #> sd_brandweight   10.8495725  1.260092e+04
    #> sd_brandyoplait   7.8130522  1.347027e+04
