# Basic Usage

## Data format

The {logitr} package requires that data be structured in a `data.frame`
and arranged in a “long” format \[@Wickham2014\] where each row contains
data on a single alternative from a choice observation. The choice
observations do not have to be symmetric, meaning they can have a
“ragged” structure where different choice observations have different
numbers of alternatives. The data must also include variables for each
of the following:

- **Outcome**: A dummy-coded variable that identifies which alternative
  was chosen (`1` is chosen, `0` is not chosen). Only one alternative
  should have a `1` per choice observation.
- **Observation ID**: A sequence of repeated numbers that identifies
  each unique choice observation. For example, if the first three choice
  observations had 2 alternatives each, then the first 6 rows of the
  `obsID` variable would be `1, 1, 2, 2, 3, 3`.
- **Covariates**: Other variables that will be used as model covariates.

The [“Data Formatting and
Encoding”](https://jhelvy.github.io/logitr/articles/data_formatting.md)
vignette has more details about the required data format.

## Model specification interface

Models are specified and estimated using the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function. The `data` argument should be set to the data frame containing
the data, and the `outcome` and `obsID` arguments should be set to the
column names in the data frame that correspond to the dummy-coded
outcome (choice) variable and the observation ID variable, respectively.
All variables to be used as model covariates should be provided as a
vector of column names to the `pars` argument. Each variable in the
vector is additively included as a covariate in the utility model, with
the interpretation that they represent utilities in preference space
models and WTPs in a WTP space model.

For example, consider a preference space model where the utility for
yogurt is given by the following utility model:

$$u_{j} = \alpha p_{j} + \beta_{1}x_{j1} + \beta_{2}x_{j2} + \beta_{3}x_{j3} + \beta_{4}x_{j4} + \varepsilon_{j},$$

where $p_{j}$ is `price`, $x_{j1}$ is `feat`, and $x_{j2 - 4}$ are
dummy-coded variables for each `brand` (with the fourth brand
representing the reference level). This model can be estimated using the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function as follows:

``` r
library("logitr")

mnl_pref <- logitr(
    data    = yogurt,
    outcome = "choice",
    obsID   = "obsID",
    pars    = c("price", "feat", "brand")
)
```

The equivalent model in the WTP space is given by the following utility
model:

$$u_{j} = \lambda\left( \omega_{1}x_{j1} + \omega_{1}x_{j2} + \omega_{1}x_{j3} + \omega_{2}x_{j4} - p_{j} \right) + \varepsilon_{j},$$

To specify this model, simply move `"price"` from the `pars` argument to
the `scalePar` argument:

``` r
mnl_wtp <- logitr(
    data     = yogurt,
    outcome  = "choice",
    obsID    = "obsID",
    pars     = c("feat", "brand"),
    scalePar = "price"
)
```

In the above model, the variables in `pars` are marginal WTPs, whereas
in the preference space model they are marginal utilities. Price is
separately specified with the `scalePar` argument because it acts as a
scaling term in WTP space models. While price is the most typical
scaling variable, other continuous variables can also be used, such as
time.

Interactions between covariates can be entered in the `pars` vector
separated by the `*` symbol. For example, an interaction between `price`
with `feat` in a preference space model could be included by specifying
`pars = c("price", "feat", "brand", "price*feat")`, or even more
concisely just `pars = c("price*feat", "brand")` as the interaction
between `price` and `feat` will produce individual parameters for
`price` and `feat` in addition to the interaction parameter.

Both of these examples are multinomial logit models with fixed
parameters. See the [“Estimating Multinomial Logit
Models”](https://jhelvy.github.io/logitr/articles/mnl_models.md)
vignette for more details.

## Parallelized multi-start estimation

Since WTP space models are non-linear and have non-convex log-likelihood
functions, it is recommended to use a multi-start search to run the
optimization loop multiple times to search for different local minima.
This is implemented using the `numMultiStarts` argument, e.g.:

``` r
mnl_wtp <- logitr(
    data     = yogurt,
    outcome  = "choice",
    obsID    = "obsID",
    pars     = c("feat", "brand"),
    scalePar = "price",
    numMultiStarts = 10
)
```

The multi-start is parallelized by default for faster estimation, and
the number of cores to use can be manually set using the `numCores`
argument. If `numCores` is not provide, then the number of cores is set
to `parallel::detectCores() - 1`. For models with larger data sets, you
may need to set `numCores = 1` to avoid memory overflow issues.

## Mixed logit models

> See the [“Estimating Mixed Logit
> Models”](https://jhelvy.github.io/logitr/articles/mxl_models.md)
> vignette for more details.

To estimate a mixed logit model, use the `randPars` argument in the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function to denote which parameters will be modeled with a distribution.
The current package version supports normal (`"n"`), log-normal
(`"ln"`), and zero-censored normal (`"cn"`) distributions.

For example, assume the observed utility for yogurts was
$v_{j} = \alpha p_{j} + \beta_{1}x_{j1} + \beta_{2}x_{j2} + \beta_{3}x_{j3} + \beta_{4}x_{j4}$,
where $p_{j}$ is `price`, $x_{j1}$ is `feat`, and $x_{j2 - 4}$ are
dummy-coded variables for `brand`. To model `feat` as well as each of
the brands as normally-distributed, set
`randPars = c(feat = "n", brand = "n")`:

``` r
mxl_pref <- logitr(
    data     = yogurt,
    outcome  = 'choice',
    obsID    = 'obsID',
    pars     = c('price', 'feat', 'brand'),
    randPars = c(feat = 'n', brand = 'n'),
    numMultiStarts = 10
)
```

Since mixed logit models also have non-convex log-likelihood functions,
it is recommended to use a multi-start search to run the optimization
loop multiple times to search for different local minima.

## Viewing results

> See the [“Summarizing
> Results”](https://jhelvy.github.io/logitr/articles/summarizing_results.md)
> vignette for more details.

Use the [`summary()`](https://rdrr.io/r/base/summary.html) function to
print a summary of the results from an estimated model, e.g.

``` r
summary(mnl_pref)
#> =================================================
#> 
#> Model estimated on: Fri Dec 19 21:43:44 2025 
#> 
#> Using logitr version: 1.1.3 
#> 
#> Call:
#> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("price", 
#>     "feat", "brand"))
#> 
#> Frequencies of alternatives:
#>        1        2        3        4 
#> 0.402156 0.029436 0.229270 0.339138 
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                 
#> Model Type:    Multinomial Logit
#> Model Space:          Preference
#> Model Run:                1 of 1
#> Iterations:                   21
#> Elapsed Time:        0h:0m:0.03s
#> Algorithm:        NLOPT_LD_LBFGS
#> Weights Used?:             FALSE
#> Robust?                    FALSE
#> 
#> Model Coefficients: 
#>               Estimate Std. Error  z-value  Pr(>|z|)    
#> price        -0.366555   0.024365 -15.0441 < 2.2e-16 ***
#> feat          0.491439   0.120062   4.0932 4.254e-05 ***
#> brandhiland  -3.715477   0.145417 -25.5506 < 2.2e-16 ***
#> brandweight  -0.641138   0.054498 -11.7645 < 2.2e-16 ***
#> brandyoplait  0.734519   0.080642   9.1084 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2656.8878790
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5323.7757580
#> BIC:                     5352.7168000
#> McFadden R2:                0.2054148
#> Adj McFadden R2:            0.2039195
#> Number of Observations:  2412.0000000
```

Use
[`statusCodes()`](https://jhelvy.github.io/logitr/reference/statusCodes.md)
to print a description of each status code from the `nloptr`
optimization routine.

You can also extract other values of interest at the solution, such as:

**The estimated coefficients**

``` r
coef(mnl_pref)
#>        price         feat  brandhiland  brandweight brandyoplait 
#>   -0.3665546    0.4914392   -3.7154773   -0.6411384    0.7345195
```

**The coefficient standard errors**

``` r
se(mnl_pref)
#>        price         feat  brandhiland  brandweight brandyoplait 
#>   0.02436526   0.12006175   0.14541671   0.05449794   0.08064229
```

**The log-likelihood**

``` r
logLik(mnl_pref)
#> 'log Lik.' -2656.888 (df=5)
```

**The variance-covariance matrix**

``` r
vcov(mnl_pref)
#>                      price          feat  brandhiland  brandweight
#> price         0.0005936657  5.729619e-04  0.001851795 1.249988e-04
#> feat          0.0005729619  1.441482e-02  0.000855011 5.092011e-06
#> brandhiland   0.0018517954  8.550110e-04  0.021146019 1.490080e-03
#> brandweight   0.0001249988  5.092011e-06  0.001490080 2.970026e-03
#> brandyoplait -0.0015377721 -1.821331e-03 -0.003681036 7.779427e-04
#>               brandyoplait
#> price        -0.0015377721
#> feat         -0.0018213311
#> brandhiland  -0.0036810363
#> brandweight   0.0007779427
#> brandyoplait  0.0065031782
```

## Computing and comparing WTP

For models in the preference space, a summary table of the computed WTP
based on the estimated preference space parameters can be obtained with
the [`wtp()`](https://jhelvy.github.io/logitr/reference/wtp.md)
function. For example, the computed WTP from the previously estimated
fixed parameter model can be obtained with the following command:

``` r
wtp(mnl_pref, scalePar = "price")
#>                Estimate Std. Error  z-value  Pr(>|z|)    
#> scalePar       0.366555   0.024367  15.0429 < 2.2e-16 ***
#> feat           1.340699   0.360685   3.7171 0.0002015 ***
#> brandhiland  -10.136219   0.585034 -17.3259 < 2.2e-16 ***
#> brandweight   -1.749094   0.182224  -9.5986 < 2.2e-16 ***
#> brandyoplait   2.003848   0.143809  13.9341 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

The [`wtp()`](https://jhelvy.github.io/logitr/reference/wtp.md) function
divides the non-price parameters by the negative of the `scalePar`
parameter (usually “price”). Standard errors are estimated using the
Krinsky and Robb parametric bootstrapping method \[@Krinsky1986\].
Similarly, the
[`wtpCompare()`](https://jhelvy.github.io/logitr/reference/wtpCompare.md)
function can be used to compare the WTP from a WTP space model with that
computed from an equivalent preference space model:

``` r
wtpCompare(mnl_pref, mnl_wtp, scalePar = "price")
#>                       pref           wtp  difference
#> scalePar         0.3665546     0.3665844  0.00002982
#> feat             1.3406987     1.3405717 -0.00012701
#> brandhiland    -10.1362190   -10.1357262  0.00049285
#> brandweight     -1.7490940    -1.7490778  0.00001617
#> brandyoplait     2.0038476     2.0038274 -0.00002024
#> logLik       -2656.8878790 -2656.8878779  0.00000107
```

## Predicting probabilities and outcomes

Estimated models can be used to predict probabilities and outcomes for a
set (or multiple sets) of alternatives based on an estimated model. As
an example, consider predicting probabilities for two of the choice
observations from the `yogurt` dataset:

``` r
data <- subset(
  yogurt, obsID %in% c(42, 13),
  select = c('obsID', 'alt', 'choice', 'price', 'feat', 'brand')
)

data
#>     obsID alt choice price feat   brand
#> 49     13   1      0   8.1    0  dannon
#> 50     13   2      0   5.0    0  hiland
#> 51     13   3      1   8.6    0  weight
#> 52     13   4      0  10.8    0 yoplait
#> 165    42   1      1   6.3    0  dannon
#> 166    42   2      0   6.1    1  hiland
#> 167    42   3      0   7.9    0  weight
#> 168    42   4      0  11.5    0 yoplait
```

In the example below, the probabilities for these two sets of
alternatives are computed using the fixed parameter `mnl_pref` model
using the [`predict()`](https://rdrr.io/r/stats/predict.html) method:

``` r
probs <- predict(
  mnl_pref,
  newdata = data,
  obsID   = "obsID",
  ci      = 0.95
)

probs
#>     obsID predicted_prob predicted_prob_lower predicted_prob_upper
#> 49     13     0.43685145           0.41577964           0.45832350
#> 50     13     0.03312986           0.02623050           0.04157896
#> 51     13     0.19155548           0.17600023           0.20831105
#> 52     13     0.33846321           0.31833043           0.35844177
#> 165    42     0.60764778           0.57371241           0.64034793
#> 166    42     0.02602007           0.01845982           0.03640252
#> 167    42     0.17803313           0.16174641           0.19529813
#> 168    42     0.18829902           0.16844415           0.20928650
```

The resulting `probs` data frame contains the expected probabilities for
each alternative. The lower and upper predictions reflect a 95%
confidence interval (controlled by the `ci` argument), which are
estimated using the Krinsky and Robb parametric bootstrapping method
\[@Krinsky1986\]. The default is `ci = NULL`, in which case no CI
predictions are made.

WTP space models can also be used to predict probabilities:

``` r
probs <- predict(
  mnl_wtp,
  newdata = data,
  obsID   = "obsID",
  ci      = 0.95
)

probs
#>     obsID predicted_prob predicted_prob_lower predicted_prob_upper
#> 49     13     0.43686116           0.41513987           0.45788692
#> 50     13     0.03312963           0.02643243           0.04224291
#> 51     13     0.19154802           0.17606584           0.20755745
#> 52     13     0.33846119           0.31872018           0.35905184
#> 165    42     0.60767163           0.57343133           0.64002670
#> 166    42     0.02601791           0.01845253           0.03647376
#> 167    42     0.17802339           0.16222249           0.19447613
#> 168    42     0.18828706           0.16757845           0.20892092
```

You can also use the [`predict()`](https://rdrr.io/r/stats/predict.html)
method to predict outcomes by setting `type = "outcome"` (the default
value is `"prob"` for predicting probabilities). If no new data are
provided for `newdata`, then outcomes will be predicted for every
alternative in the original data used to estimate the model. In the
example below the `returnData` argument is also set to `TRUE` so that
the predicted outcomes can be compared to the actual ones.

``` r
outcomes <- predict(
  mnl_pref,
  type = "outcome",
  returnData = TRUE
)

head(outcomes[c('obsID', 'choice', 'predicted_outcome')])
#>   obsID choice predicted_outcome
#> 1     1      0                 0
#> 2     1      0                 0
#> 3     1      1                 0
#> 4     1      0                 1
#> 5     2      1                 0
#> 6     2      0                 0
```

You can quickly compute the accuracy by dividing the number of correctly
predicted choices by the total number of choices:

``` r
chosen <- subset(outcomes, choice == 1)
chosen$correct <- chosen$choice == chosen$predicted_outcome
sum(chosen$correct) / nrow(chosen)
#> [1] 0.3922056
```

See the [“Predicting Probabilities and Choices from Estimated
Models”](https://jhelvy.github.io/logitr/articles/predict.md) vignette
for more details about making predictions.
