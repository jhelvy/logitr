# Estimating Mixed Logit Models

This vignette demonstrates an example of how to use the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function to estimate mixed logit (MXL) models with preference space and
WTP space utility parameterizations.

## Supported distributions

The mixed logit model is a popular approach for modeling unobserved
heterogeneity across individuals, which is implemented by assuming that
parameters vary randomly across individuals according to a chosen
distribution (McFadden and Train 2000). A mixed logit model is specified
by setting the `randPars` argument in the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function equal to a named vector defining parameter distributions. In
the example below, we set `randPars = c(feat = 'n', brand = 'n')` so
that `feat` and `brand` are normally distributed. The current package
version supports the following distributions:

- Normal: `"n"`
- Log-normal: `"ln"`
- Zero-censored normal: `"cn"`

Mixed logit models will estimate a mean and standard deviation of the
underlying normal distribution for each random coefficient. Note that
log-normal or zero-censored normal parameters force positivity, so when
using these it is often necessary to use the negative of a value
(e.g. for “price”, which typically has a negative coefficient). Mixed
logit models in `logitr` assume uncorrelated heterogeneity covariances
by default, though full covariances can be estimated using the
`correlation = TRUE` argument. For WTP space models, the `scalePar`
parameter can also be modeled as following a random distribution by
setting the `randScale` argument equal to `"n"`, `"ln"`, or `"cn"`.

## The data

This example uses the
[yogurt](https://jhelvy.github.io/logitr/reference/yogurt.html) data set
from Jain et al. (1994). The data set contains 2,412 choice observations
from a series of yogurt purchases by a panel of 100 households in
Springfield, Missouri, over a roughly two-year period. The data were
collected by optical scanners and contain information about the price,
brand, and a “feature” variable, which identifies whether a newspaper
advertisement was shown to the customer. There are four brands of
yogurt: Yoplait, Dannon, Weight Watchers, and Hiland, with market shares
of 34%, 40%, 23% and 3%, respectively.

In the utility models described below, the data variables are
represented as follows:

| Symbol | Variable |
|----|----|
| $`p`$ | The price in US dollars. |
| $`x_{j}^{\mathrm{Feat}}`$ | Dummy variable for whether the newspaper advertisement was shown to the customer. |
| $`x_{j}^{\mathrm{Hiland}}`$ | Dummy variable for the “Highland” brand. |
| $`x_{j}^{\mathrm{Weight}}`$ | Dummy variable for the “Weight Watchers” brand. |
| $`x_{j}^{\mathrm{Yoplait}}`$ | Dummy variable for the “Yoplait” brand. |

## Preference space model

This example will estimate the following mixed logit model in the
preference space:

``` math
\begin{equation}
    u_{j} =
        \alpha p_{j} +
        \beta_1 x_{j}^{\mathrm{Feat}} +
        \beta_2 x_{j}^{\mathrm{Hiland}} +
        \beta_3 x_{j}^{\mathrm{Weight}} +
        \beta_4 x_{j}^{\mathrm{Yoplait}}  +
        \varepsilon_{j}
\label{eq:mnlPrefExample}
\end{equation}
```

where the parameters $`\alpha`$, $`\beta_1`$, $`\beta_2`$, $`\beta_3`$,
and $`\beta_4`$ have units of utility. In the example below, we will
model $`\beta_1`$, $`\beta_2`$, $`\beta_3`$, and $`\beta_4`$ as
**normally distributed** across the population. As a result, the model
will estimate a mean and standard deviation for each of these
coefficients.

Note that since the `yogurt` data has a panel structure (i.e. multiple
choice observations for each respondent), it is necessary to set the
`panelID` argument to the `id` variable, which identifies the
individual. This will use the panel version of the log-likelihood (see
[Train 2009 chapter
6](https://eml.berkeley.edu/books/choice2nd/Ch06_p134-150.pdf), section
6.7 for details).

Finally, as with WTP space models, it is recommended to use a multistart
search for mixed logit models as they are non-convex. This is
implemented in the example below by setting `numMultiStarts = 10`:

``` r

library("logitr")

set.seed(456)

mxl_pref <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  panelID  = 'id',
  pars     = c('price', 'feat', 'brand'),
  randPars = c(feat = 'n', brand = 'n'),
  numMultiStarts = 10
)
```

Print a summary of the results:

``` r

summary(mxl_pref)
#> =================================================
#> 
#> Model estimated on: Sun Jul 05 18:08:57 2026 
#> 
#> Using logitr version: 1.2.0 
#> 
#> Call:
#> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("price", 
#>     "feat", "brand"), randPars = c(feat = "n", brand = "n"), 
#>     panelID = "id", numMultiStarts = 10, numCores = numCores)
#> 
#> Frequencies of alternatives:
#>        1        2        3        4 
#> 0.402156 0.029436 0.229270 0.339138 
#> 
#> Summary Of Multistart Runs:
#>    Log Likelihood Iterations Exit Status
#> 1       -1248.046         67           3
#> 2       -1243.548         71           3
#> 3       -1255.239         83           3
#> 4       -1244.538         77           3
#> 5       -1243.951         51           3
#> 6       -1258.800        100           3
#> 7       -1247.718         64           3
#> 8       -1248.251         45           3
#> 9       -1264.001         90           3
#> 10      -1248.046         81           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                              
#> Model Type:       Mixed Logit
#> Model Space:       Preference
#> Model Run:            2 of 10
#> Iterations:                71
#> Elapsed Time:        0h:0m:4s
#> Algorithm:     NLOPT_LD_LBFGS
#> Weights Used?:          FALSE
#> Panel ID:                  id
#> Robust?                 FALSE
#> 
#> Model Coefficients: 
#>                  Estimate Std. Error  z-value  Pr(>|z|)    
#> price           -0.457776   0.040953 -11.1782 < 2.2e-16 ***
#> feat             0.929193   0.238074   3.9030 9.502e-05 ***
#> brandhiland     -5.383503   0.443125 -12.1490 < 2.2e-16 ***
#> brandweight     -3.815535   0.413157  -9.2351 < 2.2e-16 ***
#> brandyoplait     0.761100   0.294605   2.5835  0.009781 ** 
#> sd_feat          1.273684   0.307993   4.1354 3.543e-05 ***
#> sd_brandhiland   2.616389   0.342591   7.6371 2.220e-14 ***
#> sd_brandweight   4.202638   0.340843  12.3301 < 2.2e-16 ***
#> sd_brandyoplait  3.831498   0.348782  10.9854 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -1243.5483019
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     2505.0966038
#> BIC:                     2557.1905000
#> McFadden R2:                0.6280968
#> Adj McFadden R2:            0.6254052
#> Number of Observations:  2412.0000000
#> 
#> Summary of 10k Draws for Random Coefficients: 
#>              Min.     1st Qu.     Median       Mean    3rd Qu. Max.
#> feat         -Inf  0.06924067  0.9282196  0.9274097  1.7869054  Inf
#> brandhiland  -Inf -7.14915261 -5.3843423 -5.3859286 -3.6200924  Inf
#> brandweight  -Inf -6.65233541 -3.8189820 -3.8220192 -0.9848315  Inf
#> brandyoplait -Inf -1.82856130  0.7545405  0.7512548  3.3384107  Inf
```

The above summary table prints summaries of the estimated coefficients
as well as a summary table of the distribution of 10,000 population
draws for each normally-distributed model coefficient. The results show
that the `feat` attribute has a significant standard deviation
coefficient, suggesting that there is considerable heterogeneity across
the population for this attribute. In contrast, the `brand` coefficients
have small and insignificant standard deviation coefficients.

Compute the WTP implied from the preference space model:

``` r

wtp_mxl_pref <- wtp(mxl_pref, scalePar =  "price")
wtp_mxl_pref
#>                   Estimate Std. Error z-value  Pr(>|z|)    
#> scalePar          0.457776   0.041033 11.1563 < 2.2e-16 ***
#> feat              2.029800   0.588843  3.4471 0.0005666 ***
#> brandhiland     -11.760136   1.236924 -9.5076 < 2.2e-16 ***
#> brandweight      -8.334946   1.182873 -7.0464 1.837e-12 ***
#> brandyoplait      1.662606   0.636660  2.6115 0.0090159 ** 
#> sd_feat           2.782332   0.718542  3.8722 0.0001079 ***
#> sd_brandhiland    5.715440   0.893710  6.3952 1.604e-10 ***
#> sd_brandweight    9.180564   1.133131  8.1019 4.441e-16 ***
#> sd_brandyoplait   8.369817   0.991508  8.4415 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## WTP space model

This example will estimate the following mixed logit model in the WTP
space:

``` math
\begin{equation}
    u_{j} =
        \lambda (
        \omega_1 x_{j}^{\mathrm{Feat}} +
        \omega_2 x_{j}^{\mathrm{Hiland}} +
        \omega_3 x_{j}^{\mathrm{Weight}} +
        \omega_4 x_{j}^{\mathrm{Yoplait}} -
        p_{j})  +
        \varepsilon_{j}
\label{eq:mnlWtpExample}
\end{equation}
```

where the parameters $`\omega_1`$, $`\omega_2`$, $`\omega_3`$, and
$`\omega_4`$ have units of dollars and $`\lambda`$ is the scale
parameter. In the example below, we will model $`\omega_1`$,
$`\omega_2`$, $`\omega_3`$, and $`\omega_4`$ as normally distributed
across the population. Note that this is a slightly different assumption
than in the preference space model. In the WTP space, we are assuming
that the WTP for these features is normally-distributed (as opposed to
the preference space model where the utility coefficients are assumed to
follow a normal distribution).

In the example below, we also use a 10-iteration multistart. We also set
the starting values for the first iteration to the computed WTP from the
preference space model:

``` r

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
  startVals = wtp_mxl_pref$Estimate
)
```

Print a summary of the results:

``` r

summary(mxl_wtp)
#> =================================================
#> 
#> Model estimated on: Sun Jul 05 18:09:24 2026 
#> 
#> Using logitr version: 1.2.0 
#> 
#> Call:
#> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("feat", 
#>     "brand"), scalePar = "price", randPars = c(feat = "n", brand = "n"), 
#>     panelID = "id", startVals = wtp_mxl_pref$Estimate, numMultiStarts = 10, 
#>     numCores = numCores)
#> 
#> Frequencies of alternatives:
#>        1        2        3        4 
#> 0.402156 0.029436 0.229270 0.339138 
#> 
#> Summary Of Multistart Runs:
#>    Log Likelihood Iterations Exit Status
#> 1       -1246.833        110           3
#> 2       -1244.825         99           3
#> 3       -1244.825        120           3
#> 4       -1334.249        125           4
#> 5       -1249.908         77           3
#> 6       -1241.157         98           3
#> 7       -1263.064         69           3
#> 8       -1246.285         72           3
#> 9       -1244.804         62           4
#> 10      -1249.378         58           4
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                  
#> Model Type:           Mixed Logit
#> Model Space:   Willingness-to-Pay
#> Model Run:                6 of 10
#> Iterations:                    98
#> Elapsed Time:            0h:0m:6s
#> Algorithm:         NLOPT_LD_LBFGS
#> Weights Used?:              FALSE
#> Panel ID:                      id
#> Robust?                     FALSE
#> 
#> Model Coefficients: 
#>                   Estimate Std. Error z-value  Pr(>|z|)    
#> scalePar          0.464716   0.040661 11.4292 < 2.2e-16 ***
#> feat              2.079322   0.571312  3.6396 0.0002731 ***
#> brandhiland     -11.946512   1.327983 -8.9960 < 2.2e-16 ***
#> brandweight      -5.391393   0.983317 -5.4829 4.185e-08 ***
#> brandyoplait      1.367973   0.599257  2.2828 0.0224433 *  
#> sd_feat           2.262513   0.738698  3.0628 0.0021925 ** 
#> sd_brandhiland    6.434834   1.040557  6.1840 6.249e-10 ***
#> sd_brandweight    9.015792   1.047257  8.6090 < 2.2e-16 ***
#> sd_brandyoplait   7.573298   0.848558  8.9249 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -1241.1567812
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     2500.3135624
#> BIC:                     2552.4075000
#> McFadden R2:                0.6288120
#> Adj McFadden R2:            0.6261204
#> Number of Observations:  2412.0000000
#> 
#> Summary of 10k Draws for Random Coefficients: 
#>              Min.     1st Qu.     Median       Mean    3rd Qu. Max.
#> feat         -Inf   0.5517428   2.077593   2.076155  3.6029233  Inf
#> brandhiland  -Inf -16.2890100 -11.948576 -11.952478 -7.6095213  Inf
#> brandweight  -Inf -11.4770963  -5.398789  -5.405305  0.6812284  Inf
#> brandyoplait -Inf  -3.7507247   1.355007   1.348513  6.4622575  Inf
```

If you want to compare the WTP from the two different model spaces, use
the
[`wtpCompare()`](https://jhelvy.github.io/logitr/reference/wtpCompare.md)
function:

``` r

wtpCompare(mxl_pref, mxl_wtp, scalePar = 'price')
#>                          pref           wtp  difference
#> scalePar            0.4577756     0.4647159  0.00694028
#> feat                2.0298000     2.0793223  0.04952227
#> brandhiland       -11.7601355   -11.9465118 -0.18637631
#> brandweight        -8.3349459    -5.3913935  2.94355241
#> brandyoplait        1.6626055     1.3679729 -0.29463267
#> sd_feat             2.7823319     2.2625130 -0.51981897
#> sd_brandhiland      5.7154403     6.4348345  0.71939421
#> sd_brandweight      9.1805643     9.0157924 -0.16477188
#> sd_brandyoplait     8.3698173     7.5732983 -0.79651898
#> logLik          -1243.5483019 -1241.1567812  2.39152071
```

Note that the WTP will not necessarily be the same between preference
space and WTP space MXL models. This is because the distributional
assumptions in MXL models imply different distributions on WTP depending
on the model space. See Train and Weeks (2005) and Sonnier, Ainslie, and
Otter (2007) for details on this topic.

## Correlated heterogeneity

By default, `logitr` assumes that mixed logit models have uncorrelated
heterogeneity. However, correlated heterogeneity can be implemented by
setting `correlation = TRUE` for models in either space (preference or
WTP). The example below shows the results for the same mixed logit model
in the preference space as above but now with correlated heterogeneity:

``` r

library("logitr")

set.seed(456)

mxl_pref_cor <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  panelID  = 'id',
  pars     = c('price', 'feat', 'brand'),
  randPars = c(feat = 'n', brand = 'n'),
  numMultiStarts = 10,
  correlation = TRUE
)
```

Print a summary of the results:

``` r

summary(mxl_pref_cor)
#> =================================================
#> 
#> Model estimated on: Sun Jul 05 18:09:53 2026 
#> 
#> Using logitr version: 1.2.0 
#> 
#> Call:
#> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("price", 
#>     "feat", "brand"), randPars = c(feat = "n", brand = "n"), 
#>     panelID = "id", correlation = TRUE, numMultiStarts = 10, 
#>     numCores = numCores)
#> 
#> Frequencies of alternatives:
#>        1        2        3        4 
#> 0.402156 0.029436 0.229270 0.339138 
#> 
#> Summary Of Multistart Runs:
#>    Log Likelihood Iterations Exit Status
#> 1       -1227.864        197           3
#> 2       -1227.964        121           3
#> 3       -1238.288         94           3
#> 4       -1227.964         73           3
#> 5       -1231.202        213           3
#> 6       -1232.829         84           3
#> 7       -1229.124        150           3
#> 8       -1232.829        110           3
#> 9       -1231.500        191           3
#> 10      -2843.772          5          -1
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                              
#> Model Type:       Mixed Logit
#> Model Space:       Preference
#> Model Run:            1 of 10
#> Iterations:               197
#> Elapsed Time:       0h:0m:12s
#> Algorithm:     NLOPT_LD_LBFGS
#> Weights Used?:          FALSE
#> Panel ID:                  id
#> Robust?                 FALSE
#> 
#> Model Coefficients: 
#>                              Estimate Std. Error  z-value  Pr(>|z|)    
#> price                        -0.45460    0.04113 -11.0529 < 2.2e-16 ***
#> feat                          0.78333    0.25250   3.1023  0.001921 ** 
#> brandhiland                  -4.40630    0.55458  -7.9453 1.998e-15 ***
#> brandweight                  -2.44482    0.40938  -5.9720 2.343e-09 ***
#> brandyoplait                  1.05303    0.32318   3.2584  0.001121 ** 
#> sd_feat_feat                 -0.82442    0.39246  -2.1007  0.035670 *  
#> sd_feat_brandhiland           0.17173    0.26924   0.6379  0.523570    
#> sd_feat_brandweight           0.33876    0.28152   1.2033  0.228853    
#> sd_feat_brandyoplait          0.31532    0.42992   0.7334  0.463287    
#> sd_brandhiland_brandhiland    2.09825    0.41020   5.1152 3.134e-07 ***
#> sd_brandhiland_brandweight    0.58195    0.47574   1.2232  0.221236    
#> sd_brandhiland_brandyoplait   1.43149    0.46535   3.0762  0.002097 ** 
#> sd_brandweight_brandweight    3.67902    0.39489   9.3167 < 2.2e-16 ***
#> sd_brandweight_brandyoplait   2.60251    0.34367   7.5727 3.664e-14 ***
#> sd_brandyoplait_brandyoplait  3.74815    0.33568  11.1659 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -1227.8641286
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     2485.7282572
#> BIC:                     2572.5514000
#> McFadden R2:                0.6327874
#> Adj McFadden R2:            0.6283014
#> Number of Observations:  2412.0000000
#> 
#> Summary of 10k Draws for Random Coefficients: 
#>              Min.    1st Qu.    Median       Mean    3rd Qu. Max.
#> feat         -Inf  0.1333079  0.789591  0.7829895  1.4336861  Inf
#> brandhiland  -Inf -6.1735514 -4.400532 -4.4128234 -2.6565848  Inf
#> brandweight  -Inf -5.4889374 -2.468849 -2.4571845  0.5893883  Inf
#> brandyoplait -Inf -1.4803021  1.046608  1.0433935  3.5742692  Inf
```

## References

Jain, Dipak C, Naufel J Vilcassim, and Pradeep K Chintagunta. 1994. “A
Random-Coefficients Logit Brand-Choice Model Applied to Panel Data.”
*Journal of Business & Economic Statistics* 12 (3): 317–28.

McFadden, Daniel, and Kenneth E. Train. 2000. “Mixed MNL models for
discrete response.” *J. Appl. Econom.* 15 (5): 447–70.
<https://doi.org/10.1002/1099-1255(200009/10)15:5%3C447::AID-JAE570%3E3.0.CO;2-1>.

Sonnier, Garrett, Andrew Ainslie, and Thomas Otter. 2007. “Heterogeneity
distributions of willingness-to-pay in choice models.” *Quant. Mark.
Econ.* 5 (3): 313–31. <https://doi.org/10.1007/s11129-007-9024-6>.

Train, Kenneth E., and Melvyn Weeks. 2005. “Discrete Choice Models in
Preference and Willingness-to-Pay Space.” Chap. 1 in *Appl. Simul.
Methods Environ. Resour. Econ.*
