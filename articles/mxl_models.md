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

| Symbol            | Variable                                                                          |
|-------------------|-----------------------------------------------------------------------------------|
| $p$               | The price in US dollars.                                                          |
| $x_{j}^{Feat}$    | Dummy variable for whether the newspaper advertisement was shown to the customer. |
| $x_{j}^{Hiland}$  | Dummy variable for the “Highland” brand.                                          |
| $x_{j}^{Weight}$  | Dummy variable for the “Weight Watchers” brand.                                   |
| $x_{j}^{Yoplait}$ | Dummy variable for the “Yoplait” brand.                                           |

## Preference space model

This example will estimate the following mixed logit model in the
preference space:

$$u_{j} = \alpha p_{j} + \beta_{1}x_{j}^{Feat} + \beta_{2}x_{j}^{Hiland} + \beta_{3}x_{j}^{Weight} + \beta_{4}x_{j}^{Yoplait} + \varepsilon_{j}$$

where the parameters $\alpha$, $\beta_{1}$, $\beta_{2}$, $\beta_{3}$,
and $\beta_{4}$ have units of utility. In the example below, we will
model $\beta_{1}$, $\beta_{2}$, $\beta_{3}$, and $\beta_{4}$ as
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
#> Model estimated on: Wed Sep 27 08:37:32 2023 
#> 
#> Using logitr version: 1.1.1 
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
#> 1       -1266.550         34           3
#> 2       -1300.751         64           3
#> 3       -1260.216         35           3
#> 4       -1261.216         43           3
#> 5       -1269.066         40           3
#> 6       -1239.294         56           3
#> 7       -1343.221         59           3
#> 8       -1260.006         55           3
#> 9       -1273.143         52           3
#> 10      -1304.384         59           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                              
#> Model Type:       Mixed Logit
#> Model Space:       Preference
#> Model Run:            6 of 10
#> Iterations:                56
#> Elapsed Time:        0h:0m:1s
#> Algorithm:     NLOPT_LD_LBFGS
#> Weights Used?:          FALSE
#> Panel ID:                  id
#> Robust?                 FALSE
#> 
#> Model Coefficients: 
#>                  Estimate Std. Error  z-value  Pr(>|z|)    
#> price           -0.448338   0.039987 -11.2120 < 2.2e-16 ***
#> feat             0.776990   0.193521   4.0150 5.944e-05 ***
#> brandhiland     -6.367360   0.520828 -12.2255 < 2.2e-16 ***
#> brandweight     -3.668683   0.307207 -11.9421 < 2.2e-16 ***
#> brandyoplait     1.122492   0.203483   5.5164 3.460e-08 ***
#> sd_feat          0.567495   0.225004   2.5222   0.01166 *  
#> sd_brandhiland  -3.181844   0.371697  -8.5603 < 2.2e-16 ***
#> sd_brandweight   4.097130   0.232495  17.6225 < 2.2e-16 ***
#> sd_brandyoplait  3.261281   0.219902  14.8306 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -1239.2944250
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     2496.5888500
#> BIC:                     2548.6828000
#> McFadden R2:                0.6293690
#> Adj McFadden R2:            0.6266774
#> Number of Observations:  2412.0000000
#> 
#> Summary of 10k Draws for Random Coefficients: 
#>              Min.    1st Qu.     Median       Mean    3rd Qu. Max.
#> feat         -Inf  0.3938347  0.7765564  0.7761956  1.1591475  Inf
#> brandhiland  -Inf -8.5118796 -6.3663393 -6.3644101 -4.2201174  Inf
#> brandweight  -Inf -6.4342648 -3.6720435 -3.6750045 -0.9090452  Inf
#> brandyoplait -Inf -1.0817673  1.1169084  1.1141118  3.3162383  Inf
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
#>                   Estimate Std. Error  z-value  Pr(>|z|)    
#> scalePar          0.448338   0.039906  11.2347 < 2.2e-16 ***
#> feat              1.733046   0.500457   3.4629 0.0005343 ***
#> brandhiland     -14.202148   1.388968 -10.2250 < 2.2e-16 ***
#> brandweight      -8.182853   0.979963  -8.3502 < 2.2e-16 ***
#> brandyoplait      2.503674   0.411267   6.0877 1.145e-09 ***
#> sd_feat           1.265776   0.505747   2.5028 0.0123220 *  
#> sd_brandhiland   -7.096979   0.958253  -7.4062 1.299e-13 ***
#> sd_brandweight    9.138487   0.947665   9.6432 < 2.2e-16 ***
#> sd_brandyoplait   7.274160   0.771684   9.4263 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## WTP space model

This example will estimate the following mixed logit model in the WTP
space:

$$u_{j} = \lambda\left( \omega_{1}x_{j}^{Feat} + \omega_{2}x_{j}^{Hiland} + \omega_{3}x_{j}^{Weight} + \omega_{4}x_{j}^{Yoplait} - p_{j} \right) + \varepsilon_{j}$$

where the parameters $\omega_{1}$, $\omega_{2}$, $\omega_{3}$, and
$\omega_{4}$ have units of dollars and $\lambda$ is the scale parameter.
In the example below, we will model $\omega_{1}$, $\omega_{2}$,
$\omega_{3}$, and $\omega_{4}$ as normally distributed across the
population. Note that this is a slightly different assumption than in
the preference space model. In the WTP space, we are assuming that the
WTP for these features is normally-distributed (as opposed to the
preference space model where the utility coefficients are assumed to
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
#> Model estimated on: Wed Sep 27 08:37:40 2023 
#> 
#> Using logitr version: 1.1.1 
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
#> 1       -1239.294        110           3
#> 2       -1252.536         76           3
#> 3       -1258.974         87           3
#> 4       -1342.088        110           4
#> 5       -1250.922        111           3
#> 6       -1266.990         66           3
#> 7       -1268.352         81           3
#> 8       -1239.294         76           3
#> 9       -1258.974         60           3
#> 10      -1239.294         51           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                  
#> Model Type:           Mixed Logit
#> Model Space:   Willingness-to-Pay
#> Model Run:                8 of 10
#> Iterations:                    76
#> Elapsed Time:            0h:0m:2s
#> Algorithm:         NLOPT_LD_LBFGS
#> Weights Used?:              FALSE
#> Panel ID:                      id
#> Robust?                     FALSE
#> 
#> Model Coefficients: 
#>                   Estimate Std. Error  z-value  Pr(>|z|)    
#> scalePar          0.448664   0.040011  11.2136 < 2.2e-16 ***
#> feat              1.731594   0.491861   3.5205 0.0004307 ***
#> brandhiland     -14.223131   1.365740 -10.4142 < 2.2e-16 ***
#> brandweight      -8.170605   0.955887  -8.5477 < 2.2e-16 ***
#> brandyoplait      2.504170   0.407198   6.1498 7.760e-10 ***
#> sd_feat           1.266643   0.497394   2.5466 0.0108791 *  
#> sd_brandhiland   -7.114238   0.944440  -7.5328 4.974e-14 ***
#> sd_brandweight    9.129315   0.923604   9.8845 < 2.2e-16 ***
#> sd_brandyoplait   7.269364   0.752767   9.6569 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -1239.2939753
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     2496.5879505
#> BIC:                     2548.6819000
#> McFadden R2:                0.6293691
#> Adj McFadden R2:            0.6266775
#> Number of Observations:  2412.0000000
#> 
#> Summary of 10k Draws for Random Coefficients: 
#>              Min.     1st Qu.     Median       Mean   3rd Qu. Max.
#> feat         -Inf   0.8763949   1.730626   1.729820  2.584565  Inf
#> brandhiland  -Inf -19.0180303 -14.220849 -14.216535 -9.422143  Inf
#> brandweight  -Inf -14.3329366  -8.178094  -8.184692 -2.021520  Inf
#> brandyoplait -Inf  -2.4091027   2.491724   2.485490  7.394009  Inf
```

If you want to compare the WTP from the two different model spaces, use
the
[`wtpCompare()`](https://jhelvy.github.io/logitr/reference/wtpCompare.md)
function:

``` r
wtpCompare(mxl_pref, mxl_wtp, scalePar = 'price')
#>                          pref           wtp  difference
#> scalePar            0.4483378     0.4486637  0.00032586
#> feat                1.7330459     1.7315938 -0.00145218
#> brandhiland       -14.2021477   -14.2231313 -0.02098355
#> brandweight        -8.1828533    -8.1706054  0.01224797
#> brandyoplait        2.5036744     2.5041696  0.00049521
#> sd_feat             1.2657757     1.2666434  0.00086772
#> sd_brandhiland     -7.0969786    -7.1142376 -0.01725899
#> sd_brandweight      9.1384874     9.1293151 -0.00917230
#> sd_brandyoplait     7.2741604     7.2693641 -0.00479629
#> logLik          -1239.2944250 -1239.2939753  0.00044974
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
#> Model estimated on: Wed Sep 27 08:38:25 2023 
#> 
#> Using logitr version: 1.1.1 
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
#> 1       -1249.587         47           3
#> 2       -1254.922         52           3
#> 3       -1237.322         50           3
#> 4       -1279.337         59           3
#> 5       -1232.389        127           3
#> 6       -1237.453         63           3
#> 7       -1237.589         59           3
#> 8       -1249.725         61           3
#> 9       -1254.679         95           3
#> 10      -1240.966         67           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                              
#> Model Type:       Mixed Logit
#> Model Space:       Preference
#> Model Run:            5 of 10
#> Iterations:               127
#> Elapsed Time:        0h:0m:4s
#> Algorithm:     NLOPT_LD_LBFGS
#> Weights Used?:          FALSE
#> Panel ID:                  id
#> Robust?                 FALSE
#> 
#> Model Coefficients: 
#>                               Estimate Std. Error  z-value  Pr(>|z|)    
#> price                        -0.446859   0.038377 -11.6440 < 2.2e-16 ***
#> feat                          0.713972   0.229704   3.1082  0.001882 ** 
#> brandhiland                  -4.169377   0.443153  -9.4084 < 2.2e-16 ***
#> brandweight                  -2.066697   0.413684  -4.9958 5.858e-07 ***
#> brandyoplait                  1.922103   0.204530   9.3977 < 2.2e-16 ***
#> sd_feat_feat                 -0.178998   0.265060  -0.6753  0.499478    
#> sd_feat_brandhiland          -0.147720   0.214734  -0.6879  0.491502    
#> sd_feat_brandweight           0.286440   0.211904   1.3517  0.176457    
#> sd_feat_brandyoplait         -0.193657   0.285656  -0.6779  0.497810    
#> sd_brandhiland_brandhiland   -1.575305   0.230179  -6.8438 7.710e-12 ***
#> sd_brandhiland_brandweight    0.195483   0.281263   0.6950  0.487043    
#> sd_brandhiland_brandyoplait   1.049736   0.313859   3.3446  0.000824 ***
#> sd_brandweight_brandweight    3.765293   0.372558  10.1066 < 2.2e-16 ***
#> sd_brandweight_brandyoplait   2.231668   0.282024   7.9130 2.442e-15 ***
#> sd_brandyoplait_brandyoplait  3.383901   0.279962  12.0870 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -1232.3887491
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     2494.7774983
#> BIC:                     2581.6007000
#> McFadden R2:                0.6314343
#> Adj McFadden R2:            0.6269483
#> Number of Observations:  2412.0000000
#> 
#> Summary of 10k Draws for Random Coefficients: 
#>              Min.    1st Qu.     Median       Mean    3rd Qu. Max.
#> feat         -Inf  0.4329790  0.7138166  0.7144151  0.9944701  Inf
#> brandhiland  -Inf -5.4575869 -4.1609015 -4.1709150 -2.8929016  Inf
#> brandweight  -Inf -5.0015992 -2.0555055 -2.0782411  0.8907911  Inf
#> brandyoplait -Inf -0.3650334  1.9163092  1.9134074  4.1983305  Inf
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
Preference and Willingness-to-Pay Space.” In *Appl. Simul. Methods
Environ. Resour. Econ.*, 1–16.
