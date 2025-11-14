# Estimating Multinomial Logit Models

This vignette demonstrates an example of how to use the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function to estimate multinomial logit (MNL) models with preference
space and WTP space utility parameterizations.

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

This example will estimate the following homogeneous multinomial logit
model in the preference space:

$$u_{j} = \alpha p_{j} + \beta_{1}x_{j}^{Feat} + \beta_{2}x_{j}^{Hiland} + \beta_{3}x_{j}^{Weight} + \beta_{4}x_{j}^{Yoplait} + \varepsilon_{j}$$

where the parameters $\alpha$, $\beta_{1}$, $\beta_{2}$, $\beta_{3}$,
and $\beta_{4}$ have units of utility.

Estimate the model using the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function:

``` r
library("logitr")

mnl_pref <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand')
)
```

Print a summary of the results:

``` r
summary(mnl_pref)
#> =================================================
#> 
#> Model estimated on: Fri Nov 14 17:02:26 2025 
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

View the estimated model coefficients:

``` r
coef(mnl_pref)
#>        price         feat  brandhiland  brandweight brandyoplait 
#>   -0.3665546    0.4914392   -3.7154773   -0.6411384    0.7345195
```

Compute the WTP implied from the preference space model:

``` r
wtp_mnl_pref <- wtp(mnl_pref, scalePar =  "price")
wtp_mnl_pref
#>                Estimate Std. Error  z-value  Pr(>|z|)    
#> scalePar       0.366555   0.024388  15.0299 < 2.2e-16 ***
#> feat           1.340699   0.359070   3.7338 0.0001886 ***
#> brandhiland  -10.136219   0.585204 -17.3208 < 2.2e-16 ***
#> brandweight   -1.749094   0.181314  -9.6468 < 2.2e-16 ***
#> brandyoplait   2.003848   0.143059  14.0071 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## WTP space model

This example will estimate the following homogeneous multinomial logit
model in the WTP space:

$$u_{j} = \lambda\left( \omega_{1}x_{j}^{Feat} + \omega_{2}x_{j}^{Hiland} + \omega_{3}x_{j}^{Weight} + \omega_{4}x_{j}^{Yoplait} - p_{j} \right) + \varepsilon_{j}$$

where the parameters $\omega_{1}$, $\omega_{2}$, $\omega_{3}$, and
$\omega_{4}$ have units of dollars and $\lambda$ is the scale parameter.

Estimate the model using the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function:

``` r
mnl_wtp <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('feat', 'brand'),
  scalePar = 'price',
  # Since WTP space models are non-convex, run a multistart
  numMultiStarts = 10,
  # Use the computed WTP from the preference space model as the starting
  # values for the first run:
  startVals = wtp_mnl_pref$Estimate
)
```

Print a summary of the results:

``` r
summary(mnl_wtp)
#> =================================================
#> 
#> Model estimated on: Fri Nov 14 17:02:27 2025 
#> 
#> Using logitr version: 1.1.3 
#> 
#> Call:
#> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("feat", 
#>     "brand"), scalePar = "price", startVals = wtp_mnl_pref$Estimate, 
#>     numMultiStarts = 10)
#> 
#> Frequencies of alternatives:
#>        1        2        3        4 
#> 0.402156 0.029436 0.229270 0.339138 
#> 
#> Summary Of Multistart Runs:
#>    Log Likelihood Iterations Exit Status
#> 1       -2656.888         84           3
#> 2       -2805.308         82           4
#> 3       -2656.888         35           3
#> 4       -2656.888         36           3
#> 5       -2656.888         51           3
#> 6       -2656.888         37           3
#> 7       -2656.888         36           3
#> 8       -2804.821         87           4
#> 9       -2656.888         45           3
#> 10      -2656.888         39           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                  
#> Model Type:     Multinomial Logit
#> Model Space:   Willingness-to-Pay
#> Model Run:               10 of 10
#> Iterations:                    39
#> Elapsed Time:         0h:0m:0.04s
#> Algorithm:         NLOPT_LD_LBFGS
#> Weights Used?:              FALSE
#> Robust?                     FALSE
#> 
#> Model Coefficients: 
#>                Estimate Std. Error  z-value  Pr(>|z|)    
#> scalePar       0.366584   0.024366  15.0449 < 2.2e-16 ***
#> feat           1.340573   0.355865   3.7671 0.0001652 ***
#> brandhiland  -10.135757   0.576088 -17.5941 < 2.2e-16 ***
#> brandweight   -1.749053   0.179895  -9.7226 < 2.2e-16 ***
#> brandyoplait   2.003839   0.142376  14.0742 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2656.8878780
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5323.7757559
#> BIC:                     5352.7168000
#> McFadden R2:                0.2054148
#> Adj McFadden R2:            0.2039195
#> Number of Observations:  2412.0000000
```

View the estimated model coefficients:

``` r
coef(mnl_wtp)
#>     scalePar         feat  brandhiland  brandweight brandyoplait 
#>    0.3665841    1.3405728  -10.1357568   -1.7490533    2.0038390
```

## Compare WTP from both models

Since WTP space models are non-convex, you cannot be certain that the
model reached a global solution, even when using a multi-start. However,
homogeneous models in the *preference* space are convex, so you are
guaranteed to find the global solution in that space. Therefore, it can
be useful to compute the WTP from the preference space model and compare
it against the WTP from the WTP space model. If the WTP values and
log-likelihood values from the two model spaces are equal, then the WTP
space model is likely at a global solution.

To compare the WTP and log-likelihood values between the preference
space and WTP space models, use the
[`wtpCompare()`](https://jhelvy.github.io/logitr/reference/wtpCompare.md)
function:

``` r
wtpCompare(mnl_pref, mnl_wtp, scalePar = 'price')
#>                       pref           wtp  difference
#> scalePar         0.3665546     0.3665841  0.00002954
#> feat             1.3406987     1.3405728 -0.00012592
#> brandhiland    -10.1362190   -10.1357568  0.00046217
#> brandweight     -1.7490940    -1.7490533  0.00004064
#> brandyoplait     2.0038476     2.0038390 -0.00000866
#> logLik       -2656.8878790 -2656.8878780  0.00000105
```

## References

Jain, Dipak C, Naufel J Vilcassim, and Pradeep K Chintagunta. 1994. “A
Random-Coefficients Logit Brand-Choice Model Applied to Panel Data.”
*Journal of Business & Economic Statistics* 12 (3): 317–28.
