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

| Symbol | Variable |
|----|----|
| $`p`$ | The price in US dollars. |
| $`x_{j}^{\mathrm{Feat}}`$ | Dummy variable for whether the newspaper advertisement was shown to the customer. |
| $`x_{j}^{\mathrm{Hiland}}`$ | Dummy variable for the “Highland” brand. |
| $`x_{j}^{\mathrm{Weight}}`$ | Dummy variable for the “Weight Watchers” brand. |
| $`x_{j}^{\mathrm{Yoplait}}`$ | Dummy variable for the “Yoplait” brand. |

## Preference space model

This example will estimate the following homogeneous multinomial logit
model in the preference space:

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
and $`\beta_4`$ have units of utility.

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
#> Model estimated on: Fri Jul 10 17:56:32 2026 
#> 
#> Using logitr version: 1.2.0 
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
#> Iterations:                   24
#> Elapsed Time:        0h:0m:0.03s
#> Algorithm:        NLOPT_LD_LBFGS
#> Weights Used?:             FALSE
#> Robust?                    FALSE
#> 
#> Model Coefficients: 
#>               Estimate Std. Error  z-value  Pr(>|z|)    
#> price        -0.366584   0.024366 -15.0449 < 2.2e-16 ***
#> feat          0.491434   0.120063   4.0931 4.256e-05 ***
#> brandhiland  -3.715600   0.145419 -25.5510 < 2.2e-16 ***
#> brandweight  -0.641184   0.054498 -11.7652 < 2.2e-16 ***
#> brandyoplait  0.734571   0.080644   9.1088 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2656.8878779
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5323.7757559
#> BIC:                     5352.7168000
#> McFadden R2:                0.2054148
#> Adj McFadden R2:            0.2039195
#> Number of Observations:  2412.0000000
```

View the estimated model coefficients:

``` r

coef(mnl_pref)
#>        price         feat  brandhiland  brandweight brandyoplait 
#>   -0.3665845    0.4914335   -3.7156002   -0.6411843    0.7345712
```

Compute the WTP implied from the preference space model:

``` r

wtp_mnl_pref <- wtp(mnl_pref, scalePar =  "price")
wtp_mnl_pref
#>                Estimate Std. Error  z-value  Pr(>|z|)    
#> scalePar       0.366584   0.024389  15.0307 < 2.2e-16 ***
#> feat           1.340574   0.359042   3.7338 0.0001886 ***
#> brandhiland  -10.135727   0.585141 -17.3219 < 2.2e-16 ***
#> brandweight   -1.749077   0.181301  -9.6474 < 2.2e-16 ***
#> brandyoplait   2.003825   0.143049  14.0080 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
```

## WTP space model

This example will estimate the following homogeneous multinomial logit
model in the WTP space:

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
parameter.

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
#> Model estimated on: Fri Jul 10 17:56:33 2026 
#> 
#> Using logitr version: 1.2.0 
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
#> 1       -2656.888         90           3
#> 2       -2805.308         82           4
#> 3       -2656.888         40           4
#> 4       -2656.888         39           4
#> 5       -2656.888         55           3
#> 6       -2656.888         39           3
#> 7       -2656.888         39           3
#> 8       -2804.821         87           4
#> 9       -2656.888         49           3
#> 10      -2656.888         41           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                  
#> Model Type:     Multinomial Logit
#> Model Space:   Willingness-to-Pay
#> Model Run:                1 of 10
#> Iterations:                    90
#> Elapsed Time:         0h:0m:0.11s
#> Algorithm:         NLOPT_LD_LBFGS
#> Weights Used?:              FALSE
#> Robust?                     FALSE
#> 
#> Model Coefficients: 
#>                Estimate Std. Error  z-value  Pr(>|z|)    
#> scalePar       0.366584   0.024366  15.0449 < 2.2e-16 ***
#> feat           1.340574   0.355865   3.7671 0.0001652 ***
#> brandhiland  -10.135727   0.576083 -17.5942 < 2.2e-16 ***
#> brandweight   -1.749077   0.179897  -9.7227 < 2.2e-16 ***
#> brandyoplait   2.003825   0.142377  14.0741 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2656.8878779
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
#>    0.3665845    1.3405736  -10.1357273   -1.7490766    2.0038253
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
#> scalePar         0.3665845     0.3665845 -0.00000001
#> feat             1.3405737     1.3405736 -0.00000014
#> brandhiland    -10.1357272   -10.1357273 -0.00000009
#> brandweight     -1.7490765    -1.7490766 -0.00000003
#> brandyoplait     2.0038253     2.0038253  0.00000000
#> logLik       -2656.8878779 -2656.8878779  0.00000000
```

## References

Jain, Dipak C, Naufel J Vilcassim, and Pradeep K Chintagunta. 1994. “A
Random-Coefficients Logit Brand-Choice Model Applied to Panel Data.”
*Journal of Business & Economic Statistics* 12 (3): 317–28.
