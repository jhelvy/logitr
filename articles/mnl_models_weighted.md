# Estimating Weighted Logit Models

This vignette demonstrates an example of how to use the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function with the `weights` argument to estimate weighted logit models.

## The data

This example uses the
[cars_us](https://jhelvy.github.io/logitr/reference/cars_us.html) data
set from Helveston et al. (2015) containing 384 stated choice
observations from US car buyers. Conjoint surveys were fielded in 2012
online in the US on Amazon Mechanical Turk and in person at the 2013
Pittsburgh Auto show. Participants were asked to select a vehicle from a
set of three alternatives. Each participant answered 15 choice
questions.

In the utility models described below, the data variables are
represented as follows:

| Symbol | Variable |
|----|----|
| $`p`$ | The price in US dollars. |
| $`x_{j}^{\mathrm{hev}}`$ | Dummy variable for HEV vehicle type |
| $`x_{j}^{\mathrm{phev10}}`$ | Dummy variable for PHEV10 vehicle type |
| $`x_{j}^{\mathrm{phev20}}`$ | Dummy variable for PHEV20 vehicle type |
| $`x_{j}^{\mathrm{phev40}}`$ | Dummy variable for PHEV40 vehicle type |
| $`x_{j}^{\mathrm{bev75}}`$ | Dummy variable for BEV75 vehicle type |
| $`x_{j}^{\mathrm{bev100}}`$ | Dummy variable for BEV100 vehicle type |
| $`x_{j}^{\mathrm{bev150}}`$ | Dummy variable for BEV150 vehicle type |
| $`x_{j}^{\mathrm{phevFastcharge}}`$ | Dummy variable for if the PHEV has a fast charging capability |
| $`x_{j}^{\mathrm{bevFastcharge}}`$ | Dummy variable for if the BEV has a fast charging capability |
| $`x_{j}^{\mathrm{opCost}}`$ | The vehicle operating costs (cents / mile) |
| $`x_{j}^{\mathrm{accelTime}}`$ | The vehicle 0-60mph acceleration time |
| $`x_{j}^{\mathrm{american}}`$ | Dummy variable for an American brand |
| $`x_{j}^{\mathrm{japanese}}`$ | Dummy variable for a Japanese brand |
| $`x_{j}^{\mathrm{chinese}}`$ | Dummy variable for a Chinese brand |
| $`x_{j}^{\mathrm{skorean}}`$ | Dummy variable for a S. Korean brand |

## The utility model

In this example, we’ll estimate two versions of the following utility
model in the WTP space: one without weights and one with weights.
Notation is taken from Helveston et al. (2015):

``` math
\begin{equation}
\begin{split}
&u_{j} = \lambda (\\
&\omega_1 x_{j}^{\mathrm{hev}} +
 \omega_2 x_{j}^{\mathrm{phev10}} +
 \omega_3 x_{j}^{\mathrm{phev20}} +
 \omega_4 x_{j}^{\mathrm{phev40}} +\\
&\omega_5 x_{j}^{\mathrm{bev75}} +
 \omega_6 x_{j}^{\mathrm{bev100}} +
 \omega_7 x_{j}^{\mathrm{bev150}} +\\
&\omega_8 x_{j}^{\mathrm{phevFastcharge}} +
 \omega_9 x_{j}^{\mathrm{bevFastcharge}} +
 \omega_{10} x_{j}^{\mathrm{opCost}} +
 \omega_{11} x_{j}^{\mathrm{accelTime}} +\\
&\omega_{12} x_{j}^{\mathrm{american}} +
 \omega_{13} x_{j}^{\mathrm{japanese}} +
 \omega_{14} x_{j}^{\mathrm{chinese}} +
 \omega_{15} x_{j}^{\mathrm{skorean}} - p_{j}\\
&) +\varepsilon_{j}
\end{split}
\label{eq:mnlWtpCarsExample}
\end{equation}
```

where all the $`\omega`$ parameters have units of dollars and
$`\lambda`$ is the scale parameter.

## Unweighted model

Estimate the unweighted model using the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function. In this example, I have set `robust = TRUE` since it will also
be `TRUE` in the weighted model:

``` r

library("logitr")

mnl_wtp_unweighted <- logitr(
  data    = cars_us,
  outcome = 'choice',
  obsID   = 'obsnum',
  pars    = c(
    'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  scalePar   = 'price',
  robust     = TRUE,
  # Since WTP space models are non-convex, run a multistart
  numMultiStarts = 10
)
```

Print a summary of the results:

``` r

summary(mnl_wtp_unweighted)
#> =================================================
#> 
#> Model estimated on: Mon Jul 06 10:20:33 2026 
#> 
#> Using logitr version: 1.2.0 
#> 
#> Call:
#> logitr(data = cars_us, outcome = "choice", obsID = "obsnum", 
#>     pars = c("hev", "phev10", "phev20", "phev40", "bev75", "bev100", 
#>         "bev150", "american", "japanese", "chinese", "skorean", 
#>         "phevFastcharge", "bevFastcharge", "opCost", "accelTime"), 
#>     scalePar = "price", robust = TRUE, numMultiStarts = 10)
#> 
#> Frequencies of alternatives:
#>       1       2       3 
#> 0.34323 0.33507 0.32170 
#> 
#> Summary Of Multistart Runs:
#>    Log Likelihood Iterations Exit Status
#> 1       -4616.952         30           3
#> 2       -4616.952         37           3
#> 3       -4616.952         38           3
#> 4       -4616.952         33           3
#> 5       -4616.952         37           3
#> 6       -4616.952         47           3
#> 7       -4616.952         43           3
#> 8       -4616.952         38           3
#> 9       -4616.952         44           3
#> 10      -4616.952         36           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                  
#> Model Type:     Multinomial Logit
#> Model Space:   Willingness-to-Pay
#> Model Run:                7 of 10
#> Iterations:                    43
#> Elapsed Time:         0h:0m:0.21s
#> Algorithm:         NLOPT_LD_LBFGS
#> Weights Used?:              FALSE
#> Cluster ID:                obsnum
#> Robust?                      TRUE
#> 
#> Model Coefficients: 
#>                   Estimate  Std. Error  z-value  Pr(>|z|)    
#> scalePar         0.0738776   0.0021929  33.6898 < 2.2e-16 ***
#> hev              0.8068383   0.9990699   0.8076 0.4193270    
#> phev10           1.1660579   1.0615084   1.0985 0.2719899    
#> phev20           1.6478767   1.0617545   1.5520 0.1206546    
#> phev40           2.5798021   1.0499375   2.4571 0.0140063 *  
#> bev75          -16.0465591   1.2541584 -12.7947 < 2.2e-16 ***
#> bev100         -13.0036932   1.2388776 -10.4963 < 2.2e-16 ***
#> bev150          -9.5739135   1.1641998  -8.2236 2.220e-16 ***
#> american         2.3440841   0.7979780   2.9375 0.0033084 ** 
#> japanese        -0.3746762   0.7998393  -0.4684 0.6394705    
#> chinese        -10.2689079   0.8859536 -11.5908 < 2.2e-16 ***
#> skorean         -6.0310407   0.8514392  -7.0833 1.407e-12 ***
#> phevFastcharge   2.8790888   0.8028861   3.5859 0.0003359 ***
#> bevFastcharge    2.9190768   0.9181469   3.1793 0.0014762 ** 
#> opCost          -1.6360677   0.0686324 -23.8381 < 2.2e-16 ***
#> accelTime       -1.6970474   0.1638110 -10.3598 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -4616.9517791
#> Null Log-Likelihood:    -6328.0067827
#> AIC:                     9265.9035583
#> BIC:                     9372.4426000
#> McFadden R2:                0.2703940
#> Adj McFadden R2:            0.2678655
#> Number of Observations:  5760.0000000
#> Number of Clusters       5760.0000000
```

## Weighted model

To estimate the weighted model, simply add the `weights` argument to the
call to
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md),
referring to the column of weights that will be used to weight each
choice observation. In this example, the weights used in the `weights`
column range from 0.2 to 5:

``` r

summary(cars_us$weights)
#>    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#>  0.2000  0.2000  0.2000  0.6891  0.2000  5.0000

mnl_wtp_weighted <- logitr(
  data    = cars_us,
  outcome = 'choice',
  obsID   = 'obsnum',
  pars    = c(
    'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  scalePar = 'price',
  weights  = 'weights', # This enables the weights
  robust   = TRUE,
  numMultiStarts = 10
)
```

Print a summary of the results:

``` r

summary(mnl_wtp_weighted)
#> =================================================
#> 
#> Model estimated on: Mon Jul 06 10:20:36 2026 
#> 
#> Using logitr version: 1.2.0 
#> 
#> Call:
#> logitr(data = cars_us, outcome = "choice", obsID = "obsnum", 
#>     pars = c("hev", "phev10", "phev20", "phev40", "bev75", "bev100", 
#>         "bev150", "american", "japanese", "chinese", "skorean", 
#>         "phevFastcharge", "bevFastcharge", "opCost", "accelTime"), 
#>     scalePar = "price", weights = "weights", robust = TRUE, numMultiStarts = 10)
#> 
#> Frequencies of alternatives:
#>       1       2       3 
#> 0.34323 0.33507 0.32170 
#> 
#> Summary Of Multistart Runs:
#>    Log Likelihood Iterations Exit Status
#> 1        -3425.63         27           3
#> 2        -3425.63         40           3
#> 3        -3425.63         37           3
#> 4        -3425.63         35           3
#> 5        -3425.63         40           3
#> 6        -3425.63         32           3
#> 7        -3425.63         37           3
#> 8        -3425.63         41           3
#> 9        -3425.63         36           3
#> 10       -3425.63         41           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                  
#> Model Type:     Multinomial Logit
#> Model Space:   Willingness-to-Pay
#> Model Run:                4 of 10
#> Iterations:                    35
#> Elapsed Time:         0h:0m:0.19s
#> Algorithm:         NLOPT_LD_LBFGS
#> Weights Used?:               TRUE
#> Cluster ID:                obsnum
#> Robust?                      TRUE
#> 
#> Model Coefficients: 
#>                  Estimate Std. Error z-value  Pr(>|z|)    
#> scalePar         0.052281   0.004069 12.8485 < 2.2e-16 ***
#> hev             -1.176208   2.913308 -0.4037  0.686407    
#> phev10           0.027225   3.128004  0.0087  0.993056    
#> phev20           1.694666   3.099700  0.5467  0.584572    
#> phev40           2.649552   2.985163  0.8876  0.374770    
#> bev75          -20.136491   3.667164 -5.4910 3.996e-08 ***
#> bev100         -19.496439   3.625584 -5.3775 7.554e-08 ***
#> bev150         -13.691135   3.492686 -3.9199 8.857e-05 ***
#> american         8.187709   2.405291  3.4040  0.000664 ***
#> japanese         0.934266   2.360347  0.3958  0.692240    
#> chinese        -19.007600   2.854081 -6.6598 2.742e-11 ***
#> skorean         -9.510321   2.523429 -3.7688  0.000164 ***
#> phevFastcharge   3.943902   2.362412  1.6694  0.095030 .  
#> bevFastcharge    3.342681   2.808675  1.1901  0.233996    
#> opCost          -1.597548   0.194850 -8.1989 2.220e-16 ***
#> accelTime       -1.171821   0.483469 -2.4238  0.015360 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -3425.6302845
#> Null Log-Likelihood:    -4360.5909275
#> AIC:                     6883.2605689
#> BIC:                     6989.7997000
#> McFadden R2:                0.2144115
#> Adj McFadden R2:            0.2107422
#> Number of Observations:  5760.0000000
#> Number of Clusters       5760.0000000
```

## Compare results

Here is a comparison of the coefficients between the weighted and
unweighted models. All of the significant coefficients have the same
sign, but the magnitudes shift some based on the differential weighting
of each individual choice in the weighted model:

``` r

data.frame(
  Unweighted = coef(mnl_wtp_unweighted),
  Weighted   = coef(mnl_wtp_weighted)
)
#>                  Unweighted     Weighted
#> scalePar         0.07387763   0.05228073
#> hev              0.80683832  -1.17620775
#> phev10           1.16605795   0.02722490
#> phev20           1.64787665   1.69466611
#> phev40           2.57980206   2.64955183
#> bev75          -16.04655907 -20.13649110
#> bev100         -13.00369324 -19.49643889
#> bev150          -9.57391351 -13.69113497
#> american         2.34408413   8.18770943
#> japanese        -0.37467619   0.93426585
#> chinese        -10.26890786 -19.00759982
#> skorean         -6.03104065  -9.51032141
#> phevFastcharge   2.87908878   3.94390242
#> bevFastcharge    2.91907681   3.34268056
#> opCost          -1.63606769  -1.59754825
#> accelTime       -1.69704741  -1.17182092
```

Here is a comparison of the log-likelihood for the weighted and
unweighted models:

``` r

c(
  "Unweighted" = mnl_wtp_unweighted$logLik,
  "Weighted" = mnl_wtp_weighted$logLik
)
#> Unweighted   Weighted 
#>  -4616.952  -3425.630
```

## References

Helveston, John Paul, Yimin Liu, Eleanor Mcdonnell Feit, Erica R. H.
Fuchs, Erica Klampfl, and Jeremy J. Michalek. 2015. “Will Subsidies
Drive Electric Vehicle Adoption? Measuring Consumer Preferences in the
U.S. and China.” *Transportation Research Part A: Policy and Practice*
73: 96–112. <https://doi.org/10.1016/j.tra.2015.01.002>.
