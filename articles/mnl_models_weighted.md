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

| Symbol                   | Variable                                                      |
|--------------------------|---------------------------------------------------------------|
| $p$                      | The price in US dollars.                                      |
| $x_{j}^{hev}$            | Dummy variable for HEV vehicle type                           |
| $x_{j}^{phev10}$         | Dummy variable for PHEV10 vehicle type                        |
| $x_{j}^{phev20}$         | Dummy variable for PHEV20 vehicle type                        |
| $x_{j}^{phev40}$         | Dummy variable for PHEV40 vehicle type                        |
| $x_{j}^{bev75}$          | Dummy variable for BEV75 vehicle type                         |
| $x_{j}^{bev100}$         | Dummy variable for BEV100 vehicle type                        |
| $x_{j}^{bev150}$         | Dummy variable for BEV150 vehicle type                        |
| $x_{j}^{phevFastcharge}$ | Dummy variable for if the PHEV has a fast charging capability |
| $x_{j}^{bevFastcharge}$  | Dummy variable for if the BEV has a fast charging capability  |
| $x_{j}^{opCost}$         | The vehicle operating costs (cents / mile)                    |
| $x_{j}^{accelTime}$      | The vehicle 0-60mph acceleration time                         |
| $x_{j}^{american}$       | Dummy variable for an American brand                          |
| $x_{j}^{japanese}$       | Dummy variable for a Japanese brand                           |
| $x_{j}^{chinese}$        | Dummy variable for a Chinese brand                            |
| $x_{j}^{skorean}$        | Dummy variable for a S. Korean brand                          |

## The utility model

In this example, we’ll estimate two versions of the following utility
model in the WTP space: one without weights and one with weights.
Notation is taken from Helveston et al. (2015):

$$\begin{aligned}
 & {u_{j} = \lambda(} \\
 & {\omega_{1}x_{j}^{hev} + \omega_{2}x_{j}^{phev10} + \omega_{3}x_{j}^{phev20} + \omega_{4}x_{j}^{phev40} +} \\
 & {\omega_{5}x_{j}^{bev75} + \omega_{6}x_{j}^{bev100} + \omega_{7}x_{j}^{bev150} +} \\
 & {\omega_{8}x_{j}^{phevFastcharge} + \omega_{9}x_{j}^{bevFastcharge} + \omega_{10}x_{j}^{opCost} + \omega_{11}x_{j}^{accelTime} +} \\
 & {\omega_{12}x_{j}^{american} + \omega_{13}x_{j}^{japanese} + \omega_{14}x_{j}^{chinese} + \omega_{15}x_{j}^{skorean} - p_{j}} \\
 & {) + \varepsilon_{j}}
\end{aligned}$$

where all the $\omega$ parameters have units of dollars and $\lambda$ is
the scale parameter.

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
#> Model estimated on: Fri Dec 19 21:44:13 2025 
#> 
#> Using logitr version: 1.1.3 
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
#> 1       -4616.952         26           3
#> 2       -4616.952         32           3
#> 3       -4616.952         35           3
#> 4       -4616.952         31           3
#> 5       -4616.952         34           3
#> 6       -4616.952         42           3
#> 7       -4616.952         37           3
#> 8       -4616.952         33           3
#> 9       -4616.952         37           3
#> 10      -4616.952         31           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                  
#> Model Type:     Multinomial Logit
#> Model Space:   Willingness-to-Pay
#> Model Run:                4 of 10
#> Iterations:                    31
#> Elapsed Time:         0h:0m:0.39s
#> Algorithm:         NLOPT_LD_LBFGS
#> Weights Used?:              FALSE
#> Cluster ID:                obsnum
#> Robust?                      TRUE
#> 
#> Model Coefficients: 
#>                   Estimate  Std. Error  z-value  Pr(>|z|)    
#> scalePar         0.0738774   0.0021929  33.6897 < 2.2e-16 ***
#> hev              0.8067157   0.9990727   0.8075 0.4193989    
#> phev10           1.1661239   1.0615105   1.0986 0.2719638    
#> phev20           1.6479612   1.0617569   1.5521 0.1206364    
#> phev40           2.5799061   1.0499398   2.4572 0.0140027 *  
#> bev75          -16.0466139   1.2541622 -12.7947 < 2.2e-16 ***
#> bev100         -13.0036334   1.2388778 -10.4963 < 2.2e-16 ***
#> bev150          -9.5739892   1.1642037  -8.2236 2.220e-16 ***
#> american         2.3440498   0.7979799   2.9375 0.0033089 ** 
#> japanese        -0.3747159   0.7998412  -0.4685 0.6394358    
#> chinese        -10.2689884   0.8859572 -11.5908 < 2.2e-16 ***
#> skorean         -6.0310531   0.8514411  -7.0833 1.407e-12 ***
#> phevFastcharge   2.8788982   0.8028866   3.5857 0.0003362 ***
#> bevFastcharge    2.9189842   0.9181490   3.1792 0.0014768 ** 
#> opCost          -1.6360751   0.0686328 -23.8381 < 2.2e-16 ***
#> accelTime       -1.6970516   0.1638115 -10.3598 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -4616.9517792
#> Null Log-Likelihood:    -6328.0067827
#> AIC:                     9265.9035584
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
#> Model estimated on: Fri Dec 19 21:44:17 2025 
#> 
#> Using logitr version: 1.1.3 
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
#> 1       -3425.633         19           3
#> 2       -3425.630         35           3
#> 3       -3425.630         32           3
#> 4       -3425.630         36           3
#> 5       -3425.630         30           3
#> 6       -3425.630         36           3
#> 7       -3425.630         38           3
#> 8       -3425.630         32           3
#> 9       -3425.630         36           3
#> 10      -3425.630         32           3
#> 
#> Use statusCodes() to view the meaning of each status code
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                  
#> Model Type:     Multinomial Logit
#> Model Space:   Willingness-to-Pay
#> Model Run:                8 of 10
#> Iterations:                    32
#> Elapsed Time:         0h:0m:0.36s
#> Algorithm:         NLOPT_LD_LBFGS
#> Weights Used?:               TRUE
#> Cluster ID:                obsnum
#> Robust?                      TRUE
#> 
#> Model Coefficients: 
#>                   Estimate  Std. Error z-value  Pr(>|z|)    
#> scalePar         0.0522816   0.0040689 12.8490 < 2.2e-16 ***
#> hev             -1.1767755   2.9132608 -0.4039 0.6862586    
#> phev10           0.0259001   3.1279545  0.0083 0.9933934    
#> phev20           1.6936471   3.0996513  0.5464 0.5847915    
#> phev40           2.6493078   2.9851129  0.8875 0.3748061    
#> bev75          -20.1364665   3.6670723 -5.4912 3.993e-08 ***
#> bev100         -19.4956474   3.6254343 -5.3775 7.554e-08 ***
#> bev150         -13.6901824   3.4925299 -3.9198 8.861e-05 ***
#> american         8.1856838   2.4051540  3.4034 0.0006655 ***
#> japanese         0.9329586   2.3602742  0.3953 0.6926396    
#> chinese        -19.0092581   2.8541735 -6.6602 2.735e-11 ***
#> skorean         -9.5128579   2.5235142 -3.7697 0.0001635 ***
#> phevFastcharge   3.9425031   2.3623349  1.6689 0.0951370 .  
#> bevFastcharge    3.3425219   2.8085873  1.1901 0.2340040    
#> opCost          -1.5975391   0.1948443 -8.1991 2.220e-16 ***
#> accelTime       -1.1716178   0.4834534 -2.4234 0.0153745 *  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -3425.6302899
#> Null Log-Likelihood:    -4360.5909275
#> AIC:                     6883.2605798
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
#>                 Unweighted     Weighted
#> scalePar         0.0738774   0.05228164
#> hev              0.8067157  -1.17677549
#> phev10           1.1661239   0.02590006
#> phev20           1.6479612   1.69364709
#> phev40           2.5799061   2.64930778
#> bev75          -16.0466139 -20.13646650
#> bev100         -13.0036334 -19.49564743
#> bev150          -9.5739892 -13.69018245
#> american         2.3440498   8.18568382
#> japanese        -0.3747159   0.93295864
#> chinese        -10.2689884 -19.00925808
#> skorean         -6.0310531  -9.51285786
#> phevFastcharge   2.8788982   3.94250307
#> bevFastcharge    2.9189842   3.34252186
#> opCost          -1.6360751  -1.59753905
#> accelTime       -1.6970516  -1.17161781
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
