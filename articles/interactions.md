# Estimating Models with Interactions

## Interactions with continuous variables

To add interactions between covariates in your model, you can add
additional arguments in the `pars` vector in the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function separated by the `*` symbol. For example, let’s say we want to
interact `price` with `feat` in the following model:

``` r

library("logitr")

model <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand')
)
```

To do so, I could add `"price*feat"` to the `pars` vector:

``` r

model_price_feat <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand', 'price*feat')
)
```

The model now has an estimated coefficient for the `price*feat` effect:

``` r

summary(model_price_feat)
#> =================================================
#> 
#> Model estimated on: Mon Jul 06 10:20:28 2026 
#> 
#> Using logitr version: 1.2.0 
#> 
#> Call:
#> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("price", 
#>     "feat", "brand", "price*feat"))
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
#> Iterations:                   28
#> Elapsed Time:        0h:0m:0.03s
#> Algorithm:        NLOPT_LD_LBFGS
#> Weights Used?:             FALSE
#> Robust?                    FALSE
#> 
#> Model Coefficients: 
#>               Estimate Std. Error  z-value  Pr(>|z|)    
#> price        -0.358469   0.024733 -14.4936 < 2.2e-16 ***
#> feat          1.090155   0.378956   2.8767  0.004018 ** 
#> brandhiland  -3.725143   0.146424 -25.4407 < 2.2e-16 ***
#> brandweight  -0.640131   0.054541 -11.7368 < 2.2e-16 ***
#> brandyoplait  0.727363   0.080410   9.0457 < 2.2e-16 ***
#> price:feat   -0.078128   0.047287  -1.6522  0.098491 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2655.5245489
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5323.0490978
#> BIC:                     5357.7784000
#> McFadden R2:                0.2058225
#> Adj McFadden R2:            0.2040281
#> Number of Observations:  2412.0000000
```

## Interactions with discrete variables

In the above example, both `price` and `feat` were continuous variables,
so only a single interaction coefficient was needed.

In the case of interacting *discrete* variables, multiple interactions
coefficients will be estimated according to the number of levels in the
discrete attribute. For example, the interaction of `price` with `brand`
will require three new interactions - one for each level of the `brand`
variable except the first reference level:

``` r

model_price_brand <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand', 'price*brand')
)
```

The model now has three estimated coefficients for the `price*brand`
effect:

``` r

summary(model_price_brand)
#> =================================================
#> 
#> Model estimated on: Mon Jul 06 10:20:28 2026 
#> 
#> Using logitr version: 1.2.0 
#> 
#> Call:
#> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("price", 
#>     "feat", "brand", "price*brand"))
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
#> Iterations:                   47
#> Elapsed Time:        0h:0m:0.05s
#> Algorithm:        NLOPT_LD_LBFGS
#> Weights Used?:             FALSE
#> Robust?                    FALSE
#> 
#> Model Coefficients: 
#>                     Estimate Std. Error z-value  Pr(>|z|)    
#> price              -0.389508   0.045247 -8.6086 < 2.2e-16 ***
#> feat                0.421869   0.122578  3.4416 0.0005782 ***
#> brandhiland        -1.691734   0.623149 -2.7148 0.0066313 ** 
#> brandweight        -2.228264   0.561636 -3.9675 7.265e-05 ***
#> brandyoplait        0.438708   0.450425  0.9740 0.3300636    
#> price:brandhiland  -0.434287   0.115531 -3.7590 0.0001706 ***
#> price:brandweight   0.199893   0.069829  2.8626 0.0042019 ** 
#> price:brandyoplait  0.033236   0.050372  0.6598 0.5093835    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2643.2046773
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5302.4093546
#> BIC:                     5348.7150000
#> McFadden R2:                0.2095070
#> Adj McFadden R2:            0.2071145
#> Number of Observations:  2412.0000000
```

## Interactions with individual-specific variables

If you want to include interactions with individual-specific variables
(for example, to assess the difference in an effect between groups of
respondents), you should **not** include the individual-specific
variable interactions using `*` in `pars`. This is because interactions
inside `pars` automatically generate the interaction coefficient as well
as coefficients for each covariate.

For example, if you had a `group` variable that determined whether
individuals belongs to group `A` or group `B`, including `price*group`
in `pars` would create coefficients for `price`, `groupA`, and
`price:groupA`, but the `groupA` coefficient would be unidentified. In
this case, you should only include `price` and `price:groupA` in the
model. For now, the only way to handle this situation is to manually
create dummy-coded interaction variables to include in the model.

To illustrate how one might do this, consider if the `yogurt` data frame
had two groups of individuals: `A` and `B`. For simple illustration,
I’ll define these groups arbitrarily based on whether or not the `obsID`
is even or odd:

``` r

# Create group A dummies
yogurt$groupA <- ifelse(yogurt$obsID %% 2 == 0, 1, 0)
```

An interaction between the `group` variable and `price` can be included
in the model by first manually creating a `price_groupA` interaction
variable and then including it in `pars`:

``` r

# Create dummy coefficients for group interaction with price
yogurt$price_groupA <- yogurt$price*yogurt$groupA

model_price_group <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand', 'price_groupA')
)
```

The model now has attribute coefficients for `price`, `feat`, and
`brand` as well as an interaction between the `group` and `price`:

``` r

summary(model_price_group)
#> =================================================
#> 
#> Model estimated on: Mon Jul 06 10:20:29 2026 
#> 
#> Using logitr version: 1.2.0 
#> 
#> Call:
#> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("price", 
#>     "feat", "brand", "price_groupA"))
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
#> Iterations:                   28
#> Elapsed Time:        0h:0m:0.03s
#> Algorithm:        NLOPT_LD_LBFGS
#> Weights Used?:             FALSE
#> Robust?                    FALSE
#> 
#> Model Coefficients: 
#>                Estimate Std. Error  z-value  Pr(>|z|)    
#> price        -0.3680606  0.0273909 -13.4373 < 2.2e-16 ***
#> feat          0.4915364  0.1200722   4.0937 4.246e-05 ***
#> brandhiland  -3.7154892  0.1454204 -25.5500 < 2.2e-16 ***
#> brandweight  -0.6411357  0.0544998 -11.7640 < 2.2e-16 ***
#> brandyoplait  0.7345308  0.0806438   9.1083 < 2.2e-16 ***
#> price_groupA  0.0030067  0.0254483   0.1182    0.9059    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2656.8808981
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5325.7617963
#> BIC:                     5360.4911000
#> McFadden R2:                0.2054169
#> Adj McFadden R2:            0.2036225
#> Number of Observations:  2412.0000000
```

## Interactions in mixed logit models

Suppose I want to include an interaction between two variables and I
also want one of those variables to be modeled as normally distributed
across the population. The example below illustrates this cases, where a
`price*feat` interaction is specified and the `feat` parameter is
modeled as normally distributed by setting `randPars = c(feat = "n")`:

``` r

model_price_feat_mxl <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand', 'price*feat'),
  randPars = c(feat = "n")
)
```

In this case, the `price*feat` interaction parameter is interpreted as a
difference in the `feat_mu` parameter and price; that is, it an
interaction in the *mean* `feat` parameter and `price`:

``` r

summary(model_price_feat_mxl)
#> =================================================
#> 
#> Model estimated on: Mon Jul 06 10:20:29 2026 
#> 
#> Using logitr version: 1.2.0 
#> 
#> Call:
#> logitr(data = yogurt, outcome = "choice", obsID = "obsID", pars = c("price", 
#>     "feat", "brand", "price*feat"), randPars = c(feat = "n"))
#> 
#> Frequencies of alternatives:
#>        1        2        3        4 
#> 0.402156 0.029436 0.229270 0.339138 
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                              
#> Model Type:       Mixed Logit
#> Model Space:       Preference
#> Model Run:             1 of 1
#> Iterations:                41
#> Elapsed Time:        0h:0m:1s
#> Algorithm:     NLOPT_LD_LBFGS
#> Weights Used?:          FALSE
#> Robust?                 FALSE
#> 
#> Model Coefficients: 
#>               Estimate Std. Error  z-value  Pr(>|z|)    
#> price        -0.387921   0.027020 -14.3570 < 2.2e-16 ***
#> feat          0.925402   0.551395   1.6783   0.09329 .  
#> brandhiland  -3.990058   0.165873 -24.0549 < 2.2e-16 ***
#> brandweight  -0.662270   0.055784 -11.8721 < 2.2e-16 ***
#> brandyoplait  0.787062   0.086223   9.1282 < 2.2e-16 ***
#> price:feat   -0.077777   0.070896  -1.0971   0.27262    
#> sd_feat       2.263118   0.499054   4.5348 5.765e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2645.3108884
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5304.6217768
#> BIC:                     5345.1393000
#> McFadden R2:                0.2088771
#> Adj McFadden R2:            0.2067836
#> Number of Observations:  2412.0000000
#> 
#> Summary of 10k Draws for Random Coefficients: 
#>      Min.   1st Qu.    Median      Mean  3rd Qu. Max.
#> feat -Inf -0.602586 0.9236728 0.9222336 2.449411  Inf
```
