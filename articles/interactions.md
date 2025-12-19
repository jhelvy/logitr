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
#> Model estimated on: Fri Dec 19 21:44:08 2025 
#> 
#> Using logitr version: 1.1.3 
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
#> Iterations:                   19
#> Elapsed Time:        0h:0m:0.02s
#> Algorithm:        NLOPT_LD_LBFGS
#> Weights Used?:             FALSE
#> Robust?                    FALSE
#> 
#> Model Coefficients: 
#>               Estimate Std. Error  z-value  Pr(>|z|)    
#> price        -0.356909   0.024696 -14.4522 < 2.2e-16 ***
#> feat          1.155206   0.378237   3.0542  0.002257 ** 
#> brandhiland  -3.724702   0.146520 -25.4212 < 2.2e-16 ***
#> brandweight  -0.640221   0.054543 -11.7380 < 2.2e-16 ***
#> brandyoplait  0.724315   0.080317   9.0182 < 2.2e-16 ***
#> price:feat   -0.086381   0.047275  -1.8272  0.067672 .  
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2655.5403770
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5323.0807540
#> BIC:                     5357.8100000
#> McFadden R2:                0.2058178
#> Adj McFadden R2:            0.2040234
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
#> Model estimated on: Fri Dec 19 21:44:09 2025 
#> 
#> Using logitr version: 1.1.3 
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
#> Iterations:                   42
#> Elapsed Time:        0h:0m:0.05s
#> Algorithm:        NLOPT_LD_LBFGS
#> Weights Used?:             FALSE
#> Robust?                    FALSE
#> 
#> Model Coefficients: 
#>                     Estimate Std. Error z-value  Pr(>|z|)    
#> price              -0.389813   0.045256 -8.6135 < 2.2e-16 ***
#> feat                0.422188   0.122588  3.4440 0.0005732 ***
#> brandhiland        -1.692896   0.623122 -2.7168 0.0065917 ** 
#> brandweight        -2.226187   0.561605 -3.9640 7.371e-05 ***
#> brandyoplait        0.441559   0.450562  0.9800 0.3270779    
#> price:brandhiland  -0.434214   0.115521 -3.7587 0.0001708 ***
#> price:brandweight   0.199624   0.069825  2.8589 0.0042512 ** 
#> price:brandyoplait  0.033040   0.050385  0.6558 0.5119777    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2643.2048908
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5302.4097816
#> BIC:                     5348.7155000
#> McFadden R2:                0.2095069
#> Adj McFadden R2:            0.2071144
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
#> Model estimated on: Fri Dec 19 21:44:09 2025 
#> 
#> Using logitr version: 1.1.3 
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
#> Iterations:                   26
#> Elapsed Time:        0h:0m:0.03s
#> Algorithm:        NLOPT_LD_LBFGS
#> Weights Used?:             FALSE
#> Robust?                    FALSE
#> 
#> Model Coefficients: 
#>                Estimate Std. Error  z-value  Pr(>|z|)    
#> price        -0.3680634  0.0273911 -13.4373 < 2.2e-16 ***
#> feat          0.4915271  0.1200725   4.0936 4.248e-05 ***
#> brandhiland  -3.7155231  0.1454216 -25.5500 < 2.2e-16 ***
#> brandweight  -0.6411384  0.0544999 -11.7640 < 2.2e-16 ***
#> brandyoplait  0.7345568  0.0806444   9.1086 < 2.2e-16 ***
#> price_groupA  0.0030007  0.0254484   0.1179    0.9061    
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2656.8808982
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5325.7617965
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
#> Model estimated on: Fri Dec 19 21:44:10 2025 
#> 
#> Using logitr version: 1.1.3 
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
#> Iterations:                34
#> Elapsed Time:        0h:0m:1s
#> Algorithm:     NLOPT_LD_LBFGS
#> Weights Used?:          FALSE
#> Robust?                 FALSE
#> 
#> Model Coefficients: 
#>               Estimate Std. Error  z-value  Pr(>|z|)    
#> price        -0.388123   0.027026 -14.3612 < 2.2e-16 ***
#> feat          0.829004   0.552278   1.5011    0.1333    
#> brandhiland  -3.991559   0.165894 -24.0609 < 2.2e-16 ***
#> brandweight  -0.662086   0.055779 -11.8698 < 2.2e-16 ***
#> brandyoplait  0.787718   0.086232   9.1348 < 2.2e-16 ***
#> price:feat   -0.076733   0.071069  -1.0797    0.2803    
#> sd_feat      -2.341507   0.493413  -4.7455  2.08e-06 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2645.2196929
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5304.4393858
#> BIC:                     5344.9569000
#> McFadden R2:                0.2089044
#> Adj McFadden R2:            0.2068109
#> Number of Observations:  2412.0000000
#> 
#> Summary of 10k Draws for Random Coefficients: 
#>      Min.   1st Qu.    Median      Mean  3rd Qu. Max.
#> feat -Inf -0.747793 0.8307929 0.8322819 2.409918  Inf
```
