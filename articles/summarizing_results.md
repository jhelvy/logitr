# Summarizing Results

## Extracting summary tables

The most common function used to view an overall summary of a model is
the [`summary()`](https://rdrr.io/r/base/summary.html) function:

``` r
library(logitr)

model <- logitr(
  data    = yogurt,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "feat", "brand")
)

summary(model)
#> =================================================
#> 
#> Model estimated on: Fri Nov 14 17:02:39 2025 
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

The summary prints out a table of the model coefficients as well as
other information about the model, such as the log-likelihood, the
number of observations, etc.

You can extract the coefficient from the summary table as a `data.frame`
using `coef(summary(model))`:

``` r
coefs <- coef(summary(model))
coefs
#>                Estimate Std. Error    z-value     Pr(>|z|)
#> price        -0.3665546 0.02436526 -15.044150 0.000000e+00
#> feat          0.4914392 0.12006175   4.093221 4.254226e-05
#> brandhiland  -3.7154773 0.14541671 -25.550553 0.000000e+00
#> brandweight  -0.6411384 0.05449794 -11.764450 0.000000e+00
#> brandyoplait  0.7345195 0.08064229   9.108366 0.000000e+00
```

### The {broom} package

Another approach for extracting the model coefficients as a data frame
is to use the [`tidy()`](https://generics.r-lib.org/reference/tidy.html)
function from the [{broom}](https://broom.tidymodels.org/index.html)
package:

``` r
library(broom)

coefs <- tidy(model)
coefs
#> # A tibble: 5 × 5
#>   term         estimate std.error statistic   p.value
#>   <chr>           <dbl>     <dbl>     <dbl>     <dbl>
#> 1 price          -0.367    0.0244    -15.0  0        
#> 2 feat            0.491    0.120       4.09 0.0000425
#> 3 brandhiland    -3.72     0.145     -25.6  0        
#> 4 brandweight    -0.641    0.0545    -11.8  0        
#> 5 brandyoplait    0.735    0.0806      9.11 0
```

The [`tidy()`](https://generics.r-lib.org/reference/tidy.html) function
returns a `tibble` and provides a more standardized output and
interfaces well with other packages. You can also append a confidence
interval to the data frame:

``` r
coefs <- tidy(model, conf.int = TRUE, conf.level = 0.95)
coefs
#> # A tibble: 5 × 7
#>   term         estimate std.error statistic   p.value conf.low conf.high
#>   <chr>           <dbl>     <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
#> 1 brandhiland    -3.72     0.145     -25.6  0           -4.00     -3.43 
#> 2 brandweight    -0.641    0.0545    -11.8  0           -0.748    -0.534
#> 3 brandyoplait    0.735    0.0806      9.11 0            0.577     0.892
#> 4 feat            0.491    0.120       4.09 0.0000425    0.256     0.726
#> 5 price          -0.367    0.0244    -15.0  0           -0.415    -0.319
```

## Extracting other values

You can also extract other values of interest at the solution, such as:

**The estimated coefficients**

``` r
coef(model)
#>        price         feat  brandhiland  brandweight brandyoplait 
#>   -0.3665546    0.4914392   -3.7154773   -0.6411384    0.7345195
```

**The estimated standard errors**

``` r
se(model)
#>        price         feat  brandhiland  brandweight brandyoplait 
#>   0.02436526   0.12006175   0.14541671   0.05449794   0.08064229
```

**The log-likelihood**

``` r
logLik(model)
#> 'log Lik.' -2656.888 (df=5)
```

**The variance-covariance matrix**

``` r
vcov(model)
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

You can also view a summary of statistics about the model using the
[`glance()`](https://generics.r-lib.org/reference/glance.html) function
from the [{broom}](https://broom.tidymodels.org/index.html) package:

``` r
glance(model)
#> # A tibble: 1 × 7
#>   logLik null.logLik   AIC   BIC r.squared adj.r.squared  nobs
#>    <dbl>       <dbl> <dbl> <dbl>     <dbl>         <dbl> <dbl>
#> 1 -2657.      -3344. 5324. 5353.     0.205         0.204  2412
```

## Formatted summary tables

### The {gtsummary} package

Often times you will need to create summary tables that are formatted
for publication. The
[{gtsummary}](https://www.danieldsjoberg.com/gtsummary/) package offers
a convenient solution that works well with `logitr`models. For example,
a formatted summary table can be obtained using the
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
function:

``` r
library(gtsummary)

model |> 
  tbl_regression()
```

[TABLE]

The
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html)
function has many
[options](https://www.danieldsjoberg.com/gtsummary/articles/tbl_regression.html)
for customizing the output table. For example, you can change the
coefficient names with the `label` argument:

``` r
model |> 
  tbl_regression(
    label = list(
        feat = "Newspaper ad shown?",
        brand = "Yogurt's brand"
    )
  )
```

[TABLE]

The {gtsummary} package supports a wide variety of output types,
including support for [LaTeX](https://www.latex-project.org/). One you
create the table with
[`tbl_regression()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_regression.html),
you can print it a variety of ways. For example, once you’ve created a
table `x`,

``` r
x <- model |>
  tbl_regression()
```

you can print it to LaTeX with any of the following ways:

- `as_gt(x) |> gt::as_latex()`
- `as_kable_extra(x, format = "latex")`
- `as_hux_table(x) |> to_latex()`
- `as_kable(x, format = "latex")`

Multiple models can also be printed in the same table:

``` r
model1 <- model

model2 <- logitr(
  data    = yogurt,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price*feat", "brand")
)

# Make individual tables
t1 <- tbl_regression(model1)
t2 <- tbl_regression(model2)

# Merge tables
tbl_merge(
  tbls = list(t1, t2),
  tab_spanner = c("**Baseline**", "**Interaction**")
)
```

[TABLE]

### The {texreg} package

Another option for obtaining a formatted table is to use the
[{texreg}](https://github.com/leifeld/texreg) package. This is
particularly useful for obtaining tables formatted for use in
[LaTeX](https://www.latex-project.org/).

For example, you can print a summary to the screen using
[`screenreg()`](https://rdrr.io/pkg/texreg/man/screenreg.html):

``` r
library(texreg)

screenreg(model, stars = c(0.01, 0.05, 0.1))
#> 
#> ============================
#>                 Model 1     
#> ----------------------------
#> price              -0.37 ***
#>                    (0.02)   
#> feat                0.49 ***
#>                    (0.12)   
#> brandhiland        -3.72 ***
#>                    (0.15)   
#> brandweight        -0.64 ***
#>                    (0.05)   
#> brandyoplait        0.73 ***
#>                    (0.08)   
#> ----------------------------
#> Num. obs.        2412       
#> Log Likelihood  -2656.89    
#> AIC              5323.78    
#> BIC              5352.72    
#> ============================
#> *** p < 0.01; ** p < 0.05; * p < 0.1
```

Likewise, you can print the LaTeX code for a summary table using
[`texreg()`](https://rdrr.io/pkg/texreg/man/texreg.html)

``` r
library(texreg)

texreg(model, stars = c(0.01, 0.05, 0.1))
#> 
#> \begin{table}
#> \begin{center}
#> \begin{tabular}{l c}
#> \hline
#>  & Model 1 \\
#> \hline
#> price          & $-0.37^{***}$ \\
#>                & $(0.02)$      \\
#> feat           & $0.49^{***}$  \\
#>                & $(0.12)$      \\
#> brandhiland    & $-3.72^{***}$ \\
#>                & $(0.15)$      \\
#> brandweight    & $-0.64^{***}$ \\
#>                & $(0.05)$      \\
#> brandyoplait   & $0.73^{***}$  \\
#>                & $(0.08)$      \\
#> \hline
#> Num. obs.      & $2412$        \\
#> Log Likelihood & $-2656.89$    \\
#> AIC            & $5323.78$     \\
#> BIC            & $5352.72$     \\
#> \hline
#> \multicolumn{2}{l}{\scriptsize{$^{***}p<0.01$; $^{**}p<0.05$; $^{*}p<0.1$}}
#> \end{tabular}
#> \caption{Statistical models}
#> \label{table:coefficients}
#> \end{center}
#> \end{table}
```

Similar to {gtsummary}, multiple models can be printed using
[`screenreg()`](https://rdrr.io/pkg/texreg/man/screenreg.html) or
[`texreg()`](https://rdrr.io/pkg/texreg/man/texreg.html):

``` r
model1 <- model

model2 <- logitr(
  data    = yogurt,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price*feat", "brand")
)

screenreg(
  list(
    model1,
    model2
  ),
  stars = c(0.01, 0.05, 0.1),
  custom.model.names = c("Baseline", "Interaction")
)
#> 
#> ==========================================
#>                 Baseline      Interaction 
#> ------------------------------------------
#> price              -0.37 ***     -0.36 ***
#>                    (0.02)        (0.02)   
#> feat                0.49 ***      1.16 ***
#>                    (0.12)        (0.38)   
#> brandhiland        -3.72 ***     -3.72 ***
#>                    (0.15)        (0.15)   
#> brandweight        -0.64 ***     -0.64 ***
#>                    (0.05)        (0.05)   
#> brandyoplait        0.73 ***      0.72 ***
#>                    (0.08)        (0.08)   
#> price:feat                       -0.09 *  
#>                                  (0.05)   
#> ------------------------------------------
#> Num. obs.        2412          2412       
#> Log Likelihood  -2656.89      -2655.54    
#> AIC              5323.78       5323.08    
#> BIC              5352.72       5357.81    
#> ==========================================
#> *** p < 0.01; ** p < 0.05; * p < 0.1
```
