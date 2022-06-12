
<!-- README.md is generated from README.Rmd. Please edit that file -->

# logitr <a href='https://jhelvy.github.io/logitr/'><img src='man/figures/logo.png' align="right" style="height:139px;"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/logitr)](https://CRAN.R-project.org/package=logitr)
[![Travis build
status](https://app.travis-ci.com/jhelvy/logitr.svg?branch=master)](https://app.travis-ci.com/github/jhelvy/logitr)
[![](http://cranlogs.r-pkg.org/badges/grand-total/logitr?color=blue)](https://cran.r-project.org/package=logitr)
<!-- badges: end -->

logitr: Fast Estimation of Multinomial (MNL) and Mixed Logit (MXL)
Models with Preference Space and Willingness to Pay Space [Utility
Parameterizations](https://jhelvy.github.io/logitr/articles/utility_models.html)

The latest version includes support for:

-   Multinomial logit (MNL) models
-   Mixed logit (MXL) models with normal and log-normal parameter
    distributions.
-   Preference space and WTP space utility parameterizations.
-   Weighted models to differentially weight individual observations.
-   Uncorrelated or correlated heterogeneity covariances for mixed logit
    models.
-   Functions for computing WTP from preference space models.
-   Functions for predicting expected probabilities and outcomes for
    sets of alternatives based on an estimated model.
-   A parallelized multistart optimization loop that uses different
    random starting points in each iteration to search for different
    local minima (useful for non-convex problems like MXL models or
    models with WTP space parameterizations).

Mixed logit models are estimated using maximum simulated likelihood
based on the algorithms in Kenneth Train’s book [*Discrete Choice
Methods with Simulation, 2nd Edition (New York: Cambridge University
Press, 2009)*](https://eml.berkeley.edu/books/choice2.html).

## Installation

You can install {logitr} from CRAN:

``` r
install.packages("logitr")
```

or you can install the development version of {logitr} from
[GitHub](https://github.com/jhelvy/logitr):

``` r
# install.packages("remotes")
remotes::install_github("jhelvy/logitr")
```

Load the library with:

``` r
library(logitr)
```

## Basic Usage

View the [basic
usage](https://jhelvy.github.io/logitr/articles/basic_usage.html) page
for details on how to use **logitr** to estimate models.

## Related software

If you are a Python user, [`xlogit`](https://github.com/arteagac/xlogit)
is a similar package built in Python. It has a similar user interface
for defining models, and it is even faster than logitr as it uses
GPU-accelerated estimation for mixed logit models. It is a good resource
for comparing results with those from logitr.

## Author, Version, and License Information

-   Author: *John Paul Helveston* <https://www.jhelvy.com/>
-   Date First Written: *Sunday, September 28, 2014*
-   License:
    [MIT](https://github.com/jhelvy/logitr/blob/master/LICENSE.md)

## Citation Information

If you use this package for in a publication, I would greatly appreciate
it if you cited it - you can get the citation by typing
`citation("logitr")` into R:

``` r
citation("logitr")
#> 
#> To cite logitr in publications use:
#> 
#>   John Paul Helveston (2021). logitr: Fast Estimation of Multinomial
#>   and Mixed Logit Models with Preference Space and Willingness to Pay
#>   Space Utility Parameterizations. R package version 0.6.1
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Manual{,
#>     title = {logitr: Fast Estimation of Multinomial and Mixed Logit Models with Preference Space and Willingness to Pay Space Utility Parameterizations},
#>     author = {John Paul Helveston},
#>     year = {2021},
#>     note = {R package version 0.6.1},
#>     url = {https://jhelvy.github.io/logitr/},
#>   }
```
