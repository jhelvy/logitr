
<!-- README.md is generated from README.Rmd. Please edit that file -->

# logitr <a href='https://jhelvy.github.io/logitr/'><img src='man/figures/logo.png' align="right" style="height:139px;"/></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/logitr)](https://CRAN.R-project.org/package=logitr)
[![Travis build
status](https://app.travis-ci.com/jhelvy/logitr.svg?branch=master)](https://app.travis-ci.com/github/jhelvy/logitr)
[![](http://cranlogs.r-pkg.org/badges/grand-total/logitr?color=blue)](https://cran.r-project.org/package=logitr)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/logitr)](https://cran.r-project.org/package=logitr)
<!-- badges: end -->

logitr: Fast Estimation of Multinomial (MNL) and Mixed Logit (MXL)
Models with Preference Space and Willingness to Pay Space [Utility
Parameterizations](https://jhelvy.github.io/logitr/articles/utility_models.html)

The latest version includes support for:

- Multinomial logit (MNL) models
- Mixed logit (MXL) models with normal and log-normal parameter
  distributions.
- Preference space and WTP space utility parameterizations.
- Weighted models to differentially weight individual observations.
- Uncorrelated or correlated heterogeneity covariances for mixed logit
  models.
- Functions for computing WTP from preference space models.
- Functions for predicting expected probabilities and outcomes for sets
  of alternatives based on an estimated model.
- A parallelized multistart optimization loop that uses different random
  starting points in each iteration to search for different local minima
  (useful for non-convex problems like MXL models or models with WTP
  space parameterizations).

Mixed logit models are estimated using maximum simulated likelihood
based on the algorithms in Kenneth Train’s book [*Discrete Choice
Methods with Simulation, 2nd Edition (New York: Cambridge University
Press, 2009)*](https://eml.berkeley.edu/books/choice2.html).

## Basic Usage

View the [basic
usage](https://jhelvy.github.io/logitr/articles/basic_usage.html) page
for details on how to use **logitr** to estimate models.

## JSS Article

An associated paper in the *Journal of Statistical Software* about this
package is available at <https://doi.org/10.18637/jss.v105.i10>

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

## Author, Version, and License Information

- Author: *John Paul Helveston* <https://www.jhelvy.com/>
- Date First Written: *Sunday, September 28, 2014*
- License:
  [MIT](https://github.com/jhelvy/logitr/blob/master/LICENSE.md)

## Citation Information

If you use this package for in a publication, please cite the JSS
article associated with it! You can get the citation by typing
`citation("logitr")` into R:

``` r
citation("logitr")
#> 
#> To cite logitr in publications use:
#> 
#>   Helveston JP (2023). "logitr: Fast Estimation of Multinomial and
#>   Mixed Logit Models with Preference Space and Willingness-to-Pay Space
#>   Utility Parameterizations." _Journal of Statistical Software_,
#>   *105*(10), 1-37. doi:10.18637/jss.v105.i10
#>   <https://doi.org/10.18637/jss.v105.i10>.
#> 
#> A BibTeX entry for LaTeX users is
#> 
#>   @Article{,
#>     title = {{logitr}: Fast Estimation of Multinomial and Mixed Logit Models with Preference Space and Willingness-to-Pay Space Utility Parameterizations},
#>     author = {John Paul Helveston},
#>     journal = {Journal of Statistical Software},
#>     year = {2023},
#>     volume = {105},
#>     number = {10},
#>     pages = {1--37},
#>     doi = {10.18637/jss.v105.i10},
#>   }
```
