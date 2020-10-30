
# logitr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/logitr)](https://CRAN.R-project.org/package=logitr)

<!-- badges: end -->

**logitr** estimates multinomial (MNL) and mixed logit (MXL) models in
R. Models can be estimated using “Preference” space or
“Willingness-to-pay” (WTP) space [utility
parameterizations](https://jhelvy.github.io/logitr/articles/utility_models.html).
The current version includes support for:

  - Homogeneous multinomial logit (MNL) models
  - Heterogeneous mixed logit (MXL) models (with normal and log-normal
    parameter distributions).
  - Preference space utility parameterization.
  - WTP space utility parameterization.
  - An optional multistart optimization that uses different random
    starting points in each iteration (useful for non-convex problems
    like MXL models or models with WTP space parameterizations).

The package also has additional functions for:

  - Computing and comparing WTP from both preference space and WTP space
    models.
  - Simulating the expected shares of a set of alternatives using an
    estimated model.

Note: MXL models assume uncorrelated heterogeneity covariances and are
estimated using maximum simulated likelihood based on the algorithms in
[Kenneth Train’s](http://eml.berkeley.edu/~train/) book [*Discrete
Choice Methods with Simulation, 2nd Edition (New York: Cambridge
University Press, 2009)*](http://eml.berkeley.edu/books/choice2.html).

View the [basic
usage](https://jhelvy.github.io/logitr/articles/basic_usage.html) page
for details on how to use **logitr** to estimate models.

## Installation

The current version is not yet on CRAN, but you can install it from
Github using the **devtools** library:

    devtools::install_github("jhelvy/logitr")

## Required Libraries

**logitr** requires the
[**nloptr**](https://cran.r-project.org/web/packages/nloptr/index.html)
library. This is because `nloptr()` allows for both the objective and
gradient functions to be computed in a single function. This speeds up
computation time considerably because both the objective and gradient
functions require many of the same calculations (e.g. computing
probabilities).

# Author, Version, and License Information

  - Author: *John Paul Helveston*
    [www.jhelvy.com](http://www.jhelvy.com/)
  - Date First Written: *Sunday, September 28, 2014*
  - Most Recent Update: *Thursday, Oct 22, 2020*
  - License: GPL-3
  - Latest Version: 1.2.0

# Citation Information

If you use this package for in a publication, I would greatly appreciate
it if you cited it. You can get the citation information by typing this
into R:

    citation('logitr')
