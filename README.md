# logitr
*logitr* estimates multinomial (MNL) and mixed logit (MXL) models in R. Models can be estimated using "Preference" space or "Willingness-to-pay (WTP)" space utility parameterizations. The current version includes support for:
- Homogeneous multinomial logit (MNL) models
- Heterogeneous mixed logit (MXL) models (support for normal and log-normal parameter distributions).
- Preference space utility parameterization.
- WTP space utility parameterization.
- A multistart optimization loop with random starting points in each iteration (useful for non-convex problems like MXL models or models with WTP space utility parameterizations).

MXL models assume uncorrelated heterogeneity covariances and are estimated using maximum simulated likelihood based on the algorithms in [Kenneth Train's](http://eml.berkeley.edu/~train/) book [*Discrete Choice Methods with Simulation, 2nd Edition (New York: Cambridge University Press, 2009)*](http://eml.berkeley.edu/books/choice2.html).

# Table of Contents
- [Installation](#installation)
  - [Required Libraries](#required-libraries)
- [Using `logitr()`](#using-logitr)
  - [Arguments](#arguments)
  - [Options](#options)
  - [Values](#values)
  - [Data File Setup](#data-file-setup)
  - [Details About `parNames` Argument](#details-about-parnames-argument)
  - [Using `summary()` with `logitr`](#using-summary-with-logitr)
  - [Computing and Comparing WTP](#computing-and-comparing-wtp)
- [Author, Version, and License Information](#author-version-and-license-information)
- [Citation Information](#citation-information)

# Installation
1. Make sure you have the `devtools` library installed:

`install.packages('devtools')`

2. Load the `devtools` library and install the `logitr` package:

```
library('devtools')
install_github('jhelvy/logitr')
library('logitr')
```

### Required Libraries
*logitr* requires the following libraries:
- `nloptr` (for doing the optimization)
- `randtoolbox` (for taking Halton draws in MXL models)

The main optimization loop uses the `nloptr` function to minimize the negative log-likelihood function. `nloptr` is used instead of the Base R `optim` because it allows for both the objective and gradient functions to be included in one function. This speeds up computation time considerably because both the objective and gradient functions require many of the same calculations (e.g. computing the probabilities), which only have to be computed once in `nloptr` (`optim` requires separate objective and gradient functions, so many calculations are repeated within each iteration of the optimization loop).

# Using `logitr()`
(See the './example' folder for an example)

The main model estimation function is the `logitr()` function:

```
model = logitr(data, choiceName, obsIDName, parNames, priceName=NULL,
               randPars=NULL, randPrice=NULL, modelSpace='pref',
               options=list(...))
```

The function returns a list of values, so assign the model output to a variable (e.g. "model") to store the output values.

## Arguments
|    Argument    |    Description    |    Default    |
|:---------------|:------------------|:--------------|
|`data`|The choice data, formatted as a data.frame object (see the [Data File Setup](#data-file-setup) Section for details).| -- |
|`choiceName`|The name of the column that identifies the `choice` variable.| -- |
|`obsIDName`|The name of the column that identifies the `obsID` variable.| -- |
|`parNames`|The names of the parameters to be estimated in the model. Must be the same as the column names in the `data` argument. For WTP space models, do not include price in `parNames`. See the [Details About `parNames` Argument](#details-about-parnames-argument) Section for more details.| -- |
|`priceName`|The name of the column that identifies the price variable. Only required for WTP space models.|`NULL`|
|`randPars`|A named vector whose names are the random parameters and values the destribution: `'n'` for normal or `'ln'` for log-normal.|`NULL`|
|`randPrice`|The random distribution for the price parameter: `'n'` for normal or `'ln'` for log-normal. Only used for WTP space MXL models.|`NULL`|
|`modelSpace`|Set to `'wtp'` for WTP space models.|`'pref'`|
|`options`|A list of options (see the [Options](#options) Section for details).| -- |

## Options
|    Argument    |    Description    |    Default    |
|:---------------|:------------------|:--------------|
|`numMultiStarts`|Number of times to run the optimization loop, each time starting from a different random starting point for each parameter between (-1, 1). Recommended for non-convex models, such as WTP space models and MXL models.|`1`|
|`keepAllRuns`|Set to `TRUE` to keep all the model information for each multistart run. If `TRUE`, the `logitr()` function will return a list with three values: `models` (a list of each model), `multistartSummary` (a summary of all the multistart runs), and `bestModel` (as determined by the largest log-likelihood value).|`FALSE`|
|`startParBounds`|Set the `lower` and `upper` bounds for the starting parameters for each optimization run, which are generated by `runif(n, lower, upper)`.|`c(-1, 1)`|
|`startVals`|A vector of values to be used as starting values for the optimization. Only used for the first run if numMultiStarts > 1.|NULL|
|`useAnalyticGrad`|Set to `FALSE` to use numerically approximated gradients instead of analytic gradients during estimation (which is slower).|`TRUE`|
|`scaleInputs`|By default each variable in `data` is scaled to be between 0 and 1 before running the optimization routine because it usually helps with stability, especially if some of the variables have very large or very small values (e.g. > 10^3 or < 10^-3). Set to `FALSE` to turn this feature off.|`TRUE`|
|`standardDraws`|The user can provide a matrix of standard draws to be used for MXL models.|`NULL`|
|`numDraws`|The number of draws to use for MXL models for the maximum simulated likelihood.|`200`|
|`drawType`|The type of draw to use for MXL models for the maximum simulated likelihood. Set to `'normal'` to use random normal draws, `'halton'` for Halton draws, or `'sobol'` for Sobol draws.|`'halton'`|
|`printLevel`|The print level of the `nloptr` optimization loop. Type `nloptr.print.options()` for more details.|`0`|
|`xtol_rel`|The relative `x` tolerance for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details.|`1.0e-8`|
|`xtol_abs`|The absolute `x` tolerance for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details.|`1.0e-8`|
|`ftol_rel`|The relative `f` tolerance for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details.|`1.0e-8`|
|`ftol_abs`|The absolute `f` tolerance for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details.|`1.0e-8`|
|`maxeval`|The maximum number of function evaluations for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details.|`1000`|

## Values
|    Value    |    Description    |
|:------------|:------------------|
|`coef`|The model coefficients at convergence.|
|`standErrs`|The standard errors of the model coefficients at convergence.|
|`logLik`|The log-likelihood value at convergence.|
|`nullLogLik`|The null log-likelihood value (if all coefficients are 0).|
|`gradient`|The gradient of the log-likelihood at convergence.|
|`hessian`|The hessian of the log-likelihood at convergence.|
|`numObs`|The number of observations.|
|`numParams`|The number of model parameters.|
|`startPars`|The starting values used.|
|`multistartNumber`|The multistart run number for this model.|
|`time`|The user, system, and elapsed time to run the optimization.|
|`iterations`|The number of iterations until convergence.|
|`message`|A more informative message with the status of the optimization result.|
|`status`|An integer value with the status of the optimization (positive values are successes). Type `logitr.statusCodes()` for a detailed description.|
|`modelSpace`|The model space (`'pref'` or `'wtp'`).|
|`standardDraws`|The draws used during maximum simulated likelihood (for MXL models).|
|`randParSummary`|A summary of any random parameters (for MXL models).|
|`options`|A list of all the model options.|

## Data File Setup
The data must be a `data.frame` object and arranged such that each row is an alternative from a choice observation. The choice observations do not have to be symmetric (i.e. they could each have a different number of alternatives). The columns must include all variables that will be used as model covariates. In addition, the following variables must be included:

- `obsID`: A sequence of numbers that identifies each unique choice occasion. For example, if the first three choice occasions had 2 alternatives each, then the first 9 rows of the `obsID` variable would be `1,1,2,2,3,3`.
- `choice`: A dummy variable that identifies which alternative was chosen (`1`=chosen, `0`=not chosen).

**WTP space models**:
You must include a `price` variable (entries should be the price values).

## Details About `parNames` Argument:
The model assumes that the deterministic part of the utility function is linear in parameters (*v* = *beta* ' *x*). Accordingly, each parameter in the `parNames` argument is an additive part of *v*. For example, for the utility model *u* = *beta1* * *price* + *beta2* * *size* + *error*, then the `parNames` argument should be `c('price', 'size')`, and there should be two columns of data in the `data` argument called `price` and `size`. If you wanted to add a third parameter, say *price^2*, then you should create a separate variable in the data.frame called something like `priceSquared` and your `parNames` argument would be `c('price', 'size', 'priceSquared')`.

**WTP space models**:
The `parNames` should be the WTP parameters, and the `price` parameter is denoted by the separate argument `priceName`. For example, for the utility model *u* = *lambda*(*beta1* * *size* - *price*) + *error*, then the `parNames` argument should be `c('size')` and the `priceName` argument should be `'price'`.

## Using `summary()` with `logitr`
The *logitr* package also includes a summary function that has several variations:
- For a single model run, it prints some summary information, including the model space, log-likelihood value at the solution, and a summary table of the model coefficients.
- For MXL models, the function also prints a summary of the random parameters.
- If the `keepAllRuns` option is set to `TRUE`, the function will print a summary of all the multistart runs followed by a summary of the best model (as determined by the largest log-likelihood value).

To understand the status code of any model, type `logitr.statusCodes()`, which prints a description of each status code from the `nloptr` optimization routine.

## Computing and Comparing WTP
For models in the preference space, you can get a summary table of the implied WTP by using:

`wtp.logitr(prefSpaceModel, priceName)`

To compare the WTP between two equivalent models in the preference space and WTP spaces, use:

`wtpCompare(prefSpaceModel, wtpSpaceModel, priceName)`

# Author, Version, and License Information
- Author: *John Paul Helveston* (www.jhelvy.com)
- Date First Written: *Sunday, September 28, 2014*
- Most Recent Update: *Sunday, March 12, 2018*
- License: GPL-3
- Latest Version: 1.0

# Citation Information
If you use this package for in a publication, I would greatly appreciate it if you cited it. You can get the citation information by typing this into R:

`citation('logitr')`
