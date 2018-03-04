# Table of Contents
- [Overview](#overview)
- [Installation](#installation)
  - [Required Libraries](#required-libraries)
- [Contents](#contents)
- [Using `logitr()`](#using-logitr)
  - [Arguments](#arguments)
  - [Options](#options)
  - [Values](#values)
  - [Data File Setup](#data-file-setup)
- [Using `logitr.summary()`](#using-logitr.summary)
- [Author, Version, and License Information](#author,-version,-and-license-information)
- [Citation Information](#citation-information)

# Overview
*logitr* estimates multinomial (MNL) and mixed logit (MXL) models and allows for models in the "preference space" or "willingness to pay (WTP) space." The current version includes support for:
- Homogeneous multinomial logit (MNL) models
- Heterogeneous mixed logit (MXL) models (support for normal and log-normal parameter distributions).
- Preference space models
- WTP space models.
- A multi-start option for searching the solution space from multiple random different starting points (useful for non-convex problems, such as those for WTP space models or MXL models).

The MXL models assume uncorrelated heterogeneity covariances. The MXL models are estimated using maximum simulated likelihood based on [Kenneth Train's](http://eml.berkeley.edu/~train/) book [*Discrete Choice Methods with Simulation, 2nd Edition (New York: Cambridge University Press, 2009)*](http://eml.berkeley.edu/books/choice2.html).

# Installation
First, make sure you have the `devtools` library installed:

`install.packages('devtools')`

Then run these commands to install and load the `logitr` package:

```
library('devtools')
install_github('jhelvy/logitr')
library('logitr')
```

### Required Libraries
*logitr* requires the following libraries:
- `nloptr` (for doing the optimization)
- `randtoolbox` (for taking Halton draws in MXL models)

The main optimization loop uses the `nloptr` function to minimize the negative log-likelihood function. `nloptr` is used instead of the Base R `optim` because it allows for both the objective and gradient functions to be included in one function. This speeds things up because both the objective and gradient functions share many of the same calculations.

# Contents
This package contains the following functions:

- `logitr()`: The main function for running the logitr program.
- `logitr.summary()`: Prints a summary of an estimated model using the logitr package.
- `logitr.statusCodes()`: Prints the status codes from the nloptr optimization routine.

# Using `logitr()`
(See the 'example' folder for an example)

The main model estimation function is the `logitr()` function:

```
model = logitr(data, choiceName, obsIDName, parNames, priceName=NULL,
               parDist=NULL, priceDist=NULL, modelSpace='pref',
               options=list(...))
```

The function returns a list of values, so be sure to assign the model output to a variable, like "model".

## Arguments
|    Argument    |    Description    |
|:---------------|:------------------|
|`data`|The choice data, formatted as a data.frame object (see the [Data File Setup](#data-file-setup) Section for details).|
|`choiceName`|The name of the column that identifies the `choice` variable.|
|`obsIDName`|The name of the column that identifies the `obsID` variable.|
|`parNames`|The names of the parameters to be estimated in the model. Must be the same as the column names in the `data` argument. For WTP space models, do not include price in parNames.|
|`priceName`|The name of the column that identifies the price variable. Only required for WTP space models. Defaults to `NULL` if left unspecified.|
|`parDist`|A vector describing the distributional assumptions on each parameter. 0=fixed, 1=normal, 2=log-normal. Only required for MXL models. Defaults to `NULL` if left unspecified.|
|`priceDist`|A number describing the distributional assumptions on the price parameter. 0=fixed, 1=normal, 2=log-normal. Only required for WTP space MXL models. Defaults to `NULL` if left unspecified.|
|`modelSpace`|Set to `'wtp'` for WTP space models. Defaults to `'pref'` (a preference space model).|
|`options`|A list of options (see the [Options](#options) Section for details).|

## Options
|    Argument    |    Description    |
|:---------------|:------------------|
|`numMultiStarts`|Number of times to run the optimization loop, each time starting from a different random starting point for each parameter between (-1, 1). Recommended for non-convex models, such as WTP space models and MXL models. Defaults to `1`.|
|`keepAllRuns`|Set to `TRUE` to keep all the model information for each multistart run. If `TRUE`, the `logitr()` function will return a list with three values: `models` (a list of each model), `multistartSummary` (a summary of all the multistart runs), and `bestModel` (as determined by the largest log-likelihood value). Defaults to `FALSE`.|
|`useAnalyticGrad`|Set to `TRUE` to use the analytic (instead of numerically approximated) gradient during estimation. Currently only works for MNL models (MXL models will ignore this option and always use numeric gradients). Defaults to `TRUE`.|
|`scaleInputs`|Set to `TRUE` to scale each variable in `data` to be between 0 and 1. This is sometimes helpful for the optimization routine is some of the variables have very large or very small values (e.g. > 10^3 or < 10^-3). Defaults to `FALSE`.|
|`prefSpaceModel`|For WTP space models, you can provide an estimated preference space model which will do two things: 1) The WTP space model will use the computed WTP from the preference space model as the starting parameters for the first multistart run, and 2) a comparison of the computed WTP from the preference space model with the estimated WTP space model results will be provided. Obviously, for this to be useful the prefSpaceModel should have the same parameters as the WTP space model being estimated, except for the price parameter. Defaults to `NULL` if left unspecified.|
|`standardDraws`|The user can provide a matrix of standard draws to be used for MXL models. Defaults to `NULL` if left unspecified.|
|`numDraws`|The number of draws to use for MXL models for the maximum simulated likelihood. Defaults to `200`.|
|`drawType`|The type of draw to use for MXL models for the maximum simulated likelihood. Set to `'normal'` to use random normal draws, `'halton'` for Halton draws, or `'sobol'` for Sobol draws. Defaults to `'halton'`.|
|`printLevel`|The print level of the `nloptr` optimization loop. Type `nloptr.print.options()` for more details. Defaults to `0`.|
|`xtol_rel`|The relative `x` tolerance for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details. Defaults to `1.0e-9`.|
|`xtol_abs`|The absolute `x` tolerance for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details. Defaults to `1.0e-9`.|
|`ftol_rel`|The relative `f` tolerance for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details. Defaults to `1.0e-9`.|
|`ftol_abs`|The absolute `f` tolerance for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details. Defaults to `1.0e-9`.|
|`maxeval`|The maximum number of function evaluations for the `nloptr` optimization loop. Type `nloptr.print.options()` for more details. Defaults to `1000`.|

## Values
|    Value    |    Description    |
|:------------|:------------------|
|`summaryTable`|A summary table of the model coefficients and fit statistics.|
|`coef`|The model coefficients at convergence.|
|`standErrs`|The standard errors of the model coefficients at convergence.|
|`logLik`|The log-likelihood value at convergence.|
|`nullLogLik`|The null log-likelihood value (if all coefficients are 0).|
|`gradient`|The gradient of the log-likelihood at convergence.|
|`hessian`|The hessian of the log-likelihood at convergence.|
|`startPars`|The starting values used.|
|`iterations`|The number of iterations until convergence.|
|`message`|A more informative message with the status of the optimization result.|
|`status`|An integer value with the status of the optimization (positive values are successes). Type `logitr.statusCodes()` for a detailed description.|
|`multistartNumber`|The multistart run number for this model.|
|`modelSpace`|The same as the `modelSpace` argument.|
|`standardDraws`|The draws used during maximum simulated likelihood (for MXL models).|
|`randParSummary`|A summary of any random parameters (for MXL models).|
|`wtpComparison`|A comparison of the WTP between a preference space and WTP space model (only reported for WTP space models that also included the `prefSpaceModel` argument in the options.|
|`options`|A list of all the model options.|

## Data File Setup
The data must be arranged such that each row is an alternative from a choice observation. The choice observations do not have to be symmetric (i.e. they could each have a different number of alternatives). The columns must include all variables that will be used as model covariates. In addition, the data must include each of the following variables:

- `obsID`: A sequence of numbers that identifies each unique choice occasion. For example, if the first three choice occasions had 2 alternatives each, then the first 9 rows of the \emph{obsID} variable would be 1,1,2,2,3,3.
- `choice`: A dummy variable that identifies which alternative was chosen (1=chosen, 0 = not chosen).

For WTP space models, you must include a `price` variable (entries should be the price value).

# Using `logitr.summary()`
The *logitr* package also includes a summary function:

`logitr.summary(model)`

where `model` is a model estimated using the `logitr()` function.

Variations:
- For a single model run, it prints some summary information, including the model space, log-likelihood value at the solution, and a summary table of the model.
- For MXL models, the function also prints a summary of the random parameters.
- For WTP space models, if a `prefSpaceModel` was included in the options argument, the function also prints a summary of the WTP comparison between the two models spaces.
- If the `keepAllRuns` option is set to `TRUE`, the function will print a summary of all the multistart runs followed by a summary of the best model (as determined by the largest log-likelihood value).

To understand the status code of any model, type `logitr.statusCodes()`, which prints a summary of the status codes from the `nloptr` optimization routine.

# Author, Version, and License Information
- Author: *John Paul Helveston* (www.jhelvy.com/logitr)
- Date First Written: *Sunday, September 28, 2014*
- Most Recent Update: *Friday, March 3, 2018*
- License: GPL-3
- Latest Version: 0.5.0

# Citation Information
If you use this package for in a publication, I would greatly appreciate it if you cited it. You can get the citation information by typing this into R:

`citation('logitr')`
