# Changelog

## logitr (development version)

## logitr 1.1.2

CRAN release: 2024-07-24

- Added `broom.helpers (>= 1.15.0)` and `gtsummary (>= 2.0.0)` in
  DESCRIPTION to accord with an update in {gtsummary}.

## logitr 1.1.1

CRAN release: 2023-09-29

- Updated the `convergence.Rmd` vignette to not run any actual code
  using other packages to fix issue on CRAN where not all packages are
  available on all platforms. Now the results are hard-coded in place.
- Fixed bug in `makeObsID()` where
  [`table()`](https://rdrr.io/r/base/table.html) was sorting the results
  stored in `reps`, which has to be manually undone. Fixes
  [\#50](https://github.com/jhelvy/logitr/issues/50).

## logitr 1.1.0

CRAN release: 2023-05-17

- Modified [`predict()`](https://rdrr.io/r/stats/predict.html) method to
  use the `interval` and `level` arguments of more standard
  [`predict()`](https://rdrr.io/r/stats/predict.html) methods.
- Added [`ci()`](https://jhelvy.github.io/logitr/reference/ci.md)
  function.
- Added
  [`logit_probs()`](https://jhelvy.github.io/logitr/reference/logit_probs.md)
  function.

## logitr 1.0.1

CRAN release: 2023-02-19

- Fixed bug in `adjustFactorLevels()` where the `levels_orig` object was
  being accidentally overwritten.
- Changed the name of `adjustFactorLevels()` to `checkFactorLevels()`.
- Added a startup message when the package is loaded.

## logitr 1.0.0

CRAN release: 2023-02-07

- Added JSS article DOI throughout package documentation.
- Fixed bug [\#41](https://github.com/jhelvy/logitr/issues/41) where the
  [`predict()`](https://rdrr.io/r/stats/predict.html) method would error
  if factor levels were missing in `newdata`.

## logitr 0.8.0

CRAN release: 2022-10-03

- Added [`tidy()`](https://generics.r-lib.org/reference/tidy.html),
  [`glance()`](https://generics.r-lib.org/reference/glance.html), and
  [`augment()`](https://generics.r-lib.org/reference/augment.html)
  methods for use with the {broom} package.
- Added
  [`model.matrix.logitr()`](https://jhelvy.github.io/logitr/reference/model.matrix.logitr.md),
  [`terms.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md),
  and
  [`model.frame.logitr()`](https://jhelvy.github.io/logitr/reference/model.frame.logitr.md)
  methods for use with {broom.helpers}.
- Added new vignette comparing convergence issues in WTP space models
  with other similar packages.
- Added new vignette on summarizing results from estimated models.

## logitr 0.7.2

- Checks were added to make the `panelID` and `clusterUD` variables a
  sequentially increasing numeric vector and to stop the program if
  there are any repeated IDs in these variables.
- A patch was added to address a bug in the computation of clustered
  errors where the data in clusters of size 1 needed to be forced into a
  matrix with correct dimensions.

## logitr 0.7.1

- A patch was added to make the `obsID` variable a sequentially
  increasing numeric vector (this was previously done but was
  accidentally removed in prior updates).
- A patch was added to pass the `modelSpace` variable along inside the
  [`vcov()`](https://rdrr.io/r/stats/vcov.html) method.

## logitr 0.7.0

CRAN release: 2022-06-16

- A new vignette on benchmarking was added which tests the package speed
  against other similar packages.
- A new data set, `runtimes`, was included, which is exported from the
  colab notebook used for benchmarking here:
  <https://colab.research.google.com/drive/1vYlBdJd4xCV43UwJ33XXpO3Ys8xWkuxx?usp=sharing>
- Sobol draws are supported via a new `drawType` argument.
- A warning is displayed against using Halton draws after 5 random
  variables have been specified in a mixed logit model. Users are
  encouraged to switch to using Sobol draws and increasing the number of
  draws to at least 200.
- Changed the argument name `price` to `scalePar` to be more general.
- Changed the argument name `randPrice` to `randScale` to be more
  general.
- The `modelSpace` argument is no longer required for specifying a WTP
  space model as it is redundant. Including a `scalePar` argument is
  enough to determine that it is a WTP space model.

## logitr 0.6.1

CRAN release: 2022-06-12

- Fixes error on some of the gradient tests from 0.6.0 release. The
  gradient tests were sensitive to the random starting parameters, so I
  modified how the starting parameters were set for the first iteration
  of a multistart loop such that they would be more consistent.

## logitr 0.6.0

CRAN release: 2022-06-11

- Added the `correlation` argument to include correlated heterogeneity.
- Added support for new mixed logit distributions: zero-censored normal
- Added new input checks for the `obsID` and `outcome` arguments.
- [`vcov.logitr()`](https://jhelvy.github.io/logitr/reference/vcov.logitr.md)
  method now returns `object$vcov` if the user set `vcov = TRUE` during
  estimation (avoids a redundant calculation of `vcov`, which is more
  efficient).
- Added new datasets: `apolloModeChoiceData`, `electricity`

## logitr 0.5.1

- Patched a bug in how standard errors were being computed when
  clustering. The source of the error was in re-scaling results
  post-estimation.

## logitr 0.5.0

CRAN release: 2022-01-04

- The multistart optimization loop is now parallelized.
- Exported the
  [`fquantile()`](https://jhelvy.github.io/logitr/reference/fquantile.md)
  function, which is a faster implementation of the
  [`stats::quantile()`](https://rdrr.io/r/stats/quantile.html) function.

## logitr 0.4.0

CRAN release: 2021-10-25

### Larger changes:

- A new
  [`predict.logitr()`](https://jhelvy.github.io/logitr/reference/predict.logitr.md)
  method was added for making probability and choice predictions from
  logitr class objects.
- The
  [`predictProbs()`](https://jhelvy.github.io/logitr/reference/predictProbs.md)
  and
  [`predictChoices()`](https://jhelvy.github.io/logitr/reference/predictChoices.md)
  functions were depreciated.
- Added new
  [`fitted.logitr()`](https://jhelvy.github.io/logitr/reference/fitted.logitr.md)
  and
  [`residuals.logitr()`](https://jhelvy.github.io/logitr/reference/residuals.logitr.md)
  methods.
- Added optional `predict` argument to the main
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function which controls whether predicted probabilities,
  fitted.values, and residuals are included in the returned object.
  Default setting is TRUE.
- Changed the name of the coefficients vector in the returned object
  from “coef” to “coefficients” to be consistent with other packages.
- Changed the argument name from “choice” to “outcome” to be more
  general

### Bugs:

- Fixed bug where the returned object contained the scaled data rather
  than the original, unscaled data

## logitr 0.3.1

- Bug fix: Cast X object to matrix for single-parameter models
- Updated the logic for clustering with and without panel data
- Added the
  [`se.logitr()`](https://jhelvy.github.io/logitr/reference/se.logitr.md)
  method.
- Added the `vcov` argument to the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function.
- Improved vignette on interaction models with individual-specific
  variable interactions.

## logitr 0.3.0

CRAN release: 2021-08-13

### Breaking changes with v0.2.0:

- Several arguments were moved out of the previous `options` argument
  and are now passed directly as arguments to
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md).
  These include: `numMultiStarts`, `useAnalyticGrad`, `scaleInputs`,
  `startParBounds`, `standardDraws`, `numDraws`, `startVals`. The
  `options` argument is now only used for options to control the
  optimization handled by `nloptr()`.
- Options for keeping all model outputs on a multistart were removed.

### Summary of larger updates:

- Added support for panel data in the log-likelihood function and
  gradients.
- Several argument names in the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function were changed to make them easier to understand: `choiceName`
  became `choice`, `obsIDName` became `obsID`, `parNames` became `pars`,
  `priceName` became `price`, `weightsName` became `weights`,
  `clusterName` became `cluster`. If used, old names will be passed to
  the new argument names and a warning will be displayed.
- The log-likelihood and gradient functions were overhauled to improve
  computational efficiency, resulting in substantially faster estimation
  for all models.
- The following new methods were introduced:
  [`print.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md),
  [`logLik.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md),
  [`coef.summary.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md),
  [`vcov.logitr()`](https://jhelvy.github.io/logitr/reference/vcov.logitr.md),
  [`terms.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)

### Summary of smaller updates:

- Improved
  [`summary.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  and
  [`coef.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  methods for better printing, now using
  [`printCoefmat()`](https://rdrr.io/r/stats/printCoefmat.html).
- Added input checks for
  [`wtp()`](https://jhelvy.github.io/logitr/reference/wtp.md) and
  [`wtpCompare()`](https://jhelvy.github.io/logitr/reference/wtpCompare.md)
  functions
- Fixed some errors in some of the documentation examples and removed
  the dontrun commands on all of them.
- Added the `altIDName` argument to
  [`predictChoices()`](https://jhelvy.github.io/logitr/reference/predictChoices.md)
  and
  [`predictProbs()`](https://jhelvy.github.io/logitr/reference/predictProbs.md)
  to preserve the row order of predictions for each alternative in each
  set of alternatives. Closes issue
  [\#13](https://github.com/jhelvy/logitr/issues/13).
- Fixed bug in data encoding where random parameter names were not
  aligned with encoded data.
- Added input checks for all predict functions.

## logitr 0.2.7

Added support for panel data in the log-likelihood function and
gradients

## logitr 0.2.6

Major changes were made to the gradient functions, which dramatically
improved computational efficiency. MNL and MXL models in either
preference or WTP spaces now use the faster implementation of the logit
calculations.

## logitr 0.2.5

This version was the first implementation of an alternative approach for
computing the logit probabilities, which increased computational speed.
Specifically, the formulation was to compute P = 1 / (1 + sum(exp(V -
V_chosen)))

## logitr 0.2.4

The [`vcov()`](https://rdrr.io/r/stats/vcov.html) method was modified
such that it computes the covariance post model estimation. Previously,
the covariance matrix was being computed internally in the
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function, and [`vcov()`](https://rdrr.io/r/stats/vcov.html) just
returned this value, which was computationally much slower.

## logitr 0.2.3

**Several breaking changes in this version**.

- Several argument names were changed to make them easier to understand.
  These include: `choiceName` –\> `choice`, `obsIDName` –\> `obsID`,
  `parNames` –\> `pars`, `priceName` –\> `price`, `weightsName` –\>
  `weights`, `clusterName` –\> `cluster`.
- Several arguments were moved out of the previous `options` argument
  and are now passed directly as arguments to
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md).
  These include: `numMultiStarts`, `useAnalyticGrad`, `scaleInputs`,
  `startParBounds`, `standardDraws`, `numDraws`, `startVals`.
- Some minor tweaks to printing methods.

## logitr 0.2.2

- Improved
  [`summary.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  and
  [`coef.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  methods for better printing, using
  [`printCoefmat()`](https://rdrr.io/r/stats/printCoefmat.html)
- Added new methods:
  [`print.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md),
  [`logLik.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md),
  [`coef.summary.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md),
  [`vcov.logitr()`](https://jhelvy.github.io/logitr/reference/vcov.logitr.md)
- Removed option for keeping all model outputs.
- Added input checks for
  [`wtp()`](https://jhelvy.github.io/logitr/reference/wtp.md) and
  [`wtpCompare()`](https://jhelvy.github.io/logitr/reference/wtpCompare.md)
  functions
- Fixed some errors in some of the examples and made them all run
  (removed dontrun commands).

## logitr 0.2.1

- Added `altIDName` argument to
  [`predictChoices()`](https://jhelvy.github.io/logitr/reference/predictChoices.md)
  and
  [`predictProbs()`](https://jhelvy.github.io/logitr/reference/predictProbs.md)
  to preserve the row order of predictions for each alternative in each
  set of alternatives. Closes issue
  [\#13](https://github.com/jhelvy/logitr/issues/13).
- Fixed bug in data encoding where random parameter names were not
  aligned with encoded data.
- Added input checks for all predict functions.

## logitr 0.2.0

CRAN release: 2021-06-14

### Summary of larger updates:

- New prediction functions:
  [`predictChoices()`](https://jhelvy.github.io/logitr/reference/predictChoices.md)
  and
  [`predictProbs()`](https://jhelvy.github.io/logitr/reference/predictProbs.md),
  and , depreciated
  [`simulateShares()`](https://jhelvy.github.io/logitr/reference/simulateShares.md).
- Added robust covariance matrix calculations.
- Added support for clustering errors.
- Major modifications to the
  [`recodeData()`](https://jhelvy.github.io/logitr/reference/recodeData.md)
  function to improve encoding efficiency.
- Depreciated
  [`dummyCode()`](https://jhelvy.github.io/logitr/reference/dummyCode.md)

### Summary of smaller updates:

- Improved documentation across all vignettes for new features.
- Improved explanation of preference space and WTP space utility models
  in vignettes.

## logitr 0.1.5

- Added robust covariance matrix calculations.
- Added support for clustering errors.

## logitr 0.1.4

- Added
  [`predictChoices()`](https://jhelvy.github.io/logitr/reference/predictChoices.md)
  function.
- Added `predictShares()` function, depreciating
  [`simulateShares()`](https://jhelvy.github.io/logitr/reference/simulateShares.md).

## logitr 0.1.3

- Modified the
  [`recodeData()`](https://jhelvy.github.io/logitr/reference/recodeData.md)
  and
  [`dummyCode()`](https://jhelvy.github.io/logitr/reference/dummyCode.md)
  functions for improved speed.
- Updated
  [`simulateShares()`](https://jhelvy.github.io/logitr/reference/simulateShares.md)
  to work with the automatic dummy coding from the revised
  [`recodeData()`](https://jhelvy.github.io/logitr/reference/recodeData.md)
  and
  [`dummyCode()`](https://jhelvy.github.io/logitr/reference/dummyCode.md)
  functions.
- Added support for
  [`simulateShares()`](https://jhelvy.github.io/logitr/reference/simulateShares.md)
  to compute shares for multiple sets of alternatives.
- Added tests for encoding functions
- Added covariance matrix to model export

### Bugs

- When simulating shares from a WTP model, only accepted a price named
  “price” rather than something else such as “Price” - fixed this.
- In
  [`simulateShares()`](https://jhelvy.github.io/logitr/reference/simulateShares.md),
  the shares were not correctly computed with a WTP space model because
  price was still being multiplied by -1. This has been corrected.
- Changes to automatic dummy coding were accidentally ignoring factor
  levels - that’s been fixed.

## logitr 0.1.2

- Fixed bug where model with single variable would error due to a matrix
  being converted to a vector in the `standardDraws()` function
- Fixed bug in `getCatVarDummyNames()` - previously used string
  matching, which can accidentally match with other similarly-named
  variables.
- Fixed bug in [`rowsum()`](https://rdrr.io/r/base/rowsum.html) where
  the `reorder` argument was set to `TRUE`, which resulted in wrong
  logit calculations unless the `obsID` happened to be already sorted.

## logitr 0.1.1

- Changed how failures to converge are handled. Previously would
  continue to run a while loop. Now it fails and records the failure,
  along with appropriate changes in summary() and coef().
- Re-defined the wtp space utility models as B*X - p. Before it was p +
  B*X and p was re-defined as -1\*p.
- If tidyverse library is loaded, data frames were getting converted to
  tibbles, which broke some things. Fixed this by forcing the input data
  to be a data.frame()

## logitr 0.1.0

CRAN release: 2021-01-19

### Summary of larger updates:

- v0.1.0 Submitted to CRAN!

### Summary of smaller updates:

- Reduced the length of the title in DESCRIPTION to less than 65
  characters.
- Changed package names in title and description to single quotes, e.g:
  {nloptr} -\> ‘nloptr’
- Added reference in description with doi to Train (2009) “Discrete
  Choice Methods with Simulation, 2nd Edition”.
- Added statements to dummyCode.Rd and statusCodes.Rd
- Added statements to dummyCode.Rd and statusCodes.Rd.
- Updated description for summary.logitr.Rd.
- Modified multiple functions to use message()/warning() instead of
  print()/cat().
- Added `algorithm` to the `options` input, with the default being set
  to `"NLOPT_LD_LBFGS"`.

### Bugs

- Fixed tiny bug in `getParTypes()` function - previously was not
  returning the correct `parNames` for continuous vs. discrete
  variables.
- Added an input check to make sure the modelSpace argument is either
  `"pref"` or `"wtp"`.
- Added an input check to make sure the `priceName` argument is only
  used when the `modelSpace` argument is set to `"wtp"`.

## logitr 0.0.5

### Summary of larger updates:

- Added support for auto creating interactions among variables
- exported `getCoefTable()` function

### Summary of smaller updates:

- Added new documentation for prepping data:
  - overall structure
  - dummyCode() function
  - handling interactions
- All vignettes proof-read with lots of small changes to examples
- Added a hex sticker

## logitr 0.0.4

Weighted models, new dataset, new encoding features

### Summary of larger updates:

- Added support for estimating weighted regressions
- Added and improved documentation for new datasets: `yogurt`,
  `cars_china`, `cars_us`
- Exported the
  [`dummyCode()`](https://jhelvy.github.io/logitr/reference/dummyCode.md)
  function for automatically creating dummy-coded variables in a data
  frame.
- Added support for auto dummy-coding categorical variables prior to
  model estimation
- Major overhaul of documentation using {pkgdown}

### Summary of smaller updates:

- Changed license to MIT (after doing a bit of reading up on this)
- Fixed dimension-matching issue with user-provided draws for mixed
  logit models
- Fixed bug in `modelInputs` where `obsID` was not a vector for tibble
  inputs
- Added placeholder hex sticker

## logitr 0.0.3

New simulation functionality

### Summary of larger updates:

- Added support for simulating shares for a set of alternatives given an
  estimated model:
  [`simulateShares()`](https://jhelvy.github.io/logitr/reference/simulateShares.md).
  This is similar to the
  [`predict()`](https://rdrr.io/r/stats/predict.html) function in
  mlogit.
- Removed support for using an estimated preference space model as an
  input in the [`options()`](https://rdrr.io/r/base/options.html)
  function. I found this just far too confusing, and instead encourage
  users to supply a WTP space model with the computed WTP from a
  preference space model as starting parameters.

### Summary of smaller updates:

- Updated the [`summary()`](https://rdrr.io/r/base/summary.html) and
  main [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  functions to keep the basic information (run \#, log-likelihood value,
  number of iterations, and output status) whenever `numMultistarts`
  \> 1. Previously this information was only kept if `keepAllRuns` was
  set to `TRUE`.

## logitr 0.0.2

Updates to options and a few small bug fixes

### Summary of larger updates:

- I got rid of the `logitr.summary()` function and instead added the
  `logitr` class to all the models and renamed the summary function to
  [`summary.logitr()`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md).
  Now you can just use the standard
  [`summary()`](https://rdrr.io/r/base/summary.html) function to
  summarize model results.
- I finally fixed the analytic gradient for WTP space MXL models. I
  tested analytic versus numeric for WTP space and Preference Space MXL
  models and they are all identical, including variations of using
  normally and log-normally distributed parameters.
- Added startParBounds as an argument in options.

### Smaller updates:

- Changed the summary() function to print more digits in the summary
  table.
- Rounded printing of the elapsed time in the summary table.
- Forced the sigma values in MXL models to be positive using abs().
  Negative values for sigma parameters should not be an issue because
  the standard normal is symmetric.
- Changed the summary of random parameters to show “summary of 10k
  draws”
- Updated hessian to always use numeric approx for SE calculation since
  it’s faster.
- Made scaleInputs default to `TRUE`.

### Bugs fixed:

- If the prefSpaceModel was a multistart, it was grabbing the correct
  bestModel for the WTP calculations, but not the logLik value. Now it’s
  getting the right logLik value too.
- Fixed a bug with the scaling option where it was blowing up to use
  scaling numbers.

## logitr 0.0.1

Full reboot of logitr!

Long overdue, I decided to give the logitr program a full overhaul. This
is the first version that is compiled as a proper R package that can be
directly installed from GitHub. This version is much more robust and
flexible than the prior, clunky collection of R files that I had
previously been using to estimate logit models.
