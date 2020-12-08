
# logitr 0.0.5

## Summary of larger updates:

- Added support for auto creating interactions amongst variables
- exported `getCoefTable()` function

## Summary of smaller updates:

- Added new documentation for prepping data:
    - overall structure
    - dummyCode() function
    - handling interactions
- All vignettes proof-read with lots of small changes to examples
- Added a hex sticker




# logitr 0.0.4

Weighted models, new dataset, new encoding features

## Summary of larger updates:

- Added support for estimating weighted regressions
- Added and improved documentation for new datasets: `yogurt`, `cars_china`, `cars_us`
- Exported the `dummyCode()` function for automatically creating dummy-coded variables in a data frame.
- Added support for auto dummy-coding categorical variables prior to model estimation
- Major overhaul of documentation using {pkgdown}

## Summary of smaller updates:

- Changed license to MIT (after doing a bit of reading up on this)
- Fixed dimension-matching issue with user-provided draws for mixed logit models
- Fixed bug in `modelInputs` where `obsID` was not a vector for tibble inputs
- Added placeholder hex sticker



# logitr 0.0.3

New simulation functionality

## Summary of larger updates:

- Added support for simulating shares for a set of alternatives given an estimated model: `simulateShares()`. This is similar to the `predict()` function in mlogit.
- Removed support for using an estimated preference space model as an input in the `options()` function. I found this just far too confusing, and instead encourage users to supply a WTP space model with the computed WTP from a preference space model as starting parameters.

## Summary of smaller updates:

- Updated the `summary()` and main `logitr()` functions to keep the basic information (run #, log-likelihood value, number of iterations, and output status) whenever `numMultistarts` > 1.  Previously this information was only kept if `keepAllRuns` was set to `TRUE`.



# logitr 0.0.2

Updates to options and a few small bug fixes

## Summary of larger updates:

- I got rid of the `logitr.summary()` function and instead added the `logitr` class to all the models and renamed the summary function to `summary.logitr()`. Now you can just use the standard `summary()` function to summarize model results.
- I finally fixed the analytic gradient for WTP space MXL models. I tested analytic versus numeric for WTP space and Preference Space MXL models and they are all identical, including variations of using normally and log-normally distributed parameters.
- Added startParBounds as an argument in options.

## Smaller updates:

- Changed the summary() function to print more digits in the
summary table.
- Rounded printing of the elapsed time in the summary table.
- Forced the sigma values in MXL models to be positive using abs(). Negative values for sigma parameters should not be an issue because the standard normal is symmetric.
- Changed the summary of random parameters to show "summary of 10k draws"
- Updated hessian to always use numeric approx for SE calculation since it's faster.
- Made scaleInputs default to `TRUE`.

## Bugs fixed:

- If the prefSpaceModel was a multistart, it was grabbing the correct
bestModel for the WTP calculations, but not the logLik value. Now it's
getting the right logLik value too.
- Fixed a bug with the scaling option where it was blowing up to use scaling numbers.



# logitr 0.0.1

Full reboot of logitr!

Long overdue, I decided to give the logitr program a full overhaul. This is the first version that is compiled as a proper R package that can be directly installed from Github. This version is much more robust and flexible than the prior, clunky collection of R files that I had previously been using to estimate logit models.
