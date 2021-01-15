## Resubmission
This is a resubmission. In this version I have:

* Removed the doi notation error from the NEWS.md.
* Reduced the length of the title to less than 65 characters.
* Changed package names in title and description to single quotes, e.g:  {nloptr} -> 'nloptr'
* Added a reference in the description with doi to Train (2009) "Discrete Choice Methods with Simulation, 2nd Edition".
* Added \value statements to dummyCode.Rd and statusCodes.Rd.
* Updated \value description for summary.logitr.Rd.
* Modified multiple functions to use message()/warning() instead of print()/cat()
* Added `algorithm` to the `options` input, with the default being set to `"NLOPT_LD_LBFGS"`.

## Bugs fixed
* Fixed tiny bug in `getParTypes()` function - previously was not returning the correct `parNames` for continuous vs. discrete variables.
* Added an input check to make sure the modelSpace argument is either `"pref"` or `"wtp"`.
* Added an input check to make sure the `priceName` argument is only used when the `modelSpace` argument is set to `"wtp"`.

## Test environments
* local R installation, R 4.0.2
* ubuntu 16.04 (on travis-ci), R 4.0.2
* win-builder (devel)

## Other notes
Thanks for the recommended changes! This is the first package I've ever submit to CRAN, and I'm still learning best practices. Making these changes actually helped me identify a few small bugs.

## R CMD check results
0 errors | 0 warnings | 1 notes

* This is a resubmission.
