# logitr 0.1.0

## Summary of larger updates:

- logitr 0.1.0 now on CRAN.

## Summary of smaller updates:

* Reduced the length of the title in DESCRIPTION to less than 65 characters.
* Changed package names in title and description to single quotes, e.g:  {nloptr} -> 'nloptr'
* Added reference in description with doi to Train (2009) "Discrete Choice Methods with Simulation, 2nd Edition" <doi:10.1017/CBO9780511805271>.
* Added \value statements to dummyCode.Rd and statusCodes.Rd
* Added \value statements to dummyCode.Rd and statusCodes.Rd.
* Updated \value description for summary.logitr.Rd.
* Modified multiple functions to use message()/warning() instead of print()/cat().
* Added `algorithm` to the `options` input, with the default being set to `"NLOPT_LD_LBFGS"`.

## Bugs

* Fixed tiny bug in `getParTypes()` function - previously was not returning the correct `parNames` for continuous vs. discrete variables.
* Added an input check to make sure the modelSpace argument is either `"pref"` or `"wtp"`.
* Added an input check to make sure the `priceName` argument is only used when the `modelSpace` argument is set to `"wtp"`.
