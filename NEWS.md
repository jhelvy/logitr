# logitr 1.2.0

## Summary of larger updates:

* Added support for simulating shares for a set of alternatives given an estimated model: `simulateShares()`. This is similar to the `predict()` function in mlogit.
* Removed support for using an estimated preference space model as an input in the `options()` function. I found this just far too confusing, and instead encourage users to supply a WTP space model with the computed WTP from a preference space model as starting parameters.

## Summary of smaller updates:

* Updated the `summary()` and main `logitr()` functions to keep the basic information (run #, log-likelihood value, number of iterations, and output status) whenever `numMultistarts` > 1.  Previously this information was only kept if `keepAllRuns` was set to `TRUE`.
