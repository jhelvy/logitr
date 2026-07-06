# Package-level documentation and NAMESPACE directives.
#
# This file became necessary when the C++ backend was added. The roxygen
# block below generates the NAMESPACE entries that load the compiled code:
#   - @useDynLib loads the package's compiled shared library (without it,
#     every .Call() to the C++ backend would fail)
#   - the @importFrom lines ensure Rcpp's and RcppParallel's native
#     libraries are loaded first
# Pure-R packages don't need this file.
#
# The "_PACKAGE" sentinel below also generates the logitr-package.Rd help
# page (shown by ?logitr), auto-filled from DESCRIPTION.

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib logitr, .registration = TRUE
#' @importFrom Rcpp sourceCpp
#' @importFrom RcppParallel RcppParallelLibs
## usethis namespace: end
NULL
