# ============================================================================
# Functions for checking inputs and setting up default options
# ============================================================================

runInputChecks <- function(data, choiceName, obsIDName, parNames, randPars,
  priceName, randPrice, modelSpace, weightsName) {
  if (! is.null(priceName)) {
    if (priceName %in% parNames) {
      stop(
        'The value you provided for the "priceName" argument is also included ',
        'in your "parNames" argument. If you are estimating a WTP space model',
        ', you should remove the price name from your "parNames" argument and ',
        'provide it separately with the "priceName" argument.'
      )
    }
    if (modelSpace != "wtp") {
      stop(
        'The "priceName" argument should only be used for WTP space models. ',
        'Please either set the "modelSpace" argument to "wtp" or remove the ',
        '"priceName" argument.'
      )
    }
  }
  if (! modelSpace %in% c('pref', 'wtp')) {
    stop(
      'The modelSpace argument must be set to either "pref" or "wtp", all ',
      'lower case (defaults to "pref").'
    )
  }
  if ((modelSpace == 'wtp') & is.null(priceName)) {
    stop(
      'You are estimating a WTP space model but have not provided a ',
      '"priceName" argument. Please set "priceName" equal to the name of the ',
      'column in your data frame that represents "price".'
    )
  }
}

runOptionsChecks <- function(options, parNameList) {
  # Run checks for all options
  if (is.null(options$numMultiStarts)) {
    options$numMultiStarts <- 1
  }
  if (options$numMultiStarts < 1) {
    options$numMultiStarts <- 1
  }
  if (is.null(options$keepAllRuns)) {
    options$keepAllRuns <- FALSE
  }
  if (is.null(options$useAnalyticGrad)) {
    options$useAnalyticGrad <- TRUE
  }
  if (is.null(options$scaleInputs)) {
    options$scaleInputs <- TRUE
  }
  if (is.null(options$startParBounds)) {
    options$startParBounds <- c(-1, 1)
  }
  if (is.null(options$standardDraws)) {
    options$standardDraws <- NULL
  }
  if (is.null(options$numDraws)) {
    options$numDraws <- 50
  }
  if (is.null(options$printLevel)) {
    options$printLevel <- 0
  }
  if (is.null(options$xtol_rel)) {
    options$xtol_rel <- 1.0e-6
  }
  if (is.null(options$xtol_abs)) {
    options$xtol_abs <- 1.0e-6
  }
  if (is.null(options$ftol_rel)) {
    options$ftol_rel <- 1.0e-6
  }
  if (is.null(options$ftol_abs)) {
    options$ftol_abs <- 1.0e-6
  }
  if (is.null(options$maxeval)) {
    options$maxeval <- 1000
  }
  if (is.null(options$algorithm)) {
    options$algorithm <- "NLOPT_LD_LBFGS"
  }
  if (is.null(options$startVals)) {
    options$startVals <- NULL
  } else {
    names(options$startVals) <- parNameList$all
  }
  return(options)
}
