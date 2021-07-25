# ============================================================================
# Functions for checking inputs and setting up default options
# ============================================================================

runInputChecks <- function(data, inputs) {
  if (! is.null(inputs$price)) {
    if (inputs$price %in% inputs$pars) {
      stop(
        'The value you provided for the "price" argument is also included ',
        'in your "pars" argument. If you are estimating a WTP space model',
        ', you should remove the price name from your "pars" argument and ',
        'provide it separately with the "price" argument.'
      )
    }
    if (inputs$modelSpace != "wtp") {
      stop(
        'The "price" argument should only be used for WTP space models. ',
        'Please either set the "modelSpace" argument to "wtp" or remove the ',
        '"price" argument.'
      )
    }
  }
  if (! inputs$modelSpace %in% c('pref', 'wtp')) {
    stop(
      'The "modelSpace" argument must be set to either "pref" or "wtp", all ',
      'lower case (defaults to "pref").'
    )
  }
  if ((inputs$modelSpace == 'wtp') & is.null(inputs$price)) {
    stop(
      'You are estimating a WTP space model but have not provided a ',
      '"price" argument. Please set "price" equal to the name of the ',
      'column in your data frame that represents "price".'
    )
  }

  dataColumnNames <- colnames(data)

  # Check cluster name
  if (! is.null(inputs$cluster)) {
    if (! inputs$cluster %in% dataColumnNames) {
      stop(
        'You have specified a cluster name that is not present in the data ',
        'provided:\n', as.character(inputs$cluster),
        '\nPlease double-check the provided argument for "cluster".'
      )
    }
  }

  # Check weights name
  if (! is.null(inputs$weights)) {
    if (! inputs$weights %in% dataColumnNames) {
      stop(
        'You have specified a weights name that is not present in the data ',
        'provided:\n', as.character(inputs$weights),
        '\nPlease double-check the provided argument for "weights".'
      )
    }
  }

  # Separate out pars with and without interactions
  ints <- grepl("\\*", inputs$pars)
  parsInt <- inputs$pars[ints == TRUE]
  parsNoInt <- inputs$pars[ints == FALSE]

  # Check if provided pars are in the data
  if (length(parsNoInt) > 0) {
    missingFixedPars <- c()
    for (par in parsNoInt) {
      if (! par %in% dataColumnNames) {
        missingFixedPars <- c(missingFixedPars, par)
      }
    }
    if (length(missingFixedPars) > 0) {
      stop(
        'You have specified a fixed parameter name(s) that is/are not present ',
        'in the data provided:\n', as.list(missingFixedPars),
        '\nPlease double-check the provided argument for "pars".'
      )
    }
  }
  if (length(parsInt) > 0) {
    parsInt <- unique(unlist(strsplit(parsInt, "\\*")))
    missingIntPars <- c()
    for (par in parsInt) {
      if (! par %in% dataColumnNames) {
        missingIntPars <- c(missingIntPars, par)
      }
    }
    if (length(missingIntPars) > 0) {
      stop(
        'You have specified an interaction parameter name(s) that is/are ',
        'not present in the data provided:\n', as.list(missingIntPars),
        '\nPlease double-check the provided argument for "pars".'
      )
    }
  }

  # Check all random parameter names
  if (! is.null(inputs$randPars)) {
    missingRandPars <- c()
    for (par in names(inputs$randPars)) {
      if (! par %in% dataColumnNames) {
        missingRandPars <- c(missingFixedPars, par)
      }
      if (length(missingRandPars) > 0) {
      stop(
        'You have specified a random parameter name(s) that is/are not ',
        'present in the data provided:\n', as.list(missingRandPars),
        '\nPlease double-check the provided argument for "randPars".'
      )
      }
    }
  }

}

runOptionsChecks <- function(options, parList) {
  # Set default option values
  if (is.null(options$numMultiStarts)) {
    options$numMultiStarts <- 1
  }
  if (options$numMultiStarts < 1) {
    options$numMultiStarts <- 1
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
    names(options$startVals) <- parList$all
  }
  return(options)
}

predictInputsCheck <- function(model, alts, altID, obsID) {
  if (!is_logitr(model)) {
    stop(
      'The "model" argument must be a model estimated using the logitr() ',
      'function.'
    )
  }
  if (missing(alts)) stop('"alts" needs to be specified')
  if (missing(altID)) stop('"altID" needs to be specified')
  if (! altID %in% names(alts)) {
    stop(
      'The "altID" argument refers to a column that does not exist in ',
      'the "alts" data frame')
  }
  if (!is.null(obsID)) {
    if (! obsID %in% names(alts)) {
      stop(
        'The "obsID" argument refers to a column that does not exist in ',
        'the "alts" data frame')
    }
  }
}

wtpInputsCheck <- function(model, price) {
  if (missing(model)) stop('"model" needs to be specified')
  if (missing(price)) stop('"price" needs to be specified')
  if (!is_logitr(model)) {
    stop('model must be a model estimated using the logitr() function.')
  }
  if (! price %in% names(model$coef)) {
    stop('"price" must be the name of a coefficient in "model".')
  }
  if (model$inputs$modelSpace != "pref") {
    stop('model must be a preference space model.')
  }
}

wtpCompareInputsCheck <- function(model_pref, model_wtp, price) {
  if (missing(model_pref)) stop('"model_pref" needs to be specified')
  if (missing(model_wtp)) stop('"model_wtp" needs to be specified')
  if (missing(price)) stop('"price" needs to be specified')
  if (!is_logitr(model_pref)) {
    stop('"model_pref" must be a model estimated using the logitr() function.')
  }
  if (!is_logitr(model_wtp)) {
    stop('"model_wtp" must be a model estimated using the logitr() function.')
  }
  if (! price %in% names(model_pref$coef)) {
    stop('"price" must be the name of a coefficient in "model_pref"')
  }
}
