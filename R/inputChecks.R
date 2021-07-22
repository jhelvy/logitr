# ============================================================================
# Functions for checking inputs and setting up default options
# ============================================================================

runInputChecks <- function(
  data, choiceName, obsIDName, parNames, randPars, priceName, randPrice,
  modelSpace, weightsName, clusterName
) {
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

  dataColumnNames <- colnames(data)
  # Check cluster name
  if (! is.null(clusterName)) {
    if (! clusterName %in% dataColumnNames) {
      stop(
        'You have specified a cluster name that is not present in the data ',
        'provided:\n', as.character(clusterName),
        '\nPlease double-check the provided data/cluster name.'
      )
    }
  }

  # Check weights name
  if (! is.null(weightsName)) {
    if (! weightsName %in% dataColumnNames) {
      stop(
        'You have specified a weights name that is not present in the data provided:\n',
        as.character(weightsName),
        '\nPlease double-check the provided data/weights name.'
      )
    }
  }

  # Check all parameter names - fixed, no interactions
  parNamesNoInt <- parNames[grepl("\\*", parNames) == FALSE]

  if (length(parNamesNoInt) > 0) {
    missingFixedPars <- c()
    for (parName in parNamesNoInt) {
      if (! parName %in% dataColumnNames) {
        missingFixedPars <- c(missingFixedPars, parName)
      }
    }

    if (length(missingFixedPars) > 0) {
      stop(
        'You have specified a fixed parameter name(s) that is/are not present ',
        'in the data provided:\n', as.list(missingFixedPars),
        '\nPlease double-check the provided data/fixed parameter name(s).'
      )
    }
  }

  # Check all parameter names - fixed, with interactions
  intNames <- parNames[grepl("\\*", parNames) == TRUE]

  if (length(intNames) > 0) {
    intNames <- unique(unlist(strsplit(intNames, "\\*")))
    missingIntPars <- c()
    for (parName in intNames) {
      if (! parName %in% dataColumnNames) {
        missingIntPars <- c(missingIntPars, parName)
      }
    }

    if (length(missingIntPars) > 0) {
      stop(
        'You have specified an interaction parameter name(s) that is/are ',
        'not present in the data provided:\n', as.list(missingIntPars),
        '\nPlease double-check the provided data / parameter name(s).'
      )
    }
  }

  # Check all parameter names - random
  if (! is.null(randPars)) {
    missingRandPars <- c()
    for (parName in names(randPars)) {
      if (! parName %in% dataColumnNames) {
        missingRandPars <- c(missingFixedPars, parName)
      }

      if (length(missingRandPars) > 0) {
      stop(
        'You have specified a random parameter name(s) that is/are not ',
        'present in the data provided:\n', as.list(missingRandPars),
        '\nPlease double-check the provided data/random parameter name(s).'
      )
      }
    }
  }

}

runOptionsChecks <- function(options, parNameList) {
  # Set default option values
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

predictInputsCheck <- function(model, alts, altIDName, obsIDName) {
  if (!is_logitr(model)) {
    stop(
      'The "model" argument must be a model estimated using the logitr() ',
      'function.'
    )
  }
  if (missing(alts)) stop("alts needs to be specified")
  if (missing(altIDName)) stop("altIDName needs to be specified")
  if (! altIDName %in% names(alts)) {
    stop(
      'The "altIDName" argument refers to a column that does not exist in ',
      'the "alts" data frame')
  }
  if (!is.null(obsIDName)) {
    if (! obsIDName %in% names(alts)) {
      stop(
        'The "obsIDName" argument refers to a column that does not exist in ',
        'the "alts" data frame')
    }
  }
}
