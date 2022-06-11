# ============================================================================
# Functions for checking inputs and setting up default options
# ============================================================================

runInputChecks <- function(data, inputs) {
  if (! is.null(inputs$price)) {
    if (inputs$price %in% inputs$pars) {
      stop(
        'The value provided for the "price" argument is also included ',
        'in the "pars" argument. If you are estimating a WTP space model',
        ', you should remove the price name from the "pars" argument and ',
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

  # Check that randPars names match those in pars
  missing <- setdiff(names(inputs$randPars), inputs$pars)
  if (length(missing) > 0) {
    stop(
      missing[1], " provided in 'randPars' is missing from 'pars'"
    )
  }

  # Check if any of the argument names are missing in the data
  dataColumnNames <- colnames(data)
  missingInData(inputs$panelID, "panelID", dataColumnNames)
  missingInData(inputs$clusterID, "clusterID", dataColumnNames)
  missingInData(inputs$weights, "weights", dataColumnNames)
  missingInData(names(inputs$randPars), "randPars", dataColumnNames)

  # Separate out pars with and without interactions
  ints <- grepl("\\*", inputs$pars)
  parsInt <- inputs$pars[ints == TRUE]
  parsNoInt <- inputs$pars[ints == FALSE]
  if (length(parsInt) > 0) {
    parsInt <- unique(unlist(strsplit(parsInt, "\\*")))
  }
  missingInData(c(parsInt, parsNoInt), "pars", dataColumnNames)

  # Make sure the number of multistarts and numDraws are positive
  if (inputs$numMultiStarts < 1) {
    stop('"numMultiStarts" must be a positive integer')
  }
  if (inputs$numDraws < 1) {
    stop('"numDraws" must be a positive integer')
  }

  # If using correlation, make sure that there are at least 2 random pars
  if (inputs$correlation) {
    if (length(inputs$randPars) < 2) {
      stop(
        "If correlation = TRUE, you must have at least two random parameters ",
        "in the 'randPars' argument"
      )
    }
  }

}

missingInData <- function(vals, var, dataColumnNames) {
  if (! is.null(vals)) {
    test <- ! vals %in% dataColumnNames
    if (any(test)) {
      missing <- paste(vals[which(test)], collapse = ", ")
      stop(
        'The following specified names for "', var, '" are missing in the ',
        'data:\n', missing
      )
    }
  }
}

# Need to check if the user-provided list of options omits any of these
# options as they are required for the optimizer
checkOptions <- function(options) {
  if (is.null(options$print_level)) {
    options$print_level <- 0
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
  return(options)
}

predictInputsCheck <- function(object, newdata, obsID, price, type, ci) {
  if (!is_logitr(object)) {
    stop(
      'The "object" argument must be a object estimated using the logitr() ',
      'function.'
    )
  }
  if (missing(newdata)) stop('"newdata" needs to be specified')
  if (!is.null(newdata)) {
    if (is.null(obsID)) {
      stop('"obsID" must be specified if newdata is not NULL')
    }
    if (object$inputs$modelSpace == "wtp") {
      if (is.null(price)) {
        stop(
          '"price" must be specified if "object" is a WTP space model and ',
          'newdata is not NULL'
        )
      }
    }
    if (!is.null(obsID)) {
      if (! obsID %in% names(newdata)) {
        stop(
          'The "obsID" argument refers to a column that does not exist in ',
          'the "newdata" data frame'
        )
      }
    }
    if (!is.null(price)) {
      if (! price %in% names(newdata)) {
        stop(
          'The "price" argument refers to a column that does not exist in ',
          'the "newdata" data frame'
        )
      }
    }
  }
  if ("probs" %in% type) {
    stop('Use "prob" instead of "probs" in the type argument')
  }
  if ("outcomes" %in% type) {
    stop('Use "outcome" instead of "outcomes" in the type argument')
  }
  typeTest <- identical(type, "prob") |
    identical(type, "outcome") |
    identical(type, c("prob", "outcome")) |
    identical(type, c("outcome", "prob"))
  if (!typeTest) {
    stop(
      'type must be a vector containing "prob" (for returning ',
      'predicted probabilities) and / or "outcome" (for returning predicted ',
      'outcomes)')
  }
  if (!is.null(ci)) {
    ci_test <- (ci < 1) & (ci > 0)
    if (!ci_test) {
      stop("ci must be a number between 0 and 1")
    }
  }
}

predictParCheck <- function(model, X) {
  modelPars <- names(model$parSetup)
  if (model$inputs$modelSpace == "wtp") {
    # Drop lambda parameter
    modelPars <- modelPars[2:length(modelPars)]
  }
  dataNames <- colnames(X)
  if (length(setdiff(modelPars, dataNames)) > 0) {
    modelPars <- paste(modelPars, collapse = ", ")
    dataPars <- paste(dataNames, collapse = ", ")
    stop(paste0(
      'The coefficient names for the provided model do not correspond to ',
      'variables in "newdata".\n\n',
      'Expect columns:\n\t', modelPars, '\n\n',
      'Encoded column names from provided `newdata` object:\n\t', dataPars,
      '\n\n',
      'If you have a factor variable in "newdata", check that the factor ',
      'levels match those of the data used to estimate the model.'
    ))
  }
}

wtpInputsCheck <- function(model, price) {
  if (missing(model)) stop('"model" needs to be specified')
  if (missing(price)) stop('"price" needs to be specified')
  if (!is_logitr(model)) {
    stop('model must be a model estimated using the logitr() function.')
  }
  if (! price %in% names(stats::coef(model))) {
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
  if (! price %in% names(stats::coef(model_pref))) {
    stop('"price" must be the name of a coefficient in "model_pref"')
  }
}
