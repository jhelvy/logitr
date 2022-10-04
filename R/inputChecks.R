# ============================================================================
# Functions for checking inputs and setting up default options
# ============================================================================

runInputChecks <- function(data, inputs) {
  if (! is.null(inputs$scalePar)) {
    if (inputs$scalePar %in% inputs$pars) {
      stop(
        'The value provided for the "scalePar" argument is also included ',
        'in the "pars" argument. If you are estimating a WTP space model',
        ', you should remove the "scalePar" name from the "pars" argument.'
      )
    }
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

  # Make sure the drawType is either 'halton' or 'sobol'
  if (! inputs$drawType %in% c('halton', 'sobol')) {
    stop("drawType must be either 'halton' or 'sobol'")
  }

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

predictInputsCheck <- function(object, newdata, obsID, type, ci) {
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
    if (!is.null(obsID)) {
      if (! obsID %in% names(newdata)) {
        stop(
          'The "obsID" argument refers to a column that does not exist in ',
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
  if (model$modelSpace == "wtp") {
    # Drop scale parameter (scalePar)
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

wtpInputsCheck <- function(model, scalePar) {
  if (missing(model)) stop('"model" needs to be specified')
  if (missing(scalePar)) stop('"scalePar" needs to be specified')
  if (!is_logitr(model)) {
    stop('"model" must be an object of class "logitr".')
  }
  if (model$modelSpace != "pref") {
    stop('model must be a preference space model.')
  }
}

wtpCompareInputsCheck <- function(model_pref, model_wtp, scalePar) {
  if (missing(model_pref)) stop('"model_pref" needs to be specified')
  if (missing(model_wtp)) stop('"model_wtp" needs to be specified')
  if (missing(scalePar)) stop('"scalePar" needs to be specified')
  if (!is_logitr(model_pref)) {
    stop('"model_pref" must be an object of class "logitr".')
  }
  if (!is_logitr(model_wtp)) {
    stop('"model_wtp" must be an object of class "logitr".')
  }
  if (model_pref$modelSpace != "pref") {
    stop('"model_pref" must be a preference space model.')
  }
  if (model_wtp$modelSpace != "wtp") {
    stop('"model_wtp" must be a preference space model.')
  }
}
