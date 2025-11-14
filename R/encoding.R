# ============================================================================
# Functions for encoding the data to prep for estimation
# ============================================================================

#' Add dummy-coded variables to data frame.
#'
#' This function is depreciated. Use fastDummies::dummy_cols() instead.
#' @param df A data frame.
#' @param vars The variables in the data frame for which you want to
#' create new dummy coded variables.
#' @return A a dataframe with new dummy-coded variables added.
#' @keywords internal
#' @export
dummyCode <- function(df, vars) {
  # v0.1.3
  .Deprecated("fastDummies::dummy_cols")
}

#' Returns a list of the design matrix `X` and updated `pars` and
#' `randPars` to include any dummy-coded categorical or interaction
#' variables.
#'
#' Recodes the data and returns a list of the encoded design matrix (`X`) as
#' well as two vectors (`pars` and `randPars`) with discrete (categorical)
#' variables and interaction variables added to `X`, `pars`, and
#' `randPars`.
#' @param data The data, formatted as a `data.frame` object.
#' @param pars The names of the parameters to be estimated in the model.
#' Must be the same as the column names in the `data` argument. For WTP space
#' models, do not include price in `pars` - it should instead be defined by
#' the `scalePar` argument.
#' @param randPars A named vector whose names are the random parameters and
#' values the distribution: `'n'` for normal or `'ln'` for log-normal.
#' Defaults to `NULL`.
#' @return A list of the design matrix (`X`) and two vectors (`pars` and
#' `randPars`) with discrete (categorical) variables and interaction variables
#' added.
#' @export
#' @examples
#' library(logitr)
#'
#' data(yogurt)
#'
#' # Recode the yogurt data
#' result <- recodeData(
#'     data = yogurt,
#'     pars = c("price", "feat", "brand", "price*brand"),
#'     randPars = c(feat = "n", brand = "n")
#' )
#'
#' result$formula
#' result$pars
#' result$randPars
#' head(result$X)
recodeData <- function(data, pars, randPars) {
  data <- as.data.frame(data) # tibbles break things
  factorLevels <- getFactorLevels(data, pars) # need to store for predicting
  data <- orderedFactorsToChars(data) # ordered factors cause weird names
  formula <- stats::as.formula(paste0("~ ", paste(pars, collapse = " + ")))
  X <- getDesignMatrix(formula, data)
  return(list(
    formula = formula,
    X = X,
    pars = colnames(X),
    factorLevels = factorLevels,
    randPars = recodeRandPars(data, pars, randPars)
  ))
}

# Ordered factors have strange returned column names when encoding with
# model.matrix, so convert them to characters
orderedFactorsToChars <- function(data) {
  types <- getColumnTypes(data)
  names <- names(types[types == "ordered"])
  if (length(names) > 0) {
    for (i in 1:length(names)) {
      data[, names[i]] <- factor(data[, names[i]], ordered = FALSE)
    }
  }
  return(data)
}

getColumnTypes <- function(data) {
  return(unlist(lapply(lapply(data, class), function(x) x[1])))
}

getFactorLevels <- function(data, pars) {
  # Separate out interactions
  ints <- grepl("\\*", pars)
  if (any(ints)) {
    pars_int <- pars[ints == TRUE]
    pars_int <- unlist(lapply(pars_int, function(x) strsplit(x, "\\*")))
    pars <- unique(c(pars[ints == FALSE], pars_int))
  }
  factorLevels <- NULL
  parTypes <- getParTypes(data, pars)
  discrete <- parTypes$discrete
  if (!is.null(discrete)) {
    factorLevels <- lapply(discrete, function(x) levels(as.factor(data[, x])))
    names(factorLevels) <- discrete
  }
  return(factorLevels)
}

getDesignMatrix <- function(formula, data) {
  tmp <- stats::model.matrix(formula, data)
  X <- tmp[, -1, drop = FALSE] # Drop intercept
  attr(X, "contrasts") <- attr(tmp, "contrasts")
  attr(X, "assign") <- attr(tmp, "assign")[-1]
  return(X)
}

recodeRandPars <- function(data, pars, randPars) {
  # Separate out interactions
  ints <- grepl("\\*", pars)
  if (any(ints)) {
    pars <- pars[ints == FALSE]
  }
  # Dummy code categorical variables (if any exist)
  parTypes <- getParTypes(data, pars)
  if (!is.null(parTypes$discrete)) {
    dummyLevels <- getDummyLevels(data, parTypes)
    for (i in seq_len(length(dummyLevels))) {
      name <- names(dummyLevels)[i]
      dummyNames <- dummyLevels[[name]]
      if (name %in% names(randPars)) {
        matchID <- which(names(randPars) == name)
        newRandPars <- rep(randPars[matchID], length(dummyNames))
        names(newRandPars) <- dummyNames
        randPars <- randPars[-matchID]
        randPars <- c(randPars, newRandPars)
      }
    }
  }
  return(randPars)
}

getParTypes <- function(df, pars) {
  types <- getColumnTypes(df[pars])
  discIDs <- which(types %in% c("character", "factor"))
  continuous <- pars[setdiff(seq_len(length(pars)), discIDs)]
  if (length(continuous) == 0) {
    continuous <- NULL
  }
  discrete <- pars[discIDs]
  if (length(discrete) == 0) {
    discrete <- NULL
  }
  return(list(continuous = continuous, discrete = discrete))
}

getDummyLevels <- function(data, parTypes) {
  discrete <- parTypes$discrete
  pars <- list()
  for (i in seq_len(length(discrete))) {
    name <- discrete[i]
    var <- data[, name]
    parLevels <- levels(as.factor(var))
    dummyNames <- paste0(name, parLevels)[-1]
    pars[[i]] <- dummyNames
  }
  names(pars) <- discrete
  return(pars)
}

#' Validate data formatting for logitr models
#'
#' This function checks that data is properly formatted for use with logitr()
#' and provides detailed diagnostic information about potential issues.
#'
#' @param data The data frame to validate
#' @param outcome The name of the column that identifies the outcome variable
#' @param obsID The name of the column that identifies each observation
#' @param pars Optional. The names of parameters to check (for additional validation)
#' @param scalePar Optional. The name of the scale parameter column (for WTP models)
#' @param panelID Optional. The name of the panel ID column (for panel data)
#'
#' @return An object of class 'logitr_validation' containing validation results
#' @export
#' @examples
#' library(logitr)
#'
#' # Validate the yogurt dataset
#' validate_data(yogurt, outcome = "choice", obsID = "obsID")
#'
#' # Validate with parameters specified
#' validate_data(yogurt, outcome = "choice", obsID = "obsID",
#'               pars = c("price", "feat", "brand"))

validate_data <- function(data, outcome, obsID, pars = NULL, scalePar = NULL,
                         panelID = NULL) {

  # Initialize results list
  results <- list(
    valid = TRUE,
    errors = character(0),
    warnings = character(0),
    summary = list(),
    diagnostics = list(),
    data_info = list(
      nrows = if(is.data.frame(data)) nrow(data) else 0,
      ncols = if(is.data.frame(data)) ncol(data) else 0,
      outcome = outcome,
      obsID = obsID,
      pars = pars,
      scalePar = scalePar,
      panelID = panelID
    )
  )

  # Helper function to add errors
  add_error <- function(msg) {
    results$errors <<- c(results$errors, msg)
    results$valid <<- FALSE
  }

  # Helper function to add warnings
  add_warning <- function(msg) {
    results$warnings <<- c(results$warnings, msg)
  }

  # 1. Basic data structure checks
  if (!is.data.frame(data)) {
    add_error("Data must be a data.frame")
    class(results) <- "logitr_validation"
    return(results)
  }

  if (nrow(data) == 0) {
    add_error("Data frame is empty")
    class(results) <- "logitr_validation"
    return(results)
  }

  # 2. Check required columns exist
  required_cols <- c(outcome, obsID)
  if (!is.null(pars)) required_cols <- c(required_cols, pars)
  if (!is.null(scalePar)) required_cols <- c(required_cols, scalePar)
  if (!is.null(panelID)) required_cols <- c(required_cols, panelID)

  missing_cols <- setdiff(required_cols, names(data))
  if (length(missing_cols) > 0) {
    add_error(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
    class(results) <- "logitr_validation"
    return(results)
  }

  # 3. Validate outcome variable
  outcome_col <- data[[outcome]]

  # Check if outcome is numeric/logical
  if (!is.numeric(outcome_col) && !is.logical(outcome_col)) {
    add_error(paste("Outcome variable '", outcome, "' must be numeric (0/1) or logical"))
  } else {
    # Check for valid values (only 0 and 1, or TRUE/FALSE)
    unique_outcomes <- unique(outcome_col[!is.na(outcome_col)])

    if (is.logical(outcome_col)) {
      if (!all(unique_outcomes %in% c(TRUE, FALSE))) {
        add_error("Outcome variable contains values other than TRUE/FALSE")
      }
    } else {
      if (!all(unique_outcomes %in% c(0, 1))) {
        invalid_vals <- unique_outcomes[!unique_outcomes %in% c(0, 1)]
        add_error(paste("Outcome variable contains invalid values:",
                       paste(invalid_vals, collapse = ", "),
                       "- only 0 and 1 are allowed"))
      }
    }

    # Check for missing values
    if (any(is.na(outcome_col))) {
      na_rows <- which(is.na(outcome_col))
      add_error(paste("Outcome variable has missing values in rows:",
                     paste(utils::head(na_rows, 10), collapse = ", "),
                     if(length(na_rows) > 10) "..." else ""))
    }
  }

  # 4. Validate obsID structure
  obsID_col <- data[[obsID]]

  # Check for missing obsID values
  if (any(is.na(obsID_col))) {
    na_rows <- which(is.na(obsID_col))
    add_error(paste("ObsID has missing values in rows:",
                   paste(utils::head(na_rows, 10), collapse = ", "),
                   if(length(na_rows) > 10) "..." else ""))
  }

  # Check obsID structure and choice consistency
  if (!any(is.na(obsID_col)) && !any(is.na(outcome_col)) &&
      (is.numeric(outcome_col) || is.logical(outcome_col))) {
    obsID_summary <- stats::aggregate(outcome_col, by = list(obsID = obsID_col),
                              FUN = function(x) c(sum = sum(x, na.rm = TRUE),
                                                 count = length(x)))

    # Check for exactly one choice per observation
    choice_counts <- obsID_summary$x[,"sum"]
    multiple_choices <- which(choice_counts > 1)
    no_choices <- which(choice_counts == 0)

    if (length(multiple_choices) > 0) {
      bad_obsIDs <- obsID_summary$obsID[multiple_choices]
      add_error(paste("Multiple choices (>1) found in obsID(s):",
                     paste(utils::head(bad_obsIDs, 10), collapse = ", "),
                     if(length(bad_obsIDs) > 10) "..." else ""))

      # Store problematic rows for detailed reporting
      results$diagnostics$multiple_choice_details <- list()
      for (i in utils::head(multiple_choices, 5)) {
        prob_obsID <- obsID_summary$obsID[i]
        prob_rows <- which(data[[obsID]] == prob_obsID)
        results$diagnostics$multiple_choice_details[[as.character(prob_obsID)]] <- prob_rows
      }
    }

    if (length(no_choices) > 0) {
      bad_obsIDs <- obsID_summary$obsID[no_choices]
      add_error(paste("No choices (0) found in obsID(s):",
                     paste(utils::head(bad_obsIDs, 10), collapse = ", "),
                     if(length(bad_obsIDs) > 10) "..." else ""))

      # Store problematic rows for detailed reporting
      results$diagnostics$no_choice_details <- list()
      for (i in utils::head(no_choices, 5)) {
        prob_obsID <- obsID_summary$obsID[i]
        prob_rows <- which(data[[obsID]] == prob_obsID)
        results$diagnostics$no_choice_details[[as.character(prob_obsID)]] <- prob_rows
      }
    }

    # Store summary statistics
    results$summary$total_observations <- length(unique(obsID_col))
    results$summary$total_alternatives <- nrow(data)
    results$summary$alternatives_per_obs <- table(obsID_summary$x[,"count"])
    results$summary$valid_choices <- sum(choice_counts == 1)
  }

  # 5. Check for sequential obsID issues (non-sequential is OK, but repeated within group is not)
  # Check for repeated obsIDs that aren't contiguous
  obsID_rle <- rle(as.character(obsID_col))
  obsID_counts <- table(obsID_col)
  repeated_obsIDs <- names(obsID_counts)[obsID_counts != obsID_rle$lengths[match(names(obsID_counts), obsID_rle$values)]]

  if (length(repeated_obsIDs) > 0) {
    add_error(paste("ObsID values appear in non-contiguous blocks:",
                   paste(utils::head(repeated_obsIDs, 5), collapse = ", "),
                   if(length(repeated_obsIDs) > 5) "..." else ""))

    # Store details for print method
    results$diagnostics$noncontiguous_details <- list()
    for (id in utils::head(repeated_obsIDs, 5)) {
      rows <- which(data[[obsID]] == id)
      results$diagnostics$noncontiguous_details[[as.character(id)]] <- rows
    }
  }

  # 6. Validate parameter columns (if specified)
  if (!is.null(pars)) {
    results$summary$parameter_info <- list()

    for (par in pars) {
      par_col <- data[[par]]
      par_info <- list(name = par, type = class(par_col)[1])

      # Check for missing values
      if (any(is.na(par_col))) {
        na_count <- sum(is.na(par_col))
        na_rows <- which(is.na(par_col))
        add_warning(paste("Parameter '", par, "' has", na_count, "missing values in rows:",
                         paste(utils::head(na_rows, 10), collapse = ", "),
                         if(length(na_rows) > 10) "..." else ""))
        par_info$na_count <- na_count
        par_info$na_rows <- utils::head(na_rows, 10)
      } else {
        par_info$na_count <- 0
      }

      # Store data type info
      if (is.character(par_col) || is.factor(par_col)) {
        unique_vals <- unique(par_col[!is.na(par_col)])
        par_info$unique_values <- utils::head(unique_vals, 10)
        par_info$n_levels <- length(unique_vals)
      } else if (is.numeric(par_col)) {
        par_info$range <- c(min(par_col, na.rm = TRUE), max(par_col, na.rm = TRUE))
      }

      results$summary$parameter_info[[par]] <- par_info
    }
  }

  # 7. Validate scalePar (if specified)
  if (!is.null(scalePar)) {
    scale_col <- data[[scalePar]]

    if (!is.numeric(scale_col)) {
      add_error(paste("Scale parameter '", scalePar, "' must be numeric"))
    } else {
      if (any(is.na(scale_col))) {
        na_count <- sum(is.na(scale_col))
        na_rows <- which(is.na(scale_col))
        add_warning(paste("Scale parameter has", na_count, "missing values in rows:",
                         paste(utils::head(na_rows, 10), collapse = ", "),
                         if(length(na_rows) > 10) "..." else ""))
      }
      # Store scale parameter info
      results$summary$scalePar_info <- list(
        name = scalePar,
        type = "numeric",
        range = c(min(scale_col, na.rm = TRUE), max(scale_col, na.rm = TRUE)),
        na_count = sum(is.na(scale_col))
      )
    }
  }

  # 8. Validate panelID structure (if specified)
  if (!is.null(panelID)) {
    panel_col <- data[[panelID]]

    if (any(is.na(panel_col))) {
      na_rows <- which(is.na(panel_col))
      add_warning(paste0("Panel ID has ", length(na_rows), " missing values in rows: ",
                        paste(utils::head(na_rows, 10), collapse = ", "),
                        if(length(na_rows) > 10) "..." else ""))
    }

    # Check panel structure consistency
    if (!any(is.na(obsID_col))) {
      panel_obs_summary <- stats::aggregate(obsID_col, by = list(panel = panel_col),
                                    FUN = function(x) length(unique(x)))
      results$summary$individuals <- length(unique(panel_col))
      results$summary$obs_per_individual <- summary(panel_obs_summary$x)
    }
  }

  # Set class and return
  class(results) <- "logitr_validation"
  return(results)
}
