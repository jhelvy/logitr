# ============================================================================
# Functions for encoding the data to prep for estimation
# ============================================================================

#' Returns a list of the design matrix `X` and updated `parNames` and
#' `randPars` to include any dummy-coded categorical or interaction
#' variables.
#'
#' Recodes a list of the design matrix (`X`) and two vectors (`parNames` and
#' `randPars`) with discrete (categorical) variables and interaction variables
#' added to `X`, `parNames`, and `randPars`.
#' @param data The choice data, formatted as a `data.frame` object.
#' @param parNames The names of the parameters to be estimated in the model.
#' Must be the same as the column names in the `data` argument. For WTP space
#' models, do not include price in `parNames`.
#' @param randPars A named vector whose names are the random parameters and
#' values the distribution: `'n'` for normal or `'ln'` for log-normal.
#' Defaults to `NULL`.
#' @return A list of the design matrix (`X`) and two vectors (`parNames` and
#' `randPars`) with discrete (categorical) variables and interaction variables
#' added.
#' @export
#' @examples
#' data(yogurt)
#'
#' result <- recodeData(
#'     data = yogurt,
#'     parNames = c("price", "feat", "brand", "price*brand"),
#'     randPars = c(feat = "n", brand = "n")
#' )
#'
#' result$parNames
#' result$randPars
#' head(result$X)
recodeData <- function(data, parNames, randPars) {
  # Get design matrix
  X <- stats::model.matrix(parsToFormula(parNames), data)
  # Separate out interactions
  ints <- grepl("\\*", parNames)
  if (any(ints)) {
    intNames <- parNames[ints == TRUE]
    parNames <- parNames[ints == FALSE]
  }
  # Dummy code categorical variables (if any exist)
  parTypes <- getParTypes(data, parNames)
  if (!is.null(parTypes$discrete)) {
    dummyLevels <- getDummyLevels(data, parTypes)
    parNames <- c(parTypes$continuous, unlist(dummyLevels))
    names(parNames) <- NULL
    randPars <- updateRandPars(randPars, dummyLevels)
  }
  # Create interactions (if any exist)
  if (any(ints)) {
    parNames <- addIntPars(parNames, intNames, dummyLevels)
  }
  return(list(X = X[,parNames], parNames = parNames, randPars = randPars))
}

getParTypes <- function(df, parNames) {
  types <- unlist(lapply(df[parNames], class))
  discIDs <- which(types %in% c("character", "factor"))
  continuousNames <- parNames[setdiff(seq_len(length(parNames)), discIDs)]
  if (length(continuousNames) == 0) {
    continuousNames <- NULL
  }
  discreteNames <- parNames[discIDs]
  if (length(discreteNames) == 0) {
    discreteNames <- NULL
  }
  return(list(continuous = continuousNames, discrete = discreteNames))
}

getDummyLevels <- function(data, parTypes) {
  discreteNames <- parTypes$discrete
  levels <- lapply(data[discreteNames], unique)
  parNames <- list()
  for (i in seq_len(length(discreteNames))) {
    name <- discreteNames[i]
    dummyNames <- paste0(name, levels[[name]])[-1]
    parNames[[i]] <- dummyNames
  }
  names(parNames) <- discreteNames
  return(parNames)
}

updateRandPars <- function(randPars, dummyLevels) {
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
  return(randPars)
}

addIntPars <- function(parNames, intNames, dummyLevels) {
  intList <- strsplit(intNames, "\\*")
  allIntNames <- list()
  for (i in seq_len(length(intList))) {
    intPars1 <- intList[[i]][1]
    intPars2 <- intList[[i]][2]
    # Get dummy coded variable names for categorical vars
    if (intPars1 %in% names(dummyLevels)) {
      intPars1 <- dummyLevels[[intPars1]]
    }
    if (intPars2 %in% names(dummyLevels)) {
      intPars2 <- dummyLevels[[intPars2]]
    }
    allIntNames[[i]] <- getIntNames(intPars1, intPars2)
  }
  return(c(parNames, unlist(allIntNames)))
}

getIntNames <- function(intPars1, intPars2) {
  df <- expand.grid(intPars1, intPars2)
  return(paste(df$Var1, df$Var2, sep = ":"))
}

#' Adds dummy-coded variables to data frame.
#'
#' This function adds dummy-coded variables to a data frame based on a
#' vector of column names.
#' @param df A data frame.
#' @param vars The variables in the data frame for which you want to
#' create new dummy coded variables.
#' @return A a dataframe with new dummy-coded variables added.
#' @export
#' @examples
#' # Create an example data frame:
#' df <- data.frame(
#'     animal   = c("dog", "goldfish", "bird", "dog", "goldfish"),
#'     numLegs  = c(4, 0, 2, 4, 0),
#'     lifeSpan = c(10, 10, 5, 10, 10))
#'
#' # Create dummy coded variables for the variables "animal" and "numLegs":
#' df_dummy <- dummyCode(df, vars = c("animal", "numLegs"))
#' df_dummy
dummyCode <- function(df, vars) {
  dummies <- stats::model.matrix(parsToFormula(vars), df)
  return(cbind(df, dummies))
}

parsToFormula <- function(pars) {
  return(stats::as.formula(paste0("~ ", paste(pars, collapse = " + "), "-1")))
}
