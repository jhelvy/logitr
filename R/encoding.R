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
#' @export
dummyCode <- function(df, vars) {
  # v0.1.3
  .Deprecated("fastDummies::dummy_cols")
}

#' Returns a list of the design matrix `X` and updated `parNames` and
#' `randPars` to include any dummy-coded categorical or interaction
#' variables.
#'
#' Recodes the data and returns a list of the encoded design matrix (`X`) as
#' well as two vectors (`parNames` and `randPars`) with discrete (categorical)
#' variables and interaction variables added to `X`, `parNames`, and
#' `randPars`.
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
#' library(logitr)
#'
#' data(yogurt)
#'
#' # Recode the yogurt data
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
  data <- as.data.frame(data) # tibbles break things
  data <- orderedFactorsToChars(data) # ordered factors cause weird names
  X <- getDesignMatrix(data, parNames)
  return(list(
    X = X,
    parNames = colnames(X),
    randPars = recodeRandPars(data, parNames, randPars)))
}

# Ordered factors have strange returned column names when encoding with
# model.matrix, so convert them to characters
orderedFactorsToChars <- function(data) {
  types <- getColumnTypes(data)
  names <- names(types[types == "ordered"])
  if (length(names) > 0) {
    data[,names] <- lapply(data[,names], as.character)
  }
  return(data)
}

getColumnTypes <- function(data) {
  types <- lapply(data, class)
  test <- function(x) {x[1]}
  return(unlist(lapply(types, test)))
}

getDesignMatrix <- function(data, parNames) {
  formula <- parsToFormula(parNames)
  X <- stats::model.matrix(formula, data)
  X <- X[,-1,drop=FALSE] # Drop intercept
  return(X)
}

parsToFormula <- function(vars) {
  return(stats::as.formula(paste0("~ ", paste(vars, collapse = " + "))))
}

recodeRandPars <- function(data, parNames, randPars) {
  # Separate out interactions
  ints <- grepl("\\*", parNames)
  if (any(ints)) {
    parNames <- parNames[ints == FALSE]
  }
  # Dummy code categorical variables (if any exist)
  parTypes <- getParTypes(data, parNames)
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

getParTypes <- function(df, parNames) {
  types <- getColumnTypes(df[parNames])
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
  parNames <- list()
  for (i in seq_len(length(discreteNames))) {
    name <- discreteNames[i]
    var <- data[,name]
    parLevels <- levels(as.factor(var))
    dummyNames <- paste0(name, parLevels)[-1]
    parNames[[i]] <- dummyNames
  }
  names(parNames) <- discreteNames
  return(parNames)
}
