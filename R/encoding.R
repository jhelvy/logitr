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

#' Returns a list of the design matrix `X` and updated `pars` and
#' `randPars` to include any dummy-coded categorical or interaction
#' variables.
#'
#' Recodes the data and returns a list of the encoded design matrix (`X`) as
#' well as two vectors (`pars` and `randPars`) with discrete (categorical)
#' variables and interaction variables added to `X`, `pars`, and
#' `randPars`.
#' @param data The choice data, formatted as a `data.frame` object.
#' @param pars The names of the parameters to be estimated in the model.
#' Must be the same as the column names in the `data` argument. For WTP space
#' models, do not include price in `pars`.
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
#' result$pars
#' result$randPars
#' head(result$X)
recodeData <- function(data, pars, randPars) {
  data <- as.data.frame(data) # tibbles break things
  data <- orderedFactorsToChars(data) # ordered factors cause weird names
  X <- getDesignMatrix(data, pars)
  return(list(
    X = X,
    pars = colnames(X),
    randPars = recodeRandPars(data, pars, randPars)))
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

getDesignMatrix <- function(data, pars) {
  formula <- parsToFormula(pars)
  X <- stats::model.matrix(formula, data)
  X <- X[,-1,drop=FALSE] # Drop intercept
  return(X)
}

parsToFormula <- function(vars) {
  return(stats::as.formula(paste0("~ ", paste(vars, collapse = " + "))))
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
    var <- data[,name]
    parLevels <- levels(as.factor(var))
    dummyNames <- paste0(name, parLevels)[-1]
    pars[[i]] <- dummyNames
  }
  names(pars) <- discrete
  return(pars)
}
