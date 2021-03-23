# ============================================================================
# Functions for encoding the data to prep for estimation
# ============================================================================

#' Recode a data frame to create dummy-coded categorical and interaction
#' variables.
#'
#' Recodes a list of a dataframe (`data`) and two vectors (`parNames` and
#' `randPars`) with discrete (categorical) variables and interaction variables
#' added to the data frame as well as the `parNames` and `randPars`. This
#' function is used internally inside the main `logitr()` function but is also
#' exported for use in other libraries.
#' @param data The choice data, formatted as a `data.frame` object.
#' @param parNames The names of the parameters to be estimated in the model.
#' Must be the same as the column names in the `data` argument. For WTP space
#' models, do not include price in `parNames`.
#' @param randPars A named vector whose names are the random parameters and
#' values the distribution: `'n'` for normal or `'ln'` for log-normal.
#' Defaults to `NULL`.
#' @return A list of a dataframe (`data`) and two vectors (`parNames` and
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
#' head(result$data)
recodeData <- function(data, parNames, randPars) {
  # Separate out interactions
  ints <- grepl("\\*", parNames)
  if (any(ints)) {
    intNames <- parNames[ints == TRUE]
    parNames <- parNames[ints == FALSE]
  }
  # Dummy code categorical variables
  parTypes <- getParTypes(data, parNames)
  if (!is.null(parTypes$dist)) {
    dummyList <- addDummyPars(data, parNames, randPars, parTypes)
    data <- dummyList$data
    parNames <- dummyList$parNames
    randPars <- dummyList$randPars
  }
  # Create interactions (if any exist)
  if (any(ints)) {
    intsList <- addIntPars(data, parNames, intNames, parTypes)
    data <- intsList$data
    parNames <- intsList$parNames
  }
  return(list(data = data, parNames = parNames, randPars = randPars))
}

getParTypes <- function(df, parNames) {
    types <- unlist(lapply(df[parNames], class))
    discIDs <- which(types %in% c("character", "factor"))
    if (length(discIDs) == 0) {
      return(list(cont = parNames, dist = NULL))
    }
    if (length(discIDs) == length(parNames)) {
      return(list(cont = NULL, dist = parNames))
    }
    return(list(
      cont = parNames[setdiff(seq(length(parNames)), discIDs)],
      dist = parNames[discIDs])
    )
}

addDummyPars <- function(data, parNames, randPars, parTypes) {
  data <- dummyCode(data, parTypes$dist)
  dummyPars <- c()
  for (discPar in parTypes$dist) {
    newNames <- getCatVarDummyNames(data, discPar)
    # Update randPars with new dummy coded pars
    if (discPar %in% names(randPars)) {
      matchID <- which(names(randPars) == discPar)
      dist <- randPars[matchID]
      newRandPars <- rep(dist, length(newNames))
      names(newRandPars) <- newNames
      randPars <- randPars[-matchID]
      randPars <- c(randPars, newRandPars)
    }
    dummyPars <- c(dummyPars, newNames)
  }
  parNames <- c(parTypes$cont, dummyPars)
  return(list(data = data, parNames = parNames, randPars = randPars))
}

#' Creates dummy-coded variables.
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
    df <- as.data.frame(df)
    nonVars <- colnames(df)[which(! colnames(df) %in% vars)]
    # Keep the original variables and the order to restore later after merging
    df$order <- seq(nrow(df))
    for (i in seq_len(length(vars))) {
        var <- vars[i]
        colIndex <- which(colnames(df) == var)
        levels <- sort(unique(df[,colIndex]))
        mergeMat <- as.data.frame(diag(length(levels)))
        mergeMat <- cbind(levels, mergeMat)
        colnames(mergeMat) <- c(var, paste(var, levels, sep='_'))
        df <- merge(df, mergeMat)
    }
    # Restore the original column order
    new <- colnames(df)[which(! colnames(df) %in% c(vars, nonVars))]
    df <- df[c(nonVars, vars, new)]
    # Restore the original row order
    df <- df[order(df$order),]
    row.names(df) <- df$order
    df$order <- NULL
    return(df)
}

addIntPars <- function(data, parNames, intNames, parTypes) {
  intList <- strsplit(intNames, "\\*")
  allIntPars <- list()
  for (i in seq_len(length(intList))) {
    intPars1 <- intList[[i]][1]
    intPars2 <- intList[[i]][2]
    # Get dummy coded variable names for categorical vars
    if (intPars1 %in% parTypes$dist) {
      intPars1 <- getCatVarDummyNames(data, intPars1)
    }
    if (intPars2 %in% parTypes$dist) {
      intPars2 <- getCatVarDummyNames(data, intPars2)
    }
    intPars <- list()
    index <- 1
    for (intPar1 in intPars1) {
      for (intPar2 in intPars2) {
        int <- data[intPar1] * data[intPar2]
        names(int) <- paste0(intPar1, "*", intPar2)
        intPars[[index]] <- int
        index <- index + 1
      }
    }
    allIntPars[[i]] <- do.call(cbind, intPars)
  }
  allIntPars <- do.call(cbind, allIntPars)
  data <- cbind(data, allIntPars)
  parNames <- c(parNames, names(allIntPars))
  return(list(data = data, parNames = parNames))
}

getCatVarDummyNames <- function(data, discPar) {
  levels <- as.vector(as.matrix(unique(data[discPar])))
  varNames <- sort(paste(discPar, levels, sep = "_"))
  return(varNames[-1])
}
