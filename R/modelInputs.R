# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs <- function(data, choiceName, obsIDName, parNames, randPars,
                           priceName, randPrice, modelSpace, weightsName,
                           options) {
  # Setup pars
  runInputChecks(choiceName, obsIDName, parNames, randPars, priceName,
                 randPrice, modelSpace, weightsName)
  # Recode discrete (categorical) variables and interactions
  recoded <- recodeData(data, parNames, randPars)
  data <- recoded$data
  parNames <- recoded$parNames
  randPars <- recoded$randPars
  # Set up the parameters
  parSetup <- getParSetup(parNames, priceName, randPars, randPrice)
  parNameList <- getParNameList(parSetup)
  options <- runOptionsChecks(options, parNameList)
  # Separate data elements
  data <- removeNAs(
    data, choiceName, obsIDName, parNames, priceName,
    modelSpace, weightsName
  )
  X <- as.matrix(data[parNames])
  obsID <- as.matrix(data[, which(names(data) == obsIDName)])
  choice <- as.matrix(data[, which(names(data) == choiceName)])
  # Define price for WTP space models (price must be numeric type)
  price <- definePrice(data, priceName, modelSpace)
  # Setup weights
  weights <- matrix(1, nrow(data))
  weightsUsed <- FALSE
  if (!is.null(weightsName)) {
    weights <- as.matrix(data[weightsName])
    weightsUsed <- TRUE
  }
  # Create the modelInputs list
  modelInputs <- list(
    price = price, X = X, choice = choice, obsID = obsID,
    weights = weights, priceName = priceName, parNameList = parNameList,
    parSetup = parSetup, scaleFactors = NA, modelSpace = modelSpace,
    modelType = "mnl", weightsUsed = weightsUsed, options = options
  )
  if (options$scaleInputs) {
    modelInputs <- scaleInputs(modelInputs)
  }
  modelInputs <- addDraws(modelInputs, parSetup)
  modelInputs$logitFuncs <- setLogitFunctions(modelSpace)
  modelInputs$evalFuncs <- setEvalFunctions(
    modelInputs$modelType, options$useAnalyticGrad
  )
  return(modelInputs)
}

runInputChecks <- function(choiceName, obsIDName, parNames, randPars,
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
  allVars <- names(data)
  matches <- which(grepl(paste0(discPar, "_"), allVars))
  return(allVars[matches][-1])
}

getParSetup <- function(parNames, priceName, randPars, randPrice) {
  parSetup <- rep("f", length(parNames))
  for (i in seq_len(length(parNames))) {
    name <- parNames[i]
    if (name %in% names(randPars)) {
      parSetup[i] <- randPars[name]
    }
  }
  names(parSetup) <- parNames
  if (is.null(priceName) == F) {
    if (is.null(randPrice)) {
      randPrice <- "f"
    }
    parSetup <- c(randPrice, parSetup)
    names(parSetup)[1] <- "lambda"
  }
  return(parSetup)
}

getParNameList <- function(parSetup) {
  # For mxl models, need both '_mu' and '_sigma' parameters
  randParIDs <- getRandParIDs(parSetup)
  names <- names(parSetup)
  names_mu <- names
  names_sigma <- names[randParIDs]
  if (length(randParIDs) > 0) {
    names_mu[randParIDs] <- paste(names[randParIDs], "mu", sep = "_")
    names_sigma <- paste(names_sigma, "sigma", sep = "_")
  }
  names_all <- c(names_mu, names_sigma)
  return(list(mu = names_mu, sigma = names_sigma, all = names_all))
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

removeNAs <- function(data, choiceName, obsIDName, parNames, priceName,
                      modelSpace, weightsName) {
  colsToSelect <- c(choiceName, obsIDName, parNames, weightsName)
  if (modelSpace == "wtp") {
    colsToSelect <- c(colsToSelect, priceName, weightsName)
  }
  return(stats::na.omit(data[colsToSelect]))
}

definePrice <- function(data, priceName, modelSpace) {
  price <- NA
  if (modelSpace == "wtp") {
    price <- data[, which(names(data) == priceName)]
    if (! typeof(price) %in% c("integer", "double")) {
      stop(
        'Please make sure the price column in your data defined by the ',
        '"priceName" argument is encoded as a numeric data type. Price must ',
        'be numeric for WTP space models.'
      )
    }
  }
  return(-1*as.matrix(price))
}

# Function that scales all the variables in X to be between 0 and 1:
scaleInputs <- function(modelInputs) {
  price <- modelInputs$price
  X <- modelInputs$X
  scaledX <- X
  scaledPrice <- price
  # Scale X data
  scaleFactorsX <- rep(0, ncol(scaledX))
  for (col in seq_len(ncol(scaledX))) {
    var <- X[, col]
    vals <- unique(var)
    scalingFactor <- abs(max(vals) - min(vals))
    scaledX[, col] <- var / scalingFactor
    scaleFactorsX[col] <- scalingFactor
  }
  scaleFactors <- scaleFactorsX
  names(scaleFactors) <- colnames(scaledX)
  # Scale price if WTP space model
  if (modelInputs$modelSpace == "wtp") {
    vals <- unique(price)
    scaleFactorPrice <- abs(max(vals) - min(vals))
    scaledPrice <- price / scaleFactorPrice
    scaleFactors <- c(scaleFactorPrice, scaleFactorsX)
    names(scaleFactors) <- c("lambda", colnames(scaledX))
  }
  modelInputs$X <- scaledX
  modelInputs$price <- scaledPrice
  modelInputs$scaleFactors <- scaleFactors
  return(modelInputs)
}

addDraws <- function(modelInputs, parSetup) {
  options <- modelInputs$options
  if (isMxlModel(parSetup)) {
    modelInputs$modelType <- "mxl"
  }
  userDraws <- options$standardDraws
  standardDraws <- getStandardDraws(parSetup, options$numDraws)
  if (is.null(userDraws)) {
    modelInputs$standardDraws <- standardDraws
    return(modelInputs)
  }
  # If the user provides their own draws, make sure there are enough
  # columns
  if (ncol(userDraws) != ncol(standardDraws)) {
    stop("The user-provided draws do not match the dimensions of the number of parameters")
  }
  modelInputs$standardDraws <- userDraws
  return(modelInputs)
}

setLogitFunctions <- function(modelSpace) {
  logitFuncs <- list(
    getMnlLogit  = getMnlLogit,
    mnlNegLL     = mnlNegLL,
    getMnlV      = getMnlV_pref,
    mnlNegGradLL = mnlNegGradLL_pref,
    mnlHessLL    = mnlHessLL_pref,
    getMxlLogit  = getMxlLogit,
    mxlNegLL     = mxlNegLL,
    getMxlV      = getMxlV_pref,
    mxlNegGradLL = mxlNegGradLL_pref
  )
  if (modelSpace == "wtp") {
    logitFuncs$getMnlV <- getMnlV_wtp
    logitFuncs$mnlNegGradLL <- mnlNegGradLL_wtp
    logitFuncs$mnlHessLL <- mnlHessLL_wtp
    logitFuncs$getMxlV <- getMxlV_wtp
    logitFuncs$mxlNegGradLL <- mxlNegGradLL_wtp
  }
  return(logitFuncs)
}

setEvalFunctions <- function(modelType, useAnalyticGrad) {
  evalFuncs <- list(
    objective = mnlNegLLAndNumericGradLL,
    negLL     = getMnlNegLL,
    negGradLL = getNumericNegGradLL,
    hessLL    = getNumericHessLL
  )
  if (useAnalyticGrad) {
    evalFuncs$objective <- mnlNegLLAndGradLL
    evalFuncs$negGradLL <- getMnlNegGradLL
    # evalFuncs$hessLL    = getMnlHessLL # Numeric approx is faster
  }
  if (modelType == "mxl") {
    evalFuncs$objective <- mxlNegLLAndNumericGradLL
    evalFuncs$negLL <- getMxlNegLL
    evalFuncs$negGradLL <- getNumericNegGradLL
    evalFuncs$hessLL <- getNumericHessLL
    if (useAnalyticGrad) {
      evalFuncs$objective <- mxlNegLLAndGradLL
      evalFuncs$negGradLL <- getMxlNegGradLL
    }
  }
  return(evalFuncs)
}
