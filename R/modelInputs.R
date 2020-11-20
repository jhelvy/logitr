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
  # Dummy code categorical variables
  catVars <- getCatVars(data, parNames)
  if (!is.null(catVars)) {
    data <- dummyCode(data, catVars)
    parNames <- getDummyCodedParNames(data, parNames)
  }
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
  price <- NA
  if (modelSpace == "wtp") {
    price <- -1 * as.matrix(data[, which(names(data) == priceName)])
  }
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

runInputChecks <- function(choiceName, obsIDName, parNames, randPars, priceName,
                 randPrice, modelSpace, weightsName) {
  if (! is.null(priceName)) {
    if (priceName %in% parNames) {
      stop('The value you provided for the "priceName" argument is also included in your "parNames" argument. If you are estimating a WTP space model, you should remove the price column name from your "parNames" argument and provid it separately with the "priceName" argument.')
    }
  }
}

getCatVars <- function(df, parNames) {
    tempDf <- df[parNames]
    types <- sapply(tempDf, typeof)
    categoricalIDs <- which(types == "character")
    if (length(categoricalIDs) == 0) {
      return(NULL)
    }
    return(parNames[categoricalIDs])
}

#' Creates dummy-coded variables.
#'
#' Use this function to create dummy-coded variables in a data frame.
#' @param df A data frame.
#' @param vars The variables in the data frame for which you want to
#' create new dummy coded variables.
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
dummyCode = function(df, vars) {
    df = as.data.frame(df)
    nonVars = colnames(df)[which(! colnames(df) %in% vars)]
    # Keep the original variables and the order to restore later after merging
    df$order = seq(nrow(df))
    for (i in 1:length(vars)) {
        var      = vars[i]
        colIndex = which(colnames(df) == var)
        levels   = sort(unique(df[,colIndex]))
        mergeMat = as.data.frame(diag(length(levels)))
        mergeMat = cbind(levels, mergeMat)
        colnames(mergeMat) = c(var, paste(var, levels, sep='_'))
        df = merge(df, mergeMat)
    }
    # Restore the original column order
    new = colnames(df)[which(! colnames(df) %in% c(vars, nonVars))]
    df = df[c(nonVars, vars, new)]
    # Restore the original row order
    df = df[order(df$order),]
    row.names(df) = df$order
    df$order <- NULL
    return(df)
}

getDummyCodedParNames <- function(df, parNames) {
  # Create a new set of parNames with the dummy-coded names
  catVars <- getCatVars(data, parNames)
  nonCatVars <- setdiff(parNames, catVars)
  allVars <- names(df)
  keepDummyVars <- c()
  for (i in 1:length(catVars)) {
    tempMatches <- which(grepl(paste0(catVars[i], "_"), allVars))
    tempDummyVars <- allVars[tempMatches]
    keepDummyVars <- c(keepDummyVars, tempDummyVars[2:length(tempDummyVars)])
  }
  codedParNames <- c(nonCatVars, keepDummyVars)
  return(codedParNames)
}

getParSetup <- function(parNames, priceName, randPars, randPrice) {
  parSetup <- rep("f", length(parNames))
  for (i in 1:length(parNames)) {
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
  # Run checks for all inputs
  if (is.null(options$numMultiStarts)) {
    options$numMultiStarts <- 1
  }
  if (options$numMultiStarts < 1) {
    options$numMultiStarts <- 1
  }
  if (is.null(options$keepAllRuns)) {
    options$keepAllRuns <- F
  }
  if (is.null(options$useAnalyticGrad)) {
    options$useAnalyticGrad <- T
  }
  if (is.null(options$scaleInputs)) {
    options$scaleInputs <- T
  }
  if (is.null(options$startParBounds)) {
    options$startParBounds <- c(-1, 1)
  }
  if (is.null(options$standardDraws)) {
    options$standardDraws <- NULL
  }
  if (is.null(options$numDraws)) {
    options$numDraws <- 200
  }
  if (is.null(options$drawType)) {
    options$drawType <- "halton"
  }
  if (is.null(options$printLevel)) {
    options$printLevel <- 0
  }
  if (is.null(options$xtol_rel)) {
    options$xtol_rel <- 1.0e-8
  }
  if (is.null(options$xtol_abs)) {
    options$xtol_abs <- 1.0e-8
  }
  if (is.null(options$ftol_rel)) {
    options$ftol_rel <- 1.0e-8
  }
  if (is.null(options$ftol_abs)) {
    options$ftol_abs <- 1.0e-8
  }
  if (is.null(options$maxeval)) {
    options$maxeval <- 1000
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

# Function that scales all the variables in X to be between 0 and 1:
scaleInputs <- function(modelInputs) {
  price <- modelInputs$price
  X <- modelInputs$X
  scaledX <- X
  scaledPrice <- price
  # Scale X data
  scaleFactorsX <- rep(0, ncol(scaledX))
  for (col in 1:ncol(scaledX)) {
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
  standardDraws <- getStandardDraws(
      parSetup, options$numDraws, options$drawType
  )
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
    getMnlLogit = getMnlLogit,
    mnlNegLL = mnlNegLL,
    getMnlV = getMnlV_pref,
    mnlNegGradLL = mnlNegGradLL_pref,
    mnlHessLL = mnlHessLL_pref,
    getMxlLogit = getMxlLogit,
    mxlNegLL = mxlNegLL,
    getMxlV = getMxlV_pref,
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
    negLL = getMnlNegLL,
    negGradLL = getNumericNegGradLL,
    hessLL = getNumericHessLL
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
