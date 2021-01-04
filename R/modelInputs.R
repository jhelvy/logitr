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
  # Recode categorical variables and interactions
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

runInputChecks <- function(choiceName, obsIDName, parNames, randPars,
  priceName, randPrice, modelSpace, weightsName) {
  if (! is.null(priceName)) {
    if (priceName %in% parNames) {
      stop('The value you provided for the "priceName" argument is also included in your "parNames" argument. If you are estimating a WTP space model, you should remove the price column name from your "parNames" argument and provide it separately with the "priceName" argument.')
    }
  }
  if ((modelSpace == 'wtp') & is.null(priceName)) {
    stop('You are estimating a WTP space model but have not provided a "priceName" argument. Please provide the name of the column in your data frame that represents "price" for the "priceName" argument.')
  }
}

recodeData <- function(data, parNames, randPars) {
  # Separate out interactions
  ints <- grepl("\\*", parNames)
  if (any(ints)) {
    intNames <- parNames[ints == TRUE]
    parNames <- parNames[ints == FALSE]
  }
  # Dummy code categorical variables
  parTypes <- getParTypes(data, parNames)
  if ("d" %in% parTypes) {
    dummyList <- addDummyVars(data, parNames, randPars, parTypes)
    data <- dummyList$data
    parNames <- dummyList$parNames
    randPars <- dummyList$randPars
  }
  # Create interactions (if any exist)
  if (any(ints)) {
    intsList <- addIntVars(data, parNames, intNames, catVars)
    data <- intsList$data
    parNames <- intsList$parNames
  }
  return(list(data = data, parNames = parNames, randPars = randPars))
}

getParTypes <- function(df, parNames) {
    types <- unlist(lapply(df[parNames], class))
    categoricalIDs <- which(types %in% c("character", "factor"))
    parTypes <- rep("c", length(parNames))
    names(parTypes) <- parNames
    if (length(categoricalIDs) == 0) {
      return(parTypes)
    }
    parTypes[categoricalIDs] <- "d"
    return(parTypes)
}

addDummyVars <- function(data, parNames, randPars, parTypes) {
  discPars <- parNames[which(parTypes  == "d")]
  contPars <- parNames[which(parTypes  == "c")]
  data <- dummyCode(data, discPars)
  allVars <- names(data)
  # Create a new set of parNames with the dummy-coded names
  dummyPars <- c()
  for (discPar in discPars) {
    matchIDs <- which(grepl(paste0(discPar, "_"), allVars))
    newNames <- allVars[matchIDs][-1]
    # Update randPars
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
  parNames <- c(contPars, dummyPars)
  return(list(data = data, parNames = parNames, randPars = randPars))
}

addIntVars <- function(data, parNames, intNames, catVars) {
  intList <- strsplit(intNames, "\\*")
  allIntVars <- list()
  for (i in seq_len(length(intList))) {
    intVars1 <- intList[[i]][1]
    intVars2 <- intList[[i]][2]
    # Get dummy coded variable names for categorical vars
    if (intVars1 %in% catVars) {
      intVars1 <- getCatVarDummyNames(data, intVars1)
    }
    if (intVars2 %in% catVars) {
      intVars2 <- getCatVarDummyNames(data, intVars2)
    }
    intVars <- list()
    index <- 1
    for (intVar1 in intVars1) {
      for (intVar2 in intVars2) {
        int <- data[intVar1] * data[intVar2]
        names(int) <- paste0(intVar1, "*", intVar2)
        intVars[[index]] <- int
        index <- index + 1
      }
    }
    allIntVars[[i]] <- do.call(cbind, intVars)
  }
  allIntVars <- do.call(cbind, allIntVars)
  data <- cbind(data, allIntVars)
  parNames <- c(parNames, names(allIntVars))
  return(list(data = data, parNames = parNames))
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
  if (is.null(options$message)) {
    options$message <- TRUE
  }
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
