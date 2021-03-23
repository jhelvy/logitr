# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs <- function(data, choiceName, obsIDName, parNames, randPars,
                           priceName, randPrice, modelSpace, weightsName,
                           options) {
  # Setup pars
  data <- as.data.frame(data)
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
  modelInputs <- addDraws(modelInputs)
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
  if (modelSpace == "pref") {
    return(NA)
  }
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
  return(as.matrix(price))
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

addDraws <- function(modelInputs) {
  options <- modelInputs$options
  if (isMxlModel(modelInputs$parSetup)) {
    modelInputs$modelType <- "mxl"
  }
  userDraws <- options$standardDraws
  standardDraws <- getStandardDraws(modelInputs$parSetup, options$numDraws)
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
