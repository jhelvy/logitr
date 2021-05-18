# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs <- function(data, choiceName, obsIDName, parNames, randPars,
                           priceName, randPrice, modelSpace, weightsName, clusterName, robust,
                           options) {
  data <- as.data.frame(data) # tibbles break things
  # Setup pars
  runInputChecks(
    data, choiceName, obsIDName, parNames, randPars, priceName,
    randPrice, modelSpace, weightsName, clusterName)
  # Get the design matrix, recoding parameters that are categorical
  # or have interactions
  parNames_orig <- parNames
  randPars_orig <- randPars
  recoded <- recodeData(data, parNames, randPars)
  X <- recoded$X
  parNames <- recoded$parNames
  randPars <- recoded$randPars
  # Set up the parameters
  parSetup <- getParSetup(parNames, priceName, randPars, randPrice)
  parNameList <- getParNameList(parSetup)
  options <- runOptionsChecks(options, parNameList)
  obsID <- as.matrix(data[obsIDName])
  choice <- as.matrix(data[choiceName])
  # Define price for WTP space models (price must be numeric type)
  price <- definePrice(data, priceName, modelSpace)
  # Setup weights
  weights <- matrix(1, nrow(data))
  weightsUsed <- FALSE
  if (!is.null(weightsName)) {
    weights <- as.matrix(data[weightsName])
    weightsUsed <- TRUE
  }

  # Setup Clusters
  clusterIDs<-NULL
  numClusters <- 0
  if(weightsUsed & is.null(clusterName)){
    clusterName <- obsIDName
    robust<-TRUE
  }
  if(!is.null(clusterName)){
    clusterName <- clusterName
    robust<-TRUE
    clusterIDs <- as.matrix(data[clusterName])
    numClusters <- getNumClusters(clusterIDs)
  }


  # Create the modelInputs list
  modelInputs <- list(
    price = price, X = X, choice = choice, obsID = obsID,
    weights = weights, priceName = priceName, parNames = parNames_orig,
    randPars = randPars_orig, parNameList = parNameList, parSetup = parSetup,
    scaleFactors = NA, modelSpace = modelSpace, modelType = "mnl",
    weightsUsed = weightsUsed, clusterName = clusterName, clusterIDs = clusterIDs, numClusters = numClusters, robust = robust, options = options
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

getNumClusters <- function(clusterID){
  if(is.null(clusterID)){
    return(0)
  }
  return(length(unique(clusterID)))
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
