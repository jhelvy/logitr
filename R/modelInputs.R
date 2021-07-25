# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs <- function(
  data, choice, obsID, pars, randPars, price, randPrice,
  modelSpace, weights, cluster, robust, call, options
) {
  data <- as.data.frame(data) # tibbles break things

  # Keep original input arguments
  inputs <- list(
    choice     = choice,
    obsID      = obsID,
    pars       = pars,
    randPars   = randPars,
    price      = price,
    randPrice  = randPrice,
    modelSpace = modelSpace,
    weights    = weights,
    robust     = robust,
    cluster    = cluster
  )

  # Check that input are all valid
  runInputChecks(data, inputs)

  # Get the design matrix, recoding parameters that are categorical
  # or have interactions
  recoded <- recodeData(data, pars, randPars)
  X <- recoded$X
  pars <- recoded$pars
  randPars <- recoded$randPars

  # Set up the parameters
  parSetup <- getParSetup(pars, price, randPars, randPrice)
  parList <- getParList(parSetup)
  options <- runOptionsChecks(options, parList)
  obsID <- as.matrix(data[obsID])
  choice <- as.matrix(data[choice])

  # Define price for WTP space models (price must be numeric type)
  price <- definePrice(data, inputs)

  # Setup weights
  weights <- matrix(1, nrow(data))
  weightsUsed <- FALSE
  if (!is.null(inputs$weights)) {
    weights <- as.matrix(data[inputs$weights])
    weightsUsed <- TRUE
  }

  # Setup Clusters
  clusterIDs <- NULL
  numClusters <- 0
  if (robust & is.null(cluster)) {
    cluster <- inputs$obsID
  }
  if (weightsUsed & is.null(cluster)) {
    message(
      "Since weights are being used and no cluster was provided, ",
      "the obsID argument will be used for clustering")
    cluster <- inputs$obsID
  }
  if (!is.null(cluster)) {
    if (robust == FALSE) {
      message("Setting robust to TRUE since clusters are being used")
      robust <- TRUE
    }
    clusterIDs <- as.matrix(data[cluster])
    numClusters <- getNumClusters(clusterIDs)
  }

  modelInputs <- list(
    call         = call,
    inputs       = inputs,
    modelType    = "mnl",
    freq         = getFrequencyCounts(obsID, choice),
    price        = price,
    X            = X,
    choice       = choice,
    obsID        = obsID,
    weights      = weights,
    weightsUsed  = weightsUsed,
    cluster      = cluster,
    clusterIDs   = clusterIDs,
    numClusters  = numClusters,
    robust       = robust,
    parList      = parList,
    parSetup     = parSetup,
    scaleFactors = NA,
    options      = options
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

getParSetup <- function(pars, price, randPars, randPrice) {
  parSetup <- rep("f", length(pars))
  for (i in seq_len(length(pars))) {
    name <- pars[i]
    if (name %in% names(randPars)) {
      parSetup[i] <- randPars[name]
    }
  }
  names(parSetup) <- pars
  if (is.null(price) == F) {
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

getParList <- function(parSetup) {
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

definePrice <- function(data, inputs) {
  if (inputs$modelSpace == "pref") {
    return(NA)
  }
  if (inputs$modelSpace == "wtp") {
    price <- data[, which(names(data) == inputs$price)]
    if (! typeof(inputs$price) %in% c("integer", "double")) {
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
  if (modelInputs$inputs$modelSpace == "wtp") {
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
    getMnlV      = getMnlV_pref,
    mnlNegGradLL = mnlNegGradLL_pref,
    mnlHessLL    = mnlHessLL_pref,
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
    objective = mnlNegLLAndGradLL,
    negLL     = getMnlNegLL,
    negGradLL = getMnlNegGradLL,
    # hessLL    = getMnlHessLL # For now, numeric is faster
    hessLL    = getNumericHessLL
  )
  if (!useAnalyticGrad) {
    evalFuncs$objective <- mnlNegLLAndNumericGradLL
    evalFuncs$negGradLL <- getNumericNegGradLL
    evalFuncs$hessLL    <- getNumericHessLL
  }
  if (modelType == "mxl") {
    # evalFuncs$objective <- mxlNegLLAndGradLL # For now, numeric is faster
    evalFuncs$objective <- mxlNegLLAndNumericGradLL
    evalFuncs$negLL <- getMxlNegLL
    # evalFuncs$negGradLL <- getMxlNegGradLL # For now, numeric is faster
    # evalFuncs$hessLL <- getMxlHessLL
    evalFuncs$negGradLL <- getNumericNegGradLL
    evalFuncs$hessLL <- getNumericHessLL
    if (!useAnalyticGrad) {
      evalFuncs$objective <- mxlNegLLAndNumericGradLL
      evalFuncs$negGradLL <- getNumericNegGradLL
      evalFuncs$hessLL    <- getNumericHessLL
    }
  }
  return(evalFuncs)
}

getFrequencyCounts <- function(obsID, choice) {
  obsIDCounts <- table(obsID)
  alt <- sequence(obsIDCounts)
  freq <- table(alt, choice)
  freq <- freq[, which(colnames(freq) == "1")]
  return(freq)
}
