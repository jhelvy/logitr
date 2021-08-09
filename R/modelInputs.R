# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs <- function(
    data, choice, obsID, pars, randPars, price, randPrice, modelSpace, weights,
    cluster, robust, numMultiStarts, useAnalyticGrad, scaleInputs,
    startParBounds, standardDraws, numDraws, startVals, call, options
) {

  data <- as.data.frame(data) # tibbles break things
  checkArguments(call) # Argument names were changed in v0.2.3

  # Keep original input arguments
  inputs <- list(
    choice          = choice,
    obsID           = obsID,
    pars            = pars,
    randPars        = randPars,
    price           = price,
    randPrice       = randPrice,
    modelSpace      = modelSpace,
    weights         = weights,
    cluster         = cluster,
    robust          = robust,
    numMultiStarts  = numMultiStarts,
    useAnalyticGrad = useAnalyticGrad,
    scaleInputs     = scaleInputs,
    startParBounds  = startParBounds,
    numDraws        = numDraws,
    startVals       = startVals
  )

  # Check for valid inputs and options
  runInputChecks(data, inputs)
  options <- checkOptions(options)

  # Get the design matrix, recoding parameters that are categorical
  # or have interactions
  recoded <- recodeData(data, pars, randPars)
  X <- recoded$X
  pars <- recoded$pars
  randPars <- recoded$randPars
  price <- definePrice(data, inputs)

  # Set up other data
  parSetup <- getParSetup(pars, price, randPars, randPrice)
  parIDs <- getParIDs(parSetup, modelSpace, randPrice)
  parList <- getParList(parSetup, parIDs$random)
  obsID <- as.matrix(data[obsID])
  reps <- as.numeric(table(obsID))
  obsID <- rep(seq_along(reps), reps) # Make sure obsID is sequential number
  choice <- as.matrix(data[choice])

  # Add names to startVals (if provided)
  if (!is.null(startVals)) {
    names(startVals) <- parList$all
    inputs$startVals <- startVals
  }

  # Setup weights
  weights <- matrix(1, nrow(data))
  weightsUsed <- FALSE
  if (!is.null(inputs$weights)) {
    weights <- as.matrix(data[inputs$weights])
    weightsUsed <- TRUE
  }

  # Setup Clusters
  clusterID <- NULL
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
    clusterID <- as.matrix(data[cluster])
    numClusters <- getNumClusters(clusterID)
  }

  # Update inputs for cluster and robust controls
  inputs$cluster <- cluster
  inputs$robust <- robust

  # Make data object
  data <- list(
    price     = price,
    X         = X,
    choice    = choice,
    obsID     = obsID,
    clusterID = clusterID,
    weights   = weights,
    scaleFactors = rep(1, length(parList$all))
  )

  # Scale data
  if (scaleInputs) {
    data <- scaleData(data, modelSpace, parSetup, parIDs)
  }

  # Make differenced data
  data_diff <- makeDiffData(data)

  # Make modelInputs list
  modelInputs <- list(
    call          = call,
    inputs        = inputs,
    modelType     = "mnl",
    freq          = getFrequencyCounts(obsID, choice),
    price         = price,
    data          = data,
    data_diff     = data_diff,
    weightsUsed   = weightsUsed,
    numClusters   = numClusters,
    parSetup      = parSetup,
    parIDs        = parIDs,
    parList       = parList,
    numBetas      = length(parSetup),
    standardDraws = standardDraws,
    nrowX         = nrow(data_diff$X),
    options       = options
  )

  # Add mixed logit inputs
  if (isMxlModel(parSetup)) {
    modelInputs$modelType <- "mxl"
    modelInputs$standardDraws <- makeMxlDraws(modelInputs)
    modelInputs$partials <- makePartials(modelInputs)
  }

  # Set logit and eval functions
  modelInputs$logitFuncs <- setLogitFunctions(modelSpace)
  modelInputs$evalFuncs <- setEvalFunctions(
    modelInputs$modelType, useAnalyticGrad
  )
  return(modelInputs)
}

definePrice <- function(data, inputs) {
  if (inputs$modelSpace == "pref") {
    return(NULL)
  }
  if (inputs$modelSpace == "wtp") {
    price <- data[, which(names(data) == inputs$price)]
    if (! typeof(price) %in% c("integer", "double")) {
      stop(
        'Please make sure the price column in your data defined by the ',
        '"price" argument is encoded as a numeric data type. Price must ',
        'be numeric for WTP space models.'
      )
    }
  }
  return(as.matrix(price))
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

getParIDs <- function(parSetup, modelSpace, randPrice) {
  parIDs <- list(
    fixed     = which(parSetup == "f"),
    random    = which(parSetup != "f"),
    normal    = which(parSetup == "n"),
    logNormal = which(parSetup == "ln")
  )
  # Make lambda & omega IDs for WTP space models
  if (modelSpace == "wtp") {
    lambdaIDs <- 1
    numPars <- length(parIDs$fixed) + 2*length(parIDs$random)
    omegaIDs <- seq(numPars)
    if (!is.null(randPrice)) {
      lambdaIDs <- c(lambdaIDs, length(parSetup) + 1)
    }
    omegaIDs <- omegaIDs[-lambdaIDs]
    parIDs$lambdaIDs <- lambdaIDs
    parIDs$omegaIDs <- omegaIDs
  }
  return(parIDs)
}

getParList <- function(parSetup, randParIDs) {
  # For mxl models, need both '_mu' and '_sigma' parameters
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

getNumClusters <- function(clusterID) {
  if (is.null(clusterID)) { return(0) }
  return(length(unique(clusterID)))
}

# Function that scales all the variables in X to be between 0 and 1:
scaleData <- function(data, modelSpace, parSetup, parIDs) {
  price <- data$price
  X <- data$X
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
  if (modelSpace == "wtp") {
    vals <- unique(price)
    scaleFactorPrice <- abs(max(vals) - min(vals))
    scaledPrice <- price / scaleFactorPrice # Scale price
    scaleFactorsX <- scaleFactorsX / scaleFactorPrice # Update scaleFactorsX
    scaleFactors <- c(scaleFactorPrice, scaleFactorsX)
    names(scaleFactors) <- c("lambda", colnames(scaledX))
  }
  # If MXL model, need to replicate scale factors for sigma pars
  if (isMxlModel(parSetup)) {
    scaleFactors <- c(scaleFactors, scaleFactors[parIDs$random])
  }
  data$X <- scaledX
  data$price <- scaledPrice
  data$scaleFactors <- scaleFactors
  return(data)
}

makeDiffData <- function(data) {
  # Subtracting out the chosen alternative makes things faster
  X_chosen <- data$X[data$choice == 1,]
  X_diff <- (data$X - X_chosen[data$obsID,])[data$choice != 1,]
  price_diff <- NULL
  if (!is.null(data$price)) {
    price_chosen <- data$price[data$choice == 1]
    price_diff <- (data$price - price_chosen[data$obsID])[data$choice != 1]
  }
  return(list(
    price     = price_diff,
    X         = X_diff,
    obsID     = data$obsID[data$choice != 1],
    clusterID = data$clusterID[data$choice != 1],
    weights   = data$weights[data$choice == 1]
  ))
}

makeMxlDraws <- function(modelInputs) {
  draws <- modelInputs$standardDraws
  if (is.null(draws)) {
    draws <- getStandardDraws(
      modelInputs$parIDs, modelInputs$inputs$numDraws)
  } else if (ncol(draws) != modelInputs$numBetas) {
    # If user provides draws, make sure there are enough columns
    stop(
      "The number of columns in the user-provided draws do not match the ",
      "specified number of parameters")
  }
  return(draws)
}

makePartials <- function(modelInputs) {
  numBetas <- modelInputs$numBetas
  numDraws <- modelInputs$inputs$numDraws
  X <- modelInputs$data_diff$X
  if (modelInputs$inputs$modelSpace == "wtp") {
    X <- cbind(1, X)
  }
  X2 <- repmat(X, 1, 2)
  partials <- list()
  draws <- cbind(
    matrix(1, ncol = numBetas, nrow = numDraws), modelInputs$standardDraws)
  for (i in seq_len(2*numBetas)) {
    X_temp <- X2[,rep(i, numDraws)]
    draws_temp <- repmat(matrix(draws[, i], nrow = 1), nrow(X_temp), 1)
    partials[[i]] <- X_temp*draws_temp
  }
  return(partials[c(1:numBetas, numBetas + modelInputs$parIDs$random)])
}

setLogitFunctions <- function(modelSpace) {
  logitFuncs <- list(
    getMnlV      = getMnlV_pref,
    getMxlV      = getMxlV_pref,
    mnlNegGradLL = mnlNegGradLL_pref,
    mxlNegGradLL = mxlNegGradLL_pref,
    mnlHessLL    = mnlHessLL_pref,
    mxlHessLL    = mxlHessLL_pref
  )
  if (modelSpace == "wtp") {
    logitFuncs$getMnlV <- getMnlV_wtp
    logitFuncs$getMxlV <- getMxlV_wtp
    logitFuncs$mnlNegGradLL <- mnlNegGradLL_wtp
    logitFuncs$mxlNegGradLL <- mxlNegGradLL_wtp
    logitFuncs$mnlHessLL <- mnlHessLL_wtp
    logitFuncs$mxlHessLL <- mxlHessLL_wtp
  }
  return(logitFuncs)
}

setEvalFunctions <- function(modelType, useAnalyticGrad) {
  evalFuncs <- list(
    objective = mnlNegLLAndGradLL,
    negLL     = getMnlNegLL,
    negGradLL = getMnlNegGradLL,
    hessLL    = getMnlHessLL
  )
  if (!useAnalyticGrad) {
    evalFuncs$objective <- negLLAndNumericGradLL
    evalFuncs$negGradLL <- getNumericNegGradLL
    evalFuncs$hessLL    <- getNumericHessLL
  }
  if (modelType == "mxl") {
    evalFuncs$objective <- mxlNegLLAndGradLL
    evalFuncs$negLL     <- getMxlNegLL
    evalFuncs$negGradLL <- getMxlNegGradLL
    evalFuncs$hessLL    <- getMxlHessLL
    if (!useAnalyticGrad) {
      evalFuncs$objective <- negLLAndNumericGradLL
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
