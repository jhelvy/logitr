# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs <- function(
    data, outcome, obsID, pars , randPars, price, randPrice, modelSpace,
    weights, panelID, clusterID, robust, startParBounds, startVals,
    numMultiStarts, useAnalyticGrad, scaleInputs, standardDraws, numDraws,
    numCores, vcov, predict, call, options
) {

  # Keep original input arguments
  inputs <- list(
    outcome         = outcome,
    obsID           = obsID,
    pars            = pars,
    randPars        = randPars,
    price           = price,
    randPrice       = randPrice,
    modelSpace      = modelSpace,
    weights         = weights,
    panelID         = panelID,
    clusterID       = clusterID,
    robust          = robust,
    startParBounds  = startParBounds,
    startVals       = startVals,
    numMultiStarts  = numMultiStarts,
    useAnalyticGrad = useAnalyticGrad,
    scaleInputs     = scaleInputs,
    numDraws        = numDraws,
    numCores        = numCores,
    vcov            = vcov,
    predict         = predict
  )

  # Check for valid inputs and options
  runInputChecks(data, inputs)
  options <- checkOptions(options)

  # Set number of cores for parallel processing
  numCores <- setNumCores(numCores)

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
  outcome <- as.matrix(data[outcome])
  modelType <- "mnl"
  if (isMxlModel(parSetup)) { modelType <- "mxl" }

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

  # Setup panelID
  panel <- !is.null(inputs$panelID)
  if (panel) {
    panelID <- as.matrix(data[panelID])
    reps <- as.numeric(table(panelID))
    panelID <- rep(seq_along(reps), reps) # Make sure it's a sequential number
  }

  # Setup clusters
  numClusters <- 0
  inputs <- setupClusters(inputs, panel, robust, weightsUsed)
  if (!is.null(inputs$clusterID)) {
    if (robust == FALSE) {
      message("Setting robust to TRUE since clusters are being used")
      inputs$robust <- TRUE
    }
    clusterID <- as.matrix(data[inputs$clusterID])
    numClusters <- length(unique(clusterID))
  }

  # Make data object
  data <- list(
    price     = price,
    X         = X,
    outcome   = outcome,
    obsID     = obsID,
    panelID   = panelID,
    clusterID = clusterID,
    weights   = weights
  )

  # Scale data
  if (scaleInputs) {
    scaleFactors <- getScaleFactors(data, modelSpace, modelType, parIDs)
    data_scaled <- scaleData(data, scaleFactors, modelSpace)
    if (modelSpace == "wtp") {
      n <- length(scaleFactors)
      scaleFactors[2:n] <- scaleFactors[2:n] / scaleFactors[1]
    }
  } else {
    scaleFactors <- rep(1, length(parList$all))
    data_scaled <- data
  }

  # Make differenced data
  data_diff <- makeDiffData(data_scaled, modelType)

  # Make modelInputs list
  modelInputs <- list(
    call          = call,
    inputs        = inputs,
    modelType     = modelType,
    freq          = getFrequencyCounts(obsID, outcome),
    price         = price,
    data          = data,
    data_diff     = data_diff,
    scaleFactors  = scaleFactors,
    weightsUsed   = weightsUsed,
    numClusters   = numClusters,
    parSetup      = parSetup,
    parIDs        = parIDs,
    parList       = parList,
    numBetas      = length(parSetup),
    standardDraws = standardDraws,
    nrowX         = nrow(data_diff$X),
    panel         = panel,
    numCores      = numCores,
    options       = options
  )

  # Add mixed logit inputs
  if (modelType == "mxl") {
    modelInputs$standardDraws <- makeMxlDraws(modelInputs)
    modelInputs$partials <- makePartials(modelInputs)
  }

  # Set logit and eval functions
  modelInputs$logitFuncs <- setLogitFunctions(modelSpace)
  modelInputs$evalFuncs <- setEvalFunctions(modelType, useAnalyticGrad)

  return(modelInputs)
}

setNumCores <- function(numCores) {
  coresAvailable <- parallel::detectCores()
  maxCores <- coresAvailable - 1
  # CRAN checks limits you to 2 cores, see this SO issue:
  # https://stackoverflow.com/questions/50571325/r-cran-check-fail-when-using-parallel-functions
  chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
  if (nzchar(chk) && (chk != "false")) {
    # use 2 cores in CRAN/Travis/AppVeyor
    return(2L)
  }
  if (is.null(numCores)) {
    return(maxCores)
  } else if (!is.numeric(numCores)) {
    warning(
      "Non-numeric value provided for numCores...setting numCores to ",
      maxCores
    )
    return(maxCores)
  } else if (numCores > coresAvailable) {
    warning(
      "Cannot use ", numCores, " cores because your machine only has ",
      coresAvailable, " available...setting numCores to ", maxCores
    )
    return(maxCores)
  }
  return(numCores)
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

setupClusters <- function(inputs, panel, robust, weightsUsed) {
  if (panel & robust) {
    if (!identical(inputs$clusterID, inputs$panelID)) {
      message(
        "Setting clusterID to '", inputs$panelID, "' since robust == TRUE ",
        "and a panelID is provided")
      inputs$clusterID <- inputs$panelID
    }
  }

  if (robust & is.null(inputs$clusterID)) {
    message(
      "Setting clusterID to '", inputs$obsID, "' since robust == TRUE ")
    inputs$clusterID <- inputs$obsID
  }

  if (weightsUsed & is.null(inputs$clusterID)) {
    if (panel) {
      message(
        "Setting clusterID to '", inputs$panelID, "' since weights are being ",
        "used and no clusterID was provided")
      inputs$clusterID <- inputs$panelID
    } else {
      message(
        "Setting clusterID to '", inputs$obsID, "' since weights are being ",
        "used and no clusterID was provided")
      inputs$clusterID <- inputs$obsID
    }
  }
  return(inputs)
}

getScaleFactors <- function(data, modelSpace, modelType, parIDs) {
  price <- data$price
  X <- data$X
  minX <- apply(X, 2, min)
  maxX <- apply(X, 2, max)
  scaleFactors <- abs(maxX - minX)
  scaleFactorNames <- names(scaleFactors)
  # Scale price if WTP space model
  if (modelSpace == "wtp") {
    vals <- unique(price)
    scaleFactorPrice <- abs(max(vals) - min(vals))
    scaleFactors <- c(scaleFactorPrice, scaleFactors)
    names(scaleFactors) <- c("lambda", scaleFactorNames)
  }
  # If MXL model, need to replicate scale factors for sigma pars
  if (modelType == "mxl") {
    scaleFactors <- c(scaleFactors, scaleFactors[parIDs$random])
  }
  return(scaleFactors)
}

# Function that scales all the variables in X to be between 0 and 1:
scaleData <- function(data, scaleFactors, modelSpace) {
  scaledPrice <- data$price
  scaledX <- data$X
  if (modelSpace == "wtp") {
    scaledPrice <- scaledPrice / scaleFactors[1] # Scale price
    scaleFactors <- scaleFactors[2:length(scaleFactors)]
  }
  for (col in seq_len(ncol(scaledX))) {
    scaledX[, col] <- scaledX[, col] / scaleFactors[col]
  }
  return(list(
    price     = scaledPrice,
    X         = scaledX,
    outcome   = data$outcome,
    obsID     = data$obsID,
    panelID   = data$panelID,
    clusterID = data$clusterID,
    weights   = data$weights
  ))
}

makeDiffData <- function(data, modelType) {
  # Subtracting out the chosen alternative makes things faster
  X_chosen <- data$X[data$outcome == 1,]
  X_chosen <- checkMatrix(X_chosen)
  if (!is.matrix(X_chosen)) { X_chosen <- as.matrix(X_chosen) }
  X_diff <- (data$X - X_chosen[data$obsID,])[data$outcome != 1,]
  X_diff <- checkMatrix(X_diff)
  price_diff <- NULL
  if (!is.null(data$price)) {
    price_chosen <- data$price[data$outcome == 1]
    price_diff <- (data$price - price_chosen[data$obsID])[data$outcome != 1]
  }
  panelID <- data$panelID
  weights <- data$weights[data$outcome == 1]
  if (!is.null(panelID) & (modelType == "mxl")) {
    panelID <- data$panelID[data$outcome == 1]
    weights <- unique(data.frame(panelID = panelID, weights = weights))$weights
  }
  return(list(
    price     = price_diff,
    X         = X_diff,
    obsID     = data$obsID[data$outcome != 1],
    panelID   = panelID,
    clusterID = data$clusterID[data$outcome != 1],
    weights   = weights
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

getFrequencyCounts <- function(obsID, outcome) {
  obsIDCounts <- table(obsID)
  alt <- sequence(obsIDCounts)
  freq <- table(alt, outcome)
  freq <- freq[, which(colnames(freq) == "1")]
  return(freq)
}
