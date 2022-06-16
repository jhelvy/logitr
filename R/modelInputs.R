# ============================================================================
# Functions for setting up the modelInputs list
# (object that stores and passes the data and settings between functions)
# ============================================================================

# Creates a list of the data and other information needed for running the model
getModelInputs <- function(
    data, outcome, obsID, pars , randPars, scalePar, randScale, modelSpace,
    weights, panelID, clusterID, robust, startParBounds, startVals,
    numMultiStarts, useAnalyticGrad, scaleInputs, standardDraws, drawType,
    numDraws, numCores, vcov, predict, correlation, call, options
) {

  # Keep original input arguments
  inputs <- list(
    outcome         = outcome,
    obsID           = obsID,
    pars            = pars,
    randPars        = randPars,
    scalePar        = scalePar,
    randScale       = randScale,
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
    drawType        = drawType,
    numDraws        = numDraws,
    numCores        = numCores,
    vcov            = vcov,
    predict         = predict,
    correlation     = correlation
  )

  # Check for valid inputs and options
  runInputChecks(data, inputs)
  options <- checkOptions(options)

  # Set the modelSpace based on whether the scalePar is NULL
  if (is.null(scalePar)) {
      modelSpace <- "pref"
  } else {
      modelSpace <- "wtp"
  }
  inputs$modelSpace <- modelSpace

  # Get the design matrix, recoding parameters that are categorical
  # or have interactions
  recoded <- recodeData(data, pars, randPars)
  X <- recoded$X
  pars <- recoded$pars
  randPars <- recoded$randPars
  scalePar <- defineScalePar(data, inputs)
  outcome <- as.matrix(data[outcome])

  # Setup obsID
  obsID <- makeObsID(data, obsID, outcome)

  # Setup panelID
  panel <- !is.null(inputs$panelID)
  if (panel) {
      panelID <- as.matrix(data[panelID])
      reps <- as.numeric(table(panelID))
      panelID <- rep(seq_along(reps), reps) # Make sure it's a sequential number
  }

  # Set up other objects defining aspects of model
  parSetup <- getParSetup(pars, scalePar, randPars, randScale)

  # Define model type
  modelType <- "mnl"
  if (isMxlModel(parSetup)) {
    modelType <- "mxl"
  }

  # Create n object, which stores counts of various variables
  n <- list(
    vars        = length(parSetup),
    parsFixed   = length(which(parSetup == "f")),
    parsRandom  = length(which(parSetup != "f")),
    draws       = numDraws,
    multiStarts = numMultiStarts,
    obs         = sum(outcome),
    cores       = setNumCores(numCores) # cores for parallel processing
  )
  parNames <- getParNames(parSetup, n, correlation)
  n$pars <- length(parNames$all)
  parIDs <- getParIDs(parSetup, n, modelSpace, modelType, randScale,correlation)

  # Add names to startVals (if provided)
  if (!is.null(startVals)) {
    names(startVals) <- parNames$all
    inputs$startVals <- startVals
  }

  # Setup weights
  weights <- matrix(1, nrow(data))
  weightsUsed <- FALSE
  if (!is.null(inputs$weights)) {
    weights <- as.matrix(data[inputs$weights])
    weightsUsed <- TRUE
  }

  # Setup clusters
  n$clusters <- 0
  inputs <- setupClusters(inputs, panel, robust, weightsUsed)
  if (!is.null(inputs$clusterID)) {
    if (robust == FALSE) {
      message("Setting robust to TRUE since clusters are being used")
      inputs$robust <- TRUE
    }
    clusterID <- as.matrix(data[inputs$clusterID])
    n$clusters <- length(unique(clusterID))
  }

  # Make data object
  data <- list(
    scalePar  = scalePar,
    X         = X,
    outcome   = outcome,
    obsID     = obsID,
    panelID   = panelID,
    clusterID = clusterID,
    weights   = weights
  )

  # Scale data
  if (scaleInputs) {
    scaleFactors <- getScaleFactors(
        data, modelSpace, modelType, parIDs, parNames, n, correlation
    )
    data_scaled <- scaleData(data, scaleFactors, modelSpace)
    if (modelSpace == "wtp") {
      n_sf <- length(scaleFactors)
      scaleFactors[2:n_sf] <- scaleFactors[2:n_sf] / scaleFactors[1]
    }
  } else {
    scaleFactors <- rep(1, length(parNames$all))
    data_scaled <- data
  }

  # Make differenced data
  data_diff <- makeDiffData(data_scaled, modelType)
  if (scaleInputs) {
      data_diff_unscaled <- makeDiffData(data, modelType)
  } else {
      data_diff_unscaled <- data_diff
  }
  n$rowX <- nrow(data_diff$X)

  # Make modelInputs list
  modelInputs <- list(
    call          = call,
    date          = format(Sys.time(), "%a %b %d %X %Y"),
    version       = as.character(utils::packageVersion("logitr")),
    inputs        = inputs,
    modelType     = modelType,
    freq          = getFrequencyCounts(obsID, outcome),
    scalePar      = scalePar,
    data          = data,
    data_diff     = data_diff,
    data_diff_unscaled = data_diff_unscaled,
    scaleFactors  = scaleFactors,
    weightsUsed   = weightsUsed,
    parSetup      = parSetup,
    parIDs        = parIDs,
    parNames      = parNames,
    n             = n,
    standardDraws = standardDraws,
    drawType      = drawType,
    panel         = panel,
    options       = options
  )

  # Add mixed logit inputs
  if (modelType == "mxl") {
    modelInputs$standardDraws <- makeMxlDraws(modelInputs)
    modelInputs$partials <- makePartials(modelInputs, data_diff)
    # Need unscaled version of partials for computing hessian
    modelInputs$partials_unscaled <- makePartials(
      modelInputs, data_diff_unscaled)
  }

  # Set logit and eval functions
  modelInputs$logitFuncs <- setLogitFunctions(modelSpace)
  modelInputs$evalFuncs <- setEvalFunctions(modelType, useAnalyticGrad)

  return(modelInputs)
}

makeObsID <- function(data, obsID, outcome) {
  obsID <- as.vector(as.matrix(data[obsID]))
  # Make sure obsID is in sequential order with no repeated IDs
  reps <- table(obsID)
  obsIDCheck <- as.numeric(rep(names(reps), as.numeric(reps)))
  if (!all(obsID == obsIDCheck)) {
    stop(
      "The 'obsID' variable provided has repeated ID values. It must be a ",
      "sequentially increasing numeric value identifying the rows of each ",
      "observation, such as 1,1,1,2,2,2...double check the variable provided ",
      "for 'obsID'."
    )
  }
  # Make sure that each observation ID has only one outcome
  outcomeCheck <- tapply(outcome, obsID, function(x) sum(x))
  if (! all(outcomeCheck == 1)) {
    stop(
      "Each observation outcome (identified by the 'obsID' variable) must ",
      "have only one value equal to 1 and all others equal to 0. Double check ",
      "the variables provided for 'obsID' and 'outcome'."
    )
  }
  return(obsID)
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

defineScalePar <- function(data, inputs) {
  if (inputs$modelSpace == "pref") {
    return(NULL)
  }
  if (inputs$modelSpace == "wtp") {
    scalePar <- data[, which(names(data) == inputs$scalePar)]
    if (! typeof(scalePar) %in% c("integer", "double")) {
      stop(
        'Please make sure the "scalePar" column in your data ',
        'is encoded as a numeric data type. Scale must ',
        'be numeric and continuous.'
      )
    }
  }
  return(as.matrix(scalePar))
}

getParSetup <- function(pars, scalePar, randPars, randScale) {
  parSetup <- rep("f", length(pars))
  for (i in seq_len(length(pars))) {
    name <- pars[i]
    if (name %in% names(randPars)) {
      parSetup[i] <- randPars[name]
    }
  }
  names(parSetup) <- pars
  if (is.null(scalePar) == F) {
    if (is.null(randScale)) {
      randScale <- "f"
    }
    parSetup <- c(randScale, parSetup)
    names(parSetup)[1] <- "scalePar"
  }
  return(parSetup)
}

getParNames <- function(parSetup, n, correlation) {
  # For mxl models, need both mean and sd parameters
  names <- names(parSetup)
  names_mean <- names
  names_sd <- names(parSetup)[which(parSetup != "f")]
  if (n$parsRandom > 0) {
      if (correlation) {
          diags <- as.data.frame(utils::combn(names_sd, 2))
          diags_names <- unlist(lapply(diags, function(x) paste(x[1], x[2], sep = "_")))
          sd_names <- c()
          for (name in names_sd) {
              sd_name <- paste(name, name, sep = "_")
              diags_name <- diags_names[which(diags[1,] == name)]
              sd_names <- c(sd_names, sd_name, diags_name)
          }
          names_sd <- sd_names
      }
    names_sd <- paste("sd", names_sd, sep = "_")
  }
  names_all <- c(names_mean, names_sd)
  return(list(mean = names_mean, sd = names_sd, all = names_all))
}

getParIDs <- function(
  parSetup, n, modelSpace, modelType, randScale, correlation
) {
  parIDs <- list(
    f  = which(parSetup == "f"),
    r  = which(parSetup != "f"),
    n  = which(parSetup == "n"),
    ln = which(parSetup == "ln"),
    cn = which(parSetup == "cn")
  )
  if (modelType == "mxl") {
    allIDs  <- seq(n$pars)
    diagIDs <- seq(n$parsRandom)
    if (correlation) {
      incs  <- seq(n$parsRandom, 1, -1)
      diagIDs <- cumsum(c(1, incs))[1:n$parsRandom]
      parIDs$sdOffDiag <- allIDs[-c(seq(n$vars), n$vars + diagIDs)]
    }
    parIDs$sdDiag <- allIDs[n$vars + diagIDs]
    names(parIDs$sdDiag) <- names(parIDs$r)
    # For log-normal parameters, set the IDs for updating partials in
    # gradient calculations (used in updatePartials() function in logit.R)
    if (length(parIDs$ln) > 0) {
      parIDs$partial_lnIDs <- getPartialIDs(parIDs, parIDs$ln, n, correlation)
    }
    # For censored-normal parameters, set the IDs for updating partials in
    # gradient calculations (used in updatePartials() function in logit.R)
    if (length(parIDs$cn) > 0) {
      parIDs$partial_cnIDs <- getPartialIDs(parIDs, parIDs$cn, n, correlation)
    }
  }
  # Make lambda & omega IDs for WTP space models
  if (modelSpace == "wtp") {
    lambdaIDs <- 1
    omegaIDs <- seq(n$pars)
    if (!is.null(randScale)) {
      lambdaIDs <- c(lambdaIDs, length(parSetup) + 1)
      if (correlation) {
        lowerMat <- matrix(0, n$parsRandom, n$parsRandom)
        lowerMat[lower.tri(lowerMat, diag = TRUE)] <- (n$vars + 1):n$pars
        lambdaOffDiag <- lowerMat[,1]
        parIDs$lambdaOffDiag <- lambdaOffDiag[2:length(lambdaOffDiag)]
      }
    }
    omegaIDs <- omegaIDs[-lambdaIDs]
    parIDs$lambdaIDs <- lambdaIDs
    parIDs$omegaIDs <- omegaIDs
  }
  return(parIDs)
}

getPartialIDs <- function(parIDs, IDs, n, correlation) {
  partial_IDs <- list()
  for (i in 1:length(IDs)) {
    parID <- IDs[i]
    sdID  <- parIDs$sdDiag[names(parID)]
    if (correlation) {
      lowerMat <- matrix(0, n$parsRandom, n$parsRandom)
      lowerMat[lower.tri(lowerMat, diag = TRUE)] <- (n$vars + 1):n$pars
      index <- which(names(parIDs$r) == names(parID))
      sdID <- lowerMat[, index]
      sdID <- sdID[which(sdID != 0)]
    }
    partial_IDs[[i]] <- c(parID, sdID)
  }
  return(partial_IDs)
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

getScaleFactors <- function(
  data, modelSpace, modelType, parIDs, parNames, n, correlation
) {
  scalePar <- data$scalePar
  X <- data$X
  minX <- apply(X, 2, min)
  maxX <- apply(X, 2, max)
  scaleFactors <- abs(maxX - minX)
  scaleFactorNames <- names(scaleFactors)
  # Scale scalePar if WTP space model
  if (modelSpace == "wtp") {
    vals <- unique(scalePar)
    scaleFactorScalePar <- abs(max(vals) - min(vals))
    scaleFactors <- c(scaleFactorScalePar, scaleFactors)
    names(scaleFactors) <- c("scalePar", scaleFactorNames)
  }
  # If MXL model, need to replicate scale factors for sd pars
  if (modelType == "mxl") {
      sfRand <- scaleFactors[parIDs$r]
      if (correlation) {
        sf <- rep(1, length(parNames$all))
        sf[seq(n$vars)] <- scaleFactors
        sf[parIDs$sdDiag] <- sfRand
        diags <- as.data.frame(utils::combn(names(sfRand), 2))
        sfOffDiags <- c()
        for (i in 1:ncol(diags)) {
          sfOffDiags <- c(sfOffDiags, prod(scaleFactors[diags[,i]]))
        }
        sf[parIDs$sdOffDiag] <- sfOffDiags
        names(sf) <- parNames$all
        scaleFactors <- sf
      } else {
        scaleFactors <- c(scaleFactors, sfRand)
      }
  }
  return(scaleFactors)
}

# Function that scales all the variables in X to be between 0 and 1:
scaleData <- function(data, scaleFactors, modelSpace) {
  scaledScalePar <- data$scalePar
  scaledX <- data$X
  if (modelSpace == "wtp") {
    scaledScalePar <- scaledScalePar / scaleFactors[1] # Scale scalePar
    scaleFactors <- scaleFactors[2:length(scaleFactors)]
  }
  for (col in seq_len(ncol(scaledX))) {
    scaledX[, col] <- scaledX[, col] / scaleFactors[col]
  }
  return(list(
    scalePar  = scaledScalePar,
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
  scalePar_diff <- NULL
  if (!is.null(data$scalePar)) {
    scalePar_chosen <- data$scalePar[data$outcome == 1]
    scalePar_diff <- (data$scalePar - scalePar_chosen[data$obsID])[data$outcome != 1]
  }
  panelID <- data$panelID
  weights <- data$weights[data$outcome == 1]
  if (!is.null(panelID) & (modelType == "mxl")) {
    panelID <- data$panelID[data$outcome == 1]
    weights <- unique(data.frame(panelID = panelID, weights = weights))$weights
  }
  return(list(
    scalePar  = scalePar_diff,
    X         = X_diff,
    obsID     = data$obsID[data$outcome != 1],
    panelID   = panelID,
    clusterID = data$clusterID[data$outcome != 1],
    weights   = weights
  ))
}

makeMxlDraws <- function(modelInputs) {
  # Message about using Sobol draws with large number of random parameters
  if (length(modelInputs$parIDs$r) > 5) {
    if (modelInputs$drawType == 'halton') {
      message(
        "Since your model has 5 or more random parameters, it is ",
        "recommended that you use Sobol instead of Halton draws. ",
        "You can implement this by setting drawType = 'sobol'. \n\n",
        "It is also recommended that you use at least 200 draws, which ",
        "can be implemented by setting numDraws = 200")
    }
  }
  draws <- modelInputs$standardDraws
  if (is.null(draws)) {
    draws <- getStandardDraws(
        modelInputs$parIDs, modelInputs$n$draws, modelInputs$drawType)
  } else if (ncol(draws) != modelInputs$n$vars) {
    # If user provides draws, make sure there are enough columns
    stop(
      "The number of columns in the user-provided draws do not match the ",
      "required number. Please use draws with ", modelInputs$n$vars, " columns"
    )
  }
  return(draws)
}

makePartials <- function(mi, data) {
  n <- mi$n
  X <- data$X
  if (mi$inputs$modelSpace == "wtp") {
    X <- cbind(1, X)
  }
  X2 <- repmat(X, 1, 2)
  partials <- list()
  draws <- cbind(
    matrix(1, ncol = n$vars, nrow = n$draws), mi$standardDraws)
  for (i in seq_len(2*n$vars)) {
    X_temp <- X2[,rep(i, n$draws)]
    draws_temp <- repmat(matrix(draws[, i], nrow = 1), nrow(X_temp), 1)
    partials[[i]] <- X_temp*draws_temp
  }
  sdIDs <- n$vars + mi$parIDs$r
  if (! mi$inputs$correlation) {
    return(partials[c(1:n$vars, sdIDs)])
  }
  draws <- mi$standardDraws[,mi$parIDs$r]
  X <- X2[,sdIDs]
  IDs <- as.data.frame(utils::combn(seq(length(sdIDs)), 2))
  partialOffDiags <- list()
  for (i in 1:ncol(IDs)) {
    id <- IDs[,i]
    X_temp <- X[,rep(id[1], n$draws)]
    draws_temp <- repmat(matrix(draws[, id[2]], nrow = 1), nrow(X_temp), 1)
    partialOffDiags[[i]] <- X_temp*draws_temp
  }
  result <- lapply(seq(n$pars), function(x) x)
  result[1:n$vars] <- partials[1:n$vars]
  result[mi$parIDs$sdDiag] <- partials[sdIDs]
  result[mi$parIDs$sdOffDiag] <- partialOffDiags
  return(result)
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
