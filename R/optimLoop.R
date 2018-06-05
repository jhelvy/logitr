# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart = function(modelInputs) {
    # Setup lists for storing results
    numMultiStarts = modelInputs$options$numMultiStarts
    models = list()
    for (i in 1:numMultiStarts) {
        if (numMultiStarts==1) {
            cat('Running Model', '\n', sep='')
        } else {
            cat('Running Multistart', i, 'of', numMultiStarts, '\n', sep=' ')
        }
        logLik        = NA
        noFirstRunErr = TRUE
        while (is.na(logLik)) {
            tryCatch({
                startPars = getStartPars(modelInputs, i, noFirstRunErr)
                model     = runModel(modelInputs, startPars)
                logLik    = model$logLik
                model$multistartNumber = i
            }, error=function(e) {
            cat('ERROR: failed to converge...restarting search', '\n',
                sep='')})
            if (i==1 & is.na(logLik) &
                is.null(modelInputs$options$startVals)==F) {
                noFirstRunErr = FALSE
                cat('**User provided starting values did not converge, ',
                    'using random values now**', '\n', sep='')
            }
        }
        models[[i]] = model
    }
    return(models)
}

getStartPars = function(modelInputs, i, noFirstRunErr) {
    startPars = getRandomStartPars(modelInputs)
    if (i==1) {
        if (noFirstRunErr & is.null(modelInputs$options$startVals)==F) {
            cat('**Using User Provided Starting Values For This Run**',
                '\n', sep='')
            startPars = modelInputs$options$startVals
        } else if (noFirstRunErr) {
            startPars = 0*startPars
        }
    }
    if (i==2 & noFirstRunErr & is.null(modelInputs$options$startVals)==F) {
        startPars = 0*startPars
    }
    startPars = checkStartPars(startPars, modelInputs)
    return(startPars)
}

# Returns randomly drawn starting parameters from a uniform distribution
# between modelInputs$options$startParBounds
getRandomStartPars = function(modelInputs) {
    parNameList = modelInputs$parNameList
    bounds      = modelInputs$options$startParBounds
    lower       = bounds[1]
    upper       = bounds[2]
    # For mxl models, need both '.mu' and '.sigma' parameters
    pars.mu          = runif(length(parNameList$mu), lower, upper)
    pars.sigma       = runif(length(parNameList$sigma), lower, upper)
    startPars        = c(pars.mu, pars.sigma)
    names(startPars) = parNameList$all
    return(startPars)
}

# For mxl models in the WTP space, lambda.mu can't be zero
checkStartPars = function(startPars, modelInputs) {
    if (modelInputs$modelSpace == 'wtp' &
        'lambda.mu' %in% modelInputs$parNameList$mu) {
        if (startPars['lambda.mu'] <= 0) {
            startPars['lambda.mu'] = 0.01
            cat('Warning: lambda.mu must be > 0...', '\n', sep='')
            cat('...setting starting point for lambda.mu to 0.01 ','\n',sep='')
        }
    }
    return(startPars)
}

# Runs the MNL model
runModel = function(modelInputs, startPars) {
    startTime = proc.time()
    model = nloptr(
        x0          = startPars,
        eval_f      = modelInputs$evalFuncs$objective,
        modelInputs = modelInputs,
        opts        = list(
            'algorithm' = 'NLOPT_LD_LBFGS',
            'xtol_rel'  = modelInputs$options$xtol_rel,
            'xtol_abs'  = modelInputs$options$xtol_abs,
            'ftol_rel'  = modelInputs$options$ftol_rel,
            'ftol_abs'  = modelInputs$options$ftol_abs,
            print_level = modelInputs$options$printLevel,
            maxeval     = modelInputs$options$maxeval))
    model$startPars = startPars
    model$logLik    = -1*model$objective # -1 for (+) rather than (-) LL
    model$time      = proc.time() - startTime
    return(model)
}

