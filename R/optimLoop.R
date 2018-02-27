# ============================================================================
# Functions for running the optimization
# ============================================================================

runMultistart = function(modelInputs) {
    # Setup lists for storing results
    numMultiStarts = modelInputs$options$numMultiStarts
    models = list()
    for (i in 1:numMultiStarts) {
        cat('Running Multistart', i, 'of', numMultiStarts, '\n', sep=' ')
        logLik        = NA
        noFirstRunErr = TRUE
        # Keep trying until a solution is reached for each multistart iteration
        while (is.na(logLik)) {
        tryCatch({
            startPars = getRandomStartPars(modelInputs)
            if (is.null(modelInputs$prefSpaceModel)==F & i==1 &
                noFirstRunErr & modelInputs$modelSpace=='wtp') {
                cat('**Using Preference Space Model WTP Results as Starting ',
                    'Point For This Run**', '\n', sep='')
                startPars = modelInputs$prefSpaceModel$wtp
            }
            model  = runModel(modelInputs, startPars)
            logLik = model$logLik
            model$multistartNumber = i
                   # -1 to get the positive rather than negative LL
        }, error=function(e){
            cat('ERROR: failed to converge...restarting search', '\n',
                sep='')})
            if (i==1 & is.na(logLik)) {noFirstRunErr = FALSE}
        }
        models[[i]] = model
    }
    # printLine()
    return(models)
}

# Returns randomly drawn starting parameters from a uniform distribution
# between -1 and 1
getRandomStartPars = function(modelInputs) {
    parNames = modelInputs$parNames
    # For mxl models, need both '.mu' and '.sigma' parameters
    pars.mu    = runif(length(parNames$mu), -10, 10)
    pars.sigma = runif(length(parNames$sigma), -10, 10)
    # if (modelInputs$options$wtpSpace) {
    #     # This is a WTP space model, so lambda must be positive
    #     pars.mu[1] = runif(1, 0, 1)
    # }
    startPars        = c(pars.mu, pars.sigma)
    names(startPars) = c(parNames$mu, parNames$sigma)
    return(startPars)
}

# Runs the MNL model
runModel = function(modelInputs, startPars) {
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
    model$logLik    = -1*model$objective
    return(model)
}
