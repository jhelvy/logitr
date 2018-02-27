# ============================================================================
# Functions for taking draws for mixed logit models
# ============================================================================

# Returns shifted normal draws for each parameter
makeBetaDraws = function(pars, modelInputs){
    muMat    = getMuMat(pars, modelInputs)
    sigmaMat = getSigmaMat(pars, modelInputs)
    # Shift draws by mu and sigma
    betaDraws = muMat + modelInputs$standardDraws*sigmaMat
    # Exponentiate draws for those with logN distribution
    logNormIDs = which(modelInputs$parSetup$dist==2)
    if (length(logNormIDs) > 0) {
        betaDraws[,logNormIDs] = exp(betaDraws[,logNormIDs])
    }
    return(betaDraws)
}

getMuMat = function(pars, modelInputs) {
    pars.mu = pars[1:modelInputs$numBetas]
    numDraws = modelInputs$options$numDraws
    return(matrix(rep(pars.mu, numDraws), ncol=length(pars.mu), byrow=T))
}

getSigmaMat = function(pars, modelInputs) {
    numDraws   = modelInputs$options$numDraws
    pars.sigma = rep(0, modelInputs$numBetas)
    randParIDs = which(modelInputs$parSetup$dist != 0)
    pars.sigma[randParIDs] = pars[(modelInputs$numBetas+1):length(pars)]
    return(matrix(rep(pars.sigma, numDraws), ncol=length(pars.sigma),
           byrow=T))
}

getStandardDraws = function(parSetup, options) {
    numDraws = options$numDraws
    numBetas = nrow(parSetup)
    # Use Halton draws by default
    draws = getHaltonDraws(numDraws, numBetas)
    if (options$drawType=='normal') {
        draws = getNormalDraws(numDraws, numBetas)
    }
    if (options$drawType=='sobol') {
        draws = getSobolDraws(numDraws, numBetas)
    }
    fixedParIDs = which(parSetup$dist==0)
    draws[,fixedParIDs] = rep(0, numDraws)
    return(draws)
}

# Returns a matrix of numDraws x numBetas random standard normal draws
getNormalDraws = function(numDraws, numBetas) {
    N        = numDraws*numBetas
    draws    = rnorm(N, 0, 1)
    badDraws = which(pnorm(draws) < 0.001 | pnorm(draws) > 0.999)
    # Make sure that none of the draws are too extreme in the tail. It
    # causes the likelihood function to crash because the utility value
    # becomes too close to zero, and then taking log(0) crashes.
    while (length(badDraws) > 0){
        draws[badDraws] = rnorm(length(badDraws), 0, 1)
        badDraws = which(pnorm(draws) < 0.001 | pnorm(draws) > 0.999)
    }
    drawsMatrix = matrix(draws, ncol=numBetas)
    return(drawsMatrix)
}

# Returns a matrix of numDraws x numBetas random standard normal draws
# approximated using Halton draws
getHaltonDraws = function(numDraws, numBetas) {
    return(halton(n=numDraws, dim=numBetas, normal=T))
}

# Returns a matrix of numDraws x numBetas random standard normal draws
# approximated using Sobol draws
getSobolDraws = function(numDraws, numBetas) {
    return(sobol(n=numDraws, dim=numBetas, scrambling=3, normal=T))
}
