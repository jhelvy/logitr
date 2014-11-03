# Convert the user-provided information on parameters into a data frame.
d$covariateSetup = getCovariateSetupDF(d)

# Identify which covariates are fixed and which are random
d$randomCovariateIDs = which(d$covariateSetup[,2] != 0)
d$fixedCovariateIDs  = which(d$covariateSetup[,2] == 0)
d$fixedIDs           = which(d$covariateSetup[,2] == 0)
d$normIDs            = which(d$covariateSetup[,2] == 1)
d$logNormIDs         = which(d$covariateSetup[,2] == 2)
d$numRandom          = length(d$randomCovariateIDs)
d$numFixed           = length(d$fixedCovariateIDs)

# Setup choice variables
d$choice        = as.matrix(d$choiceData[d$choice])
d$observationID = as.matrix(d$choiceData[d$observationID])
d$numObs        = length(unique(d$observationID))

# Setup attribute variables (P and X)
d$betaNames = d$covariateSetup[,1]
d$P         = as.matrix(d$choiceData[d$priceVar])
d$X         = as.matrix(d$choiceData[d$betaNames])
if (d$modelSpace == 'wtp') {
    d$P = -1*d$P
    d$X = d$X[,which(colnames(d$X) != d$priceVar)]
}

# Setup weights
d$weights = matrix(1, nrow(d$X))
if (d$useWeights) {
    d$weights = as.matrix(d$choiceData[d$weights])
}

# Setup names of variables
d$allParNames = d$betaNames
if (d$modelType  == 'mxl') {
    d$sigmaNames  = paste(d$betaNames[d$randomCovariateIDs], 'sigma', sep='_')
    d$betaNames[d$randomCovariateIDs] =
        paste(d$betaNames[d$randomCovariateIDs], 'mu', sep='_')
    d$allParNames = c(d$betaNames, d$sigmaNames)
}

# Set variables for some basic numbers
d$numBetas  = nrow(d$covariateSetup)
d$numParams = length(d$allParNames)

# Scale P and X for optimization if desired
d$scaleFactors = rep(1, d$numBetas)
if (d$scaleParams) {
    if (d$modelSpace == 'pref') {
        Xout           = scaleX(d$X, 1)
        d$X            = Xout[[1]]
        d$scaleFactors = Xout[[2]]
    } else {
        Pout           = scaleVar(d$P, 1)
        Xout           = scaleX(d$X, 1)
        PscaleFactor   = Pout[[2]]
        XscaleFactors  = Xout[[2]]
        d$P            = Pout[[1]]
        d$X            = Xout[[1]]
        d$scaleFactors = c(PscaleFactor, XscaleFactors)
    }
}

# Replicate scale factors for the sigma terms of the randomly distributed
# betas in the mxl models
if (d$modelType == 'mxl') {
    randomSFs      = d$scaleFactors[d$randomCovariateIDs]
    d$scaleFactors = c(d$scaleFactors, randomSFs)
}

# Load the standard normal draws for the simulation
d$standardDraws = getStandardNormalHaltonDraws(d$numDraws, d$numBetas)
colnames(d$standardDraws) = d$betaNames
d$standardDraws[,d$fixedCovariateIDs] = rep(0, d$numDraws)
