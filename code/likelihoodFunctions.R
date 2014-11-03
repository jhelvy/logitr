# Load required libraries
suppressMessages(library(randtoolbox))

# ============================================================================
# Notes on notation used in these functions:
# V          = The observed utility.
# expV       = exp(V), the logit equation numerator.
# sumExpV    = Sum over j for each choice occasion (logit equation
#              denominator).
# repTimes   = The number of times to repeat sumExpV (based on observationID,
#              which dictates the number of alternatives in each choice
#              situation).
# sumExpVRep = sumExpV repeated by repTimes number of times (must be repeated
#              so that the denominator dimension matches that of the numerator
#              in the logit fraction).
# logit      = The logit fraction (expV / sumExpV).
# pHat       = Estimator for probabilities (average over draws in MXL models).
# ============================================================================


# ============================================================================
# Helper functions for computing the logit fraction (common to all models)
# ============================================================================
getMnlLogit = function(V, d) {
    expV       = exp(V)
    sumExpV    = rowsum(expV, group=d$observationID)
    repTimes   = as.numeric(table(d$observationID))
    sumExpVRep = rep(sumExpV, times=repTimes)
    logit      = expV / sumExpVRep
    return(logit)
}

getMxlLogitDraws = function(VDraws, d) {
    expVDraws       = exp(VDraws)
    sumExpVDraws    = rowsum(expVDraws, group=d$observationID)
    repTimes        = rep(as.numeric(table(d$observationID)), each=d$numDraws)
    sumExpVDrawsMat = matrix(rep(sumExpVDraws, times=repTimes),
                             ncol=d$numDraws, byrow=F)
    logitDraws      = expVDraws / sumExpVDrawsMat
    return(logitDraws)
}



# ============================================================================
# Likelihood functions for preference space models
# ============================================================================

# Computes the negative log-likelihood for a MNL model in the preference space.
negLLMnlPref = function(pars, d) {
    V     = d$X%*%pars
    logit = getMnlLogit(V, d)
    negLL = -1*sum(d$weights*d$choice*log(logit))
    return(negLL)
}

# Computes the gradient of the negative log-likelihood for a MNL model in the
# preference space.
negGradLLMnlPref = function(pars, d) {
    V         = d$X%*%pars
    logit     = getMnlLogit(V, d)
    probDiff  = d$weights*(d$choice - logit)
    negGradLL = -1*(t(d$X)%*%probDiff)
    return(negGradLL)
}

# Computes the negative log-likelihood for a MXL model in the preference space.
negLLMxlPref = function(pars, d){
    # Get the draws of beta using the current mu and sigma values (pars)
    betaDraws = makeBetaDraws(pars, d)
    # Now compute the negLL
    VDraws     = d$X%*%t(betaDraws)
    logitDraws = getMxlLogitDraws(VDraws, d)
    pHat       = rowMeans(logitDraws)
    negLL      = -1*sum(d$weights*d$choice*log(pHat))
    return(negLL)
}

# Computes the gradient of the negative log-likelihood for a MXL model in the
# preference space.
negGradLLMxlPref = function(pars, d) {
    # Get the draws of beta using the current mu and sigma values (pars)
    betaDraws = makeBetaDraws(pars, d)
    # Get pHat
    VDraws     = d$X%*%t(betaDraws)
    logitDraws = getMxlLogitDraws(VDraws, d)
    pHat       = rowMeans(logitDraws)
    # Get the sum of the gradient of V for all parameters across all draws
    gradMu    = matrix(0, nrow=nrow(d$X), ncol=d$numBetas)
    gradSigma = matrix(0, nrow=nrow(d$X), ncol=d$numBetas)
    for (i in 1:d$numDraws) {
        beta      = betaDraws[i,]
        draws     = d$standardDraws[i,]
        logit     = logitDraws[,i]
        betaMat   = matrix(rep(beta, nrow(d$X)), ncol=d$numBetas,byrow=T)
        drawsMat  = matrix(rep(draws, nrow(d$X)), ncol=d$numBetas,byrow=T)
        logitMat  = matrix(rep(logit, d$numBetas), ncol=d$numBetas, byrow=F)
        gradMu    = gradMu + getInnerVGrad(d, betaMat, drawsMat, logitMat,'mu')
        gradSigma = gradSigma + getInnerVGrad(d, betaMat, drawsMat, logitMat,
                                             'sigma')
    }
    # Get the average of the gradient of V for all parameters across all draws
    weightsMat = matrix(rep(d$weights, d$numBetas), ncol=ncol(d$X), byrow=F)
    gradMu     = weightsMat*(gradMu / d$numDraws)
    gradSigma  = weightsMat*(gradSigma / d$numDraws)
    # Get the gradient
    pHatInvChosen = matrix(rep(d$choice*(1/pHat), d$numBetas),
                           ncol=d$numBetas, byrow=F)
    gradMu        = colSums(pHatInvChosen*gradMu)
    gradSigma     = colSums(pHatInvChosen*gradSigma)
    # Remove sigma gradient for fixed coefficients
    gradSigma = gradSigma[d$randomCovariateIDs]
    # Get the negGradLL
    negGradLL        = -1*c(gradMu, gradSigma)
    names(negGradLL) = names(pars)
    return(negGradLL)
}

# Computes the inner parenthesis of the gradient for V
getInnerVGrad = function(d, betaMat, drawsMat, logitMat, id) {
    partial = d$X
    if (id == 'sigma') {partial = partial*drawsMat}
    # correct for logNormal covariates
    if (length(d$logNormIDs) > 0) {
        partial[,d$logNormIDs] = partial[,d$logNormIDs]*betaMat[,d$logNormIDs]
    }
    sumLogitMat = rowsum(logitMat*partial, group=d$observationID)
    repTimes    = rep(as.numeric(table(d$observationID)), each=d$numBetas)
    tempMat     = matrix(rep(sumLogitMat, times=repTimes),
                         ncol=ncol(partial), byrow=F)
    innerGrad   = logitMat*(partial - tempMat)
    return(innerGrad)
}



# ============================================================================
# Likelihood functions for willingness-to-pay space models
# ============================================================================

# Computes the negative likelihood in the WTP space for a MNL model
negLLMnlWtp = function(pars, d) {
    alpha = pars[1]
    gamma = pars[2:d$numParams]
    V     = alpha*(d$P + d$X%*%gamma)
    logit = getMnlLogit(V, d)
    negLL = -1*sum(d$weights*d$choice*log(logit))
    return(negLL)
}

# Computes the gradient of the negative likelihood for a MNL model.
negGradLLMnlWtp= function(pars, d) {
    alpha    = pars[1]
    gamma    = pars[2:d$numParams]
    V        = alpha*(d$P + d$X%*%gamma)
    logit    = getMnlLogit(V, d)
    probDiff = d$weights*(d$choice - logit)
    # Get the gradient for alpha and gamma, then combine them
    gradLLAlpha = t(d$P + d$X%*%gamma)%*%probDiff
    gradLLGamma = alpha*(t(d$X)%*%probDiff)
    negGradLL   = -1*c(gradLLAlpha, gradLLGamma)
    return(negGradLL)
}

# Computes the negative likelihood for a mixed logit model.
negLLMxlWtp = function(pars, d){
    # Get the draws of beta using the current mu and sigma values (pars)
    # Separate out alpha and gamma
    betaDraws  = makeBetaDraws(pars, d)
    alphaDraws = matrix(rep(betaDraws[,1], nrow(d$X)), ncol=d$numDraws,
                        byrow=T)
    gammaDraws = matrix(betaDraws[,2:ncol(betaDraws)], nrow=d$numDraws)
    # Now compute the negLL
    Pmat       = matrix(rep(d$P, d$numDraws), ncol=d$numDraws, byrow=F)
    VDraws     = alphaDraws*(Pmat + d$X%*%t(gammaDraws))
    logitDraws = getMxlLogitDraws(VDraws, d)
    pHat       = rowMeans(logitDraws)
    negLL      = -1*sum(d$weights*d$choice*log(pHat))
    return(negLL)
}

negGradLLMxlWtp = function(pars, d) {
    # Get the draws of beta using the current mu and sigma values (pars)
    # Separate out alpha and gamma
    betaDraws     = makeBetaDraws(pars, d)
    alphaDraws    = betaDraws[,1]
    alphaDrawsMat = matrix(rep(alphaDraws, nrow(d$X)), ncol=d$numDraws,
                           byrow=T)
    gammaDraws    = matrix(betaDraws[,2:ncol(betaDraws)], nrow=d$numDraws)
    # Get pHat
    Pmat       = matrix(rep(d$P, d$numDraws), ncol=d$numDraws, byrow=F)
    VDraws     = alphaDrawsMat*(Pmat + d$X%*%t(gammaDraws))
    logitDraws = getMxlLogitDraws(VDraws, d)
    pHat       = rowMeans(logitDraws)
    # Get the sum of the gradient of V for all parameters across all draws
    gradAlpha      = matrix(0, nrow=nrow(d$X), ncol=1)
    gradMuGamma    = matrix(0, nrow=nrow(d$X), ncol=d$numBetas-1)
    gradSigmaGamma = gradMuGamma
    for (i in 1:d$numDraws) {
        V        = VDraws[,i]
        alpha    = alphaDraws[i]
        gamma    = gammaDraws[i,]
        draws    = d$standardDraws[i,]
        logit    = logitDraws[,i]
        alphaMat = alphaDrawsMat[,i]
        gammaMat = matrix(rep(gamma, nrow(d$X)), ncol=ncol(d$X), byrow=T)
        drawsMat = matrix(rep(draws, nrow(d$X)), ncol=d$numBetas,byrow=T)
        logitMat = matrix(rep(logit, ncol(d$X)), ncol=ncol(d$X), byrow=F)
        # get the inner gradient partial derivatives for alpha, muGamma, and sigmaGamma
        gradAlpha      = gradAlpha + getGradAlpha(d, alpha, V, logit)
        gradMuGamma    = gradMuGamma + getGradGamma(d, alpha, gammaMat,
                                       drawsMat, logitMat, 'mu')
        gradSigmaGamma = gradSigmaGamma + getGradGamma(d, alpha, gammaMat,
                                          drawsMat, logitMat, 'sigma')
    }
    # Get the average of the gradient of V for all parameters across all draws
    weightsMat         = matrix(rep(d$weights, d$numBetas-1),
                         ncol=ncol(d$X), byrow=F)
    gradAlpha          = d$weights*(gradAlpha / d$numDraws)
    gradMuGamma        = weightsMat*(gradMuGamma / d$numDraws)
    gradMu             = c(gradAlpha, gradMuGamma)
    gradSigmaGamma     = weightsMat*(gradSigmaGamma / d$numDraws)
    gradSigma          = gradSigmaGamma
    pHatInvChosenMu    = matrix(rep(d$choice*(1/pHat), d$numBetas),
                         ncol=d$numBetas, byrow=F)
    pHatInvChosenSigma = matrix(rep(d$choice*(1/pHat), d$numRandom),
                         ncol=d$numRandom, byrow=F)
    gradMu             = colSums(pHatInvChosenMu*gradMu)
    gradSigma          = colSums(pHatInvChosenSigma*gradSigma)
    # Remove sigma gradient for fixed coefficients
    gradSigma = gradSigma[d$randomCovariateIDs-1] # minus 1 because alpha is
    #                                               excluded
    # Get the negGradLL
    negGradLL        = -1*c(gradMu, gradSigma)
    names(negGradLL) = names(pars)
    return(negGradLL)
}

getGradAlpha = function(d, alpha, V, logit) {
    partial     = V / alpha
    sumLogitMat = rowsum(logit*partial, group=d$observationID)
    repTimes    = as.numeric(table(d$observationID))
    temp        = matrix(rep(sumLogitMat, times=repTimes), ncol=1, byrow=F)
    innerGrad   = logit*(partial - temp)
    return(innerGrad)
}

getGradGamma = function(d, alpha, gammaMat, drawsMat, logitMat, id) {
    partial = alpha*d$X
    if (id == 'sigma') {partial = partial*drawsMat[,2:ncol(drawsMat)]}
    # correct for logNormal covariates
    if (length(d$logNormIDs) > 0) {
        partial[,d$logNormIDs-1] =
        partial[,d$logNormIDs-1]*gammaMat[,d$logNormIDs-1]
        # minus 1 because alpha is excluded
    }
    sumLogitMat = rowsum(logitMat*partial, group=d$observationID)
    repTimes    = rep(as.numeric(table(d$observationID)), each=ncol(d$X))
    tempMat     = matrix(rep(sumLogitMat, times=repTimes),
                         ncol=ncol(partial), byrow=F)
    innerGrad   = logitMat*(partial - tempMat)
    return(innerGrad)
}



# ============================================================================
# Functions for taking draws in a mixed logit model
# ============================================================================

# Returns shifted normal draws for each parameter
makeBetaDraws = function(pars, d){
    means     = pars[1:d$numBetas]
    sds       = rep(0, d$numBetas)
    sds[d$randomCovariateIDs] = pars[(d$numBetas+1):length(pars)]
    meanMat   = matrix(rep(means, d$numDraws), ncol=d$numBetas, byrow=T)
    sdMat     = matrix(rep(sds, d$numDraws), ncol=d$numBetas, byrow=T)
    # shift draws by mu and sigma
    betaDraws = d$standardDraws*sdMat + meanMat
    # exponentiate logNormal draws
    if (length(d$logNormIDs) > 0) {
        betaDraws[,d$logNormIDs] = exp(betaDraws[,d$logNormIDs])
    }
    colnames(betaDraws) = names(pars)[1:d$numBetas]
    return(betaDraws)
}

# Returns a matrix of numDraws x numBetas random standard normal draws
getStandardNormalHaltonDraws = function(numDraws, numBetas) {
    draws    = halton(numDraws, normal=T)
    drawsMat = matrix(rep(draws, numBetas), ncol=numBetas)
    return(drawsMat)
}
