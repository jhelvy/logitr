# ============================================================================
# Logit and log-likelihood functions
# The log-likelihood function is given as the negative log-likelihood
# because the optimization performs a minimization
# ============================================================================

# ============================================================================
# MNL logit and log-likelihood functions
# ============================================================================

# # Logit fraction using data.table package (faster)
# # Returns the logit fraction for mnl (homogeneous) models
# getMnlLogit = function(V, obsID) {
#     data = data.table(V=V, obsID=obsID)
#     data[, expV:=exp(V.V1)]
#     data[, sumExpV:=sum(expV), by=obsID]
#     data[, logit:=expV/sumExpV]
#     return(data$logit)
# }

# Returns the logit fraction for mnl (homogeneous) models
getMnlLogit = function(V, obsID) {
    expV       = exp(V)
    sumExpV    = rowsum(expV, group=obsID)
    repTimes   = as.numeric(table(obsID))
    sumExpVMat = matrix(rep(sumExpV, times=repTimes), ncol=1)
    logit      = expV / sumExpVMat
    return(logit)
}

mnlNegLL = function(choice, logit) {
    negLL = -1*sum(choice*log(logit))
    return(negLL)
}

# Returns a list containing the negative log-likelihood and it's gradient
# Primary objective function for the nloptr optimizer.
mnlNegLLAndGradLL = function(pars, modelInputs) {
    logitFuncs = modelInputs$logitFuncs
    p          = modelInputs$price
    X          = modelInputs$X
    obsID      = modelInputs$obsID
    choice     = modelInputs$choice
    V          = logitFuncs$getMnlV(pars, modelInputs)
    logit      = logitFuncs$getMnlLogit(V, obsID)
    negLL      = logitFuncs$mnlNegLL(choice, logit)
    negGradLL  = logitFuncs$mnlNegGradLL(p, X, pars, choice, logit)
    return(list('objective'=negLL, 'gradient'=negGradLL))
}

getMnlNegLL = function(pars, modelInputs) {
    logitFuncs = modelInputs$logitFuncs
    obsID      = modelInputs$obsID
    choice     = modelInputs$choice
    V          = logitFuncs$getMnlV(pars, modelInputs)
    logit      = logitFuncs$getMnlLogit(V, obsID)
    negLL      = logitFuncs$mnlNegLL(choice, logit)
    return(negLL)
}

getMnlNegGradLL = function(pars, modelInputs) {
    logitFuncs = modelInputs$logitFuncs
    p          = modelInputs$price
    X          = modelInputs$X
    obsID      = modelInputs$obsID
    choice     = modelInputs$choice
    V          = logitFuncs$getMnlV(pars, modelInputs)
    logit      = logitFuncs$getMnlLogit(V, obsID)
    negGradLL  = logitFuncs$mnlNegGradLL(p, X, pars, choice, logit)
    return(negGradLL)
}

getMnlHessLL = function(pars, modelInputs) {
    hessLL = modelInputs$logitFuncs$mnlHessLL(pars, modelInputs)
    return(hessLL)
}

# ============================================================================
# MXL logit and log-likelihood functions
# ============================================================================
# The log-likelihood function is given here as the negative log-likelihood
# because the optim function performs a minimization

# Returns the logit fraction for all the draws in a mxl (heterogeneous) models
getMxlLogit = function(VDraws, obsID) {
    numDraws        = ncol(VDraws)
    expVDraws       = exp(VDraws)
    sumExpVDraws    = rowsum(expVDraws, group=obsID)
    repTimes        = rep(as.numeric(table(obsID)), each=numDraws)
    sumExpVDrawsMat = matrix(rep(sumExpVDraws, times=repTimes),
                      ncol=numDraws, byrow=F)
    logitDraws      = expVDraws / sumExpVDrawsMat
    return(logitDraws)
}

mxlNegLL = function(choice, pHat) {
    negLL = -1*sum(choice*log(pHat))
    return(negLL)
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
mxlNegLLAndGradLL = function(pars, modelInputs){
    logitFuncs    = modelInputs$logitFuncs
    X             = modelInputs$X
    obsID         = modelInputs$obsID
    choice        = modelInputs$choice
    parSetup      = modelInputs$parSetup
    standardDraws = modelInputs$standardDraws
    betaDraws     = makeBetaDraws(pars, modelInputs)
    VDraws        = logitFuncs$getMxlV(betaDraws, modelInputs)
    logitDraws    = logitFuncs$getMxlLogit(VDraws, obsID)
    pHat          = rowMeans(logitDraws)
    negLL         = logitFuncs$mxlNegLL(choice, pHat)
    negGradLL     = logitFuncs$mxlNegGradLL(X, parSetup, obsID, choice,
                    standardDraws, betaDraws, VDraws, logitDraws, pHat)
    return(list('objective'=negLL, 'gradient'=negGradLL))
}

# Returns the negative log-likelihood of an mxl (heterogeneous) model
getMxlNegLL = function(pars, modelInputs){
    logitFuncs = modelInputs$logitFuncs
    obsID      = modelInputs$obsID
    choice     = modelInputs$choice
    betaDraws  = makeBetaDraws(pars, modelInputs)
    VDraws     = logitFuncs$getMxlV(betaDraws, modelInputs)
    logitDraws = logitFuncs$getMxlLogit(VDraws, obsID)
    pHat       = rowMeans(logitDraws)
    negLL      = logitFuncs$mxlNegLL(choice, pHat)
    return(negLL)
}

getMxlNegGradLL = function(pars, modelInputs) {
    logitFuncs    = modelInputs$logitFuncs
    X             = modelInputs$X
    obsID         = modelInputs$obsID
    choice        = modelInputs$choice
    parSetup      = modelInputs$parSetup
    standardDraws = modelInputs$standardDraws
    betaDraws     = makeBetaDraws(pars, modelInputs)
    VDraws        = logitFuncs$getMxlV(betaDraws, modelInputs)
    logitDraws    = logitFuncs$getMxlLogit(VDraws, obsID)
    pHat          = rowMeans(logitDraws)
    negGradLL     = logitFuncs$mxlNegGradLL(X, parSetup, obsID, choice,
                    standardDraws, betaDraws, VDraws, logitDraws, pHat)
    return(negGradLL)
}

# ============================================================================
# Numerical log-likelihood functions
# ============================================================================

mnlNegLLAndNumericGradLL = function(pars, modelInputs) {
    negLL     = getMnlNegLL(pars, modelInputs)
    negGradLL = getNumericNegGradLL(pars, modelInputs)
    return(list('objective'=negLL, 'gradient'=negGradLL))
}

mxlNegLLAndNumericGradLL = function(pars, modelInputs) {
    negLL     = getMxlNegLL(pars, modelInputs)
    negGradLL = getNumericNegGradLL(pars, modelInputs)
    return(list('objective'=negLL, 'gradient'=negGradLL))
}

getNumericNegGradLL = function(pars, modelInputs) {
    return(nl.jacobian(
        x0          = pars,
        fn          = modelInputs$evalFuncs$negLL,
        modelInputs = modelInputs))
}

getNumericHessLL = function(pars, modelInputs) {
    return(-1*nl.jacobian(
        x0          = pars,
        fn          = modelInputs$evalFuncs$negGradLL,
        modelInputs = modelInputs))
}

# ============================================================================
# Preference Space Logit Functions - MNL models
# ============================================================================

getMnlV.pref = function(pars, modelInputs) {
    V = modelInputs$X %*% as.numeric(pars)
    return(V)
}

mnlNegGradLL.pref = function(p, X, pars, choice, logit) {
    negGradLL = -1*(t(X) %*% (choice - logit))
    return(negGradLL)
}

# Returns the hessian of the log-likelihood at the given pars
mnlHessLL.pref = function(pars, modelInputs) {
    X        = modelInputs$X
    obsID    = modelInputs$obsID
    choice   = modelInputs$choice
    V        = getMnlV.pref(pars, modelInputs)
    logit    = getMnlLogit(V, obsID)
    diffMat  = getDiffMatByObsID.pref(logit, X, obsID)
    logitMat = repmat(matrix(logit), 1, ncol(X))
    hessLL   = -1*(t(diffMat) %*% (logitMat*diffMat))
    return(hessLL)
}

# Used in computing the hessian
getDiffMatByObsID.pref = function(logit, X, obsID) {
    diffMat = matrix(0, nrow=length(obsID), ncol=ncol(X))
    for (id in sort(unique(obsID))) {
        indices   = which(obsID==id)
        tempX     = X[indices,]
        tempLogit = logit[indices]
        diffMat[indices,] = tempX - repmat(t(tempLogit)%*%tempX, nrow(tempX),1)
    }
    return(diffMat)
}

# ============================================================================
# Preference Space Logit Functions - MXL models
# ============================================================================

# Returns the observed utility
getMxlV.pref = function(betaDraws, modelInputs) {
    VDraws = modelInputs$X %*% t(betaDraws)
    return(VDraws)
}

# Computes the gradient of the negative likelihood for a mixed logit model.
mxlNegGradLL.pref = function(X, parSetup, obsID, choice, standardDraws,
    betaDraws, VDraws, logitDraws, pHat) {
    randParIDs = which(parSetup$dist != 0)
    numDraws   = nrow(standardDraws)
    numBetas   = ncol(standardDraws)
    logNormIDs = which(parSetup$dist==2)
    repTimes   = rep(as.numeric(table(obsID)), each=2*numBetas)
    # Compute the gradient of V for all parameters
    grad = matrix(0, nrow=nrow(X), ncol=2*numBetas)
    for (i in 1:numDraws) {
        Xtemp    = X
        beta     = betaDraws[i,]
        draws    = standardDraws[i,]
        logit    = logitDraws[,i]
        betaMat  = matrix(rep(beta, nrow(X)), ncol=numBetas, byrow=T)
        drawsMat = matrix(rep(draws, nrow(X)), ncol=numBetas, byrow=T)
        logitMat = matrix(rep(logit, numBetas), ncol=numBetas, byrow=F)
        logitMat = cbind(logitMat, logitMat)
        if (length(logNormIDs) > 0) {
            Xtemp[,logNormIDs] = Xtemp[,logNormIDs]*betaMat[,logNormIDs]
        }
        partial.mu    = Xtemp
        partial.sigma = Xtemp*drawsMat
        partial       = cbind(partial.mu, partial.sigma)
        temp          = rowsum(logitMat*partial, group=obsID)
        tempMat       = matrix(rep(temp, times=repTimes), ncol=ncol(partial),
                        byrow=F)
        grad = grad + logitMat*(partial - tempMat)
    }
    grad           = grad / numDraws
    pHatInvChosen  = matrix(rep(choice*(1/pHat), 2*numBetas), ncol=2*numBetas,
                            byrow=F)
    grad      = colSums(pHatInvChosen*grad)
    negGradLL = -1*grad[c(1:numBetas, numBetas + randParIDs)]
    return(negGradLL)
}


# ============================================================================
# WTP Space Logit Functions - MNL models
# ============================================================================

# Returns the observed utility
getMnlV.wtp = function(pars, modelInputs) {
    lambda = as.numeric(pars[1])
    beta   = as.numeric(pars[2:length(pars)])
    X      = modelInputs$X
    p      = modelInputs$price
    V      = lambda*(p + (X %*% beta))
    return(V)
}

# Returns the negative gradient of the log-likelihood
mnlNegGradLL.wtp = function(p, X, pars, choice, logit) {
    lambda       = as.numeric(pars[1])
    beta         = as.numeric(pars[2:length(pars)])
    gradLLLambda = t(p + (X %*% beta)) %*% (choice - logit)
    gradLLBeta   = lambda*(t(X) %*% (choice - logit))
    negGradLL    = -1*c(gradLLLambda, gradLLBeta)
    return(negGradLL)
}

# Returns the negative hessian of the log-likelihood
mnlHessLL.wtp = function(pars, modelInputs) {
    lambda   = as.numeric(pars[1])
    beta     = as.numeric(pars[2:length(pars)])
    X        = modelInputs$X
    p        = modelInputs$price
    choice   = modelInputs$choice
    obsID    = modelInputs$obsID
    V        = getMnlV.wtp(pars, modelInputs)
    logit    = getMnlLogit(V, obsID)
    diffMat  = getDiffMatByObsID.wtp(lambda, beta, p, X, logit, obsID)
    logitMat = repmat(matrix(logit), 1, ncol(diffMat))
    hessLL   = -1*(t(diffMat) %*% (logitMat*diffMat))
    return(hessLL)
}

# Used in computing the hessian
getDiffMatByObsID.wtp = function(lambda, beta, p, X, logit, obsID) {
    diffMatLambda = matrix(0, nrow=length(obsID), ncol=1)
    diffMatBeta = matrix(0, nrow=length(obsID), ncol=ncol(X))
    for (id in sort(unique(obsID))) {
        indices   = which(obsID==id)
        tempP     = as.matrix(p[indices])
        tempX     = as.matrix(X[indices,])
        tempLogit = logit[indices]
        tempMat   = tempP + (tempX %*% beta)
        diffMatLambda[indices,] = tempMat - repmat(t(tempLogit) %*% tempMat,
            nrow(tempP), 1)
        diffMatBeta[indices,] = lambda*tempX - repmat(t(tempLogit) %*% (lambda*
            tempX), nrow(tempX), 1)
    }
    return(cbind(diffMatLambda, diffMatBeta))
}

# ============================================================================
# WTP Space Logit Functions - MXL models
# ============================================================================

# Returns the observed utility
getMxlV.wtp = function(betaDraws, modelInputs) {
    X           = modelInputs$X
    p           = modelInputs$price
    numDraws    = nrow(betaDraws)
    lambdaDraws = matrix(rep(betaDraws[,1], nrow(X)),ncol=numDraws,byrow=T)
    gammaDraws  = matrix(betaDraws[,2:ncol(betaDraws)], nrow=numDraws)
    pMat        = matrix(rep(p, numDraws), ncol=numDraws, byrow=F)
    return(lambdaDraws*(pMat + X%*%t(gammaDraws)))
}

# ************************************************************************
# This function is still broken! Need to re-derive the gradient for WTP
# space MXL models
# Computes the gradient of the negative likelihood for a mixed logit model.
mxlNegGradLL.wtp = function(X, parSetup, obsID, choice, standardDraws,
    betaDraws, VDraws, logitDraws, pHat) {
    randParIDs     = which(parSetup$dist != 0)
    numDraws       = nrow(standardDraws)
    numBetas       = ncol(standardDraws)
    numRandom      = length(randParIDs)
    logNormIDs     = which(parSetup$dist==2)
    lambdaDraws    = betaDraws[,1]
    lambdaDrawsMat = matrix(rep(lambdaDraws, nrow(X)), ncol=numDraws,byrow=T)
    gammaDraws     = matrix(betaDraws[,2:ncol(betaDraws)], nrow=numDraws)
    # Compute the inner gradient summation across the draws for all
    # parameters
    gradLambda      = matrix(0, nrow=nrow(X), ncol=1)
    gradMuGamma    = matrix(0, nrow=nrow(X), ncol=numBetas-1)
    gradSigmaGamma = gradMuGamma
    for (i in 1:numDraws) {
        V         = VDraws[,i]
        lambda    = lambdaDraws[i]
        gamma     = gammaDraws[i,]
        draws     = standardDraws[i,]
        logit     = logitDraws[,i]
        lambdaMat = lambdaDrawsMat[,i]
        gammaMat  = matrix(rep(gamma, nrow(X)), ncol=ncol(X), byrow=T)
        drawsMat  = matrix(rep(draws, nrow(X)), ncol=numBetas,byrow=T)
        logitMat  = matrix(rep(logit, ncol(X)), ncol=ncol(X), byrow=F)
        # get the inner gradient partial derivatives for lambda, muGamma, and sigmaGamma
        gradLambda     = gradLambda +
                         getGradLambda.wtp(lambda, V, logit, obsID)
        gradMuGamma    = gradMuGamma +
                         getGradGamma.wtp(X, lambda, logNormIDs, gammaMat,
                         drawsMat, logitMat, obsID, 'mu')
        gradSigmaGamma = gradSigmaGamma +
                         getGradGamma.wtp(X, lambda, logNormIDs, gammaMat,
                         drawsMat, logitMat, obsID, 'sigma')
    }
    gradLambda          = gradLambda / numDraws
    gradMuGamma        = gradMuGamma / numDraws
    gradMu             = c(gradLambda, gradMuGamma)
    gradSigmaGamma     = gradSigmaGamma / numDraws
    gradSigma          = gradSigmaGamma
    pHatInvChosenMu    = matrix(rep(choice*(1/pHat), numBetas),
                                ncol=numBetas,byrow=F)
    pHatInvChosenSigma = matrix(rep(choice*(1/pHat), numRandom),
                                ncol=numRandom, byrow=F)
    gradMu             = colSums(pHatInvChosenMu*gradMu)
    gradSigma          = colSums(pHatInvChosenSigma*gradSigma)
    negGradLL          = -1*c(gradMu, gradSigma)
    names(negGradLL)   = names(pars)
    return(negGradLL)
}

getGradLambda.wtp = function(lambda, V, logit, obsID) {
    partial = V / lambda
    temp = matrix(rep(rowsum(logit*partial, group=obsID), each=3),
                     ncol=1, byrow=F)
    innerGrad = logit*(partial - temp)
    return(innerGrad)
}

getGradGamma.wtp = function(X, lambda, logNormIDs, gammaMat, drawsMat,
    logitMat, obsID, id) {
    partial = lambda*X
    if (id=='sigma') {partial = partial*drawsMat[,2:ncol(drawsMat)]}
    if (length(logNormIDs) > 0) {
        partial[,logNormIDs] =
        partial[,logNormIDs]*gammaMat[,logNormIDs]
    }
    tempMat = matrix(rep(rowsum(logitMat*partial, group=obsID), each=3),
                     ncol=ncol(partial), byrow=F)
    innerGrad = logitMat*(partial - tempMat)
    return(innerGrad)
}
# ************************************************************************
