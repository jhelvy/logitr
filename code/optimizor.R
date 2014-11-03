# ============================================================================
# Initialize the matrices to store the results
d$parMat  = matrix(0, ncol=d$numMultiStarts, nrow=d$numParams)
d$logLMat = matrix(0, ncol=d$numMultiStarts, nrow=1)
row.names(d$parMat) = d$allParNames

# Print a header message notifying that the loop has started
printTitle('RUNNING MODELS')
printHeader(d)
cat('RUNNING MULTISTART RUNS\n')

# ============================================================================
# Run the optimization loop numMultiStarts times
bestLogL       = 10^10
bestModelIndex = 1

for (i in 1:d$numMultiStarts) {
    tryCatch({
    # Set the starting point
    startingPars = d$startPoints[,i]
    # Run the model
    tempModel = runFastOptimization(startingPars, d)
    # Save the results
    modelLogL     = getModelLogLValue(tempModel)
    modelPars     = getModelPars(tempModel, d)
    d$logLMat[,i] = modelLogL
    d$parMat[,i]  = modelPars
    # Keep the best pars for final run
    if ((-1*modelLogL) < bestLogL) {
        bestLogL       = -1*modelLogL
        bestPars       = modelPars
        bestModelIndex = i
    }
    # Print run results
    cat('iter ', i, ' of ', d$numMultiStarts, '...negative logL value = ',
        -1*modelLogL, '\n')
    }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# ============================================================================
# Get the null logL
d$nullLogL = d$negLL(0*startingPars, d)

# ============================================================================
# Run the optimization again using the starting point from the best model
# result from the previous loop
printHorizontalLine()
cat('The best run was run #', bestModelIndex, '\n')
cat('Now running a model with "maxFinalIters" iterations, starting from \n')
cat('where the best run ended\n')
printHorizontalLine()

# Run the model
d$bestModel = runOptimization(bestPars, d)

# ============================================================================
# Remove any lingering global variables that are not in d
rm(tempModel)
rm(startingPars)
rm(modelLogL)
rm(modelPars)
rm(i)
rm(bestPars)
rm(bestLogL)

# ============================================================================
# Summarize and print the results
source(file='./code/summarizeResults.R')
