# Make the primary list called 'd' that will store all the data and
# model settings. This will avoid the use of global variables and will
# make passing data between functions MUCH simpler. 'd' is for 'data'!
d = list(choiceData=choiceData,
         modelType=modelType,
         modelSpace=modelSpace,
         useWeights=useWeights,
         useAnalyticGrad=useAnalyticGrad,
         scaleParams=scaleParams,
         numMultiStarts=numMultiStarts,
         maxMultiStartIters=maxMultiStartIters,
         maxFinalIters=maxFinalIters,
         numDraws=numDraws,
         useOptimx=useOptimx,
         optAlgorithm=optAlgorithm,
         lowerBound=lowerBound,
         upperBound=upperBound,
         customStartPoints=customStartPoints,
         customLowerBound=customLowerBound,
         customUpperBound=customUpperBound,
         respondentID=respondentID,
         observationID=observationID,
         choice=choice,
         weights=weights,
         priceVar=priceVar,
         priceDist=priceDist,
         fixedVars=fixedVars,
         normalVars=normalVars,
         logNormalVars=logNormalVars)

# Remove all currently existing objects so that nothing is accidentally
# used as a global variable.
rm(choiceData)
rm(modelType)
rm(modelSpace)
rm(useWeights)
rm(useAnalyticGrad)
rm(scaleParams)
rm(numMultiStarts)
rm(maxMultiStartIters)
rm(maxFinalIters)
rm(numDraws)
rm(useOptimx)
rm(optAlgorithm)
rm(lowerBound)
rm(upperBound)
rm(respondentID)
rm(observationID)
rm(choice)
rm(weights)
rm(priceVar)
rm(priceDist)
rm(fixedVars)
rm(normalVars)
rm(logNormalVars)
rm(customStartPoints)
rm(customLowerBound)
rm(customUpperBound)
