# Summarize and store the results
d$pars          = getModelPars(d$bestModel, d)
d$hessian       = getModelHessian(d$bestModel, d)
d$sds           = getModelSDs(d$bestModel, d)
d$scaledPars    = getScaledModelPars(d$pars, d$modelSpace, d$scaleFactors)
d$scaledHessian = getScaledModelHessian(d$hessian, d$modelSpace,
                  d$scaleFactors)
d$scaledSDs     = getScaledModelSDs(d$hessian, d$modelSpace,
                  d$scaleFactors)
d$logL          = getModelLogLValue(d$bestModel)
d$summary       = getModelSummary(d)

# Rename 'd' as 'model' for exporting results
model = d
rm(d)

# Print result
printHorizontalLine()
cat(rep(' ',30), 'RESULTS SUMMARY', rep(' ',30), '\n', sep='')
printHorizontalLine()
print(model$summary)
