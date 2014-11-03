# Sets the log likelihood and gradient functions based on the
# model type and model space

if ((d$modelType == 'mnl') & (d$modelSpace == 'pref')) {
    d$negLL     = negLLMnlPref
    d$negGradLL = negGradLLMnlPref
} else if ((d$modelType == 'mxl') & (d$modelSpace == 'pref')) {
    d$negLL     = negLLMxlPref
    d$negGradLL = negGradLLMxlPref
} else if ((d$modelType == 'mnl') & (d$modelSpace == 'wtp')) {
    d$negLL     = negLLMnlWtp
    d$negGradLL = negGradLLMnlWtp
} else {
    d$negLL     = negLLMxlWtp
    d$negGradLL = negGradLLMxlWtp
}
