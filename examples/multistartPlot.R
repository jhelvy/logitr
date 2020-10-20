# ============================================================================
# Plotting the results of a logitr multistart model
# ============================================================================
# After running a multistart, it can be helpful to visualize the
# starting parameters used across all multistart runs and compare them against
# the solution coefficients

# Install logitr package from github
# library('devtools')
# install_github('jhelvy/logitr')

# Load libraries
library(logitr)
library(ggplot2)

# Load Functions
getMultistartPars = function(model) {
    allModels = model$models
    coefs     = coef(model$bestModel)
    startPars = c()
    for (i in 1:length(allModels)) {
        startPars = c(startPars, allModels[[i]]$startPars)
    }
    pars = data.frame(
        startPar = startPars,
        coef     = rep(coefs, length(allModels)))
    pars$parName = rep(names(coefs), length(allModels))
    pars$run     = rep(seq(length(allModels)), each=length(coefs))
    return(pars)
}

plotMultistartPars = function(model) {
    pars = getMultistartPars(model)
    plot = ggplot(pars,
        aes(x = parName, y = startPar)) +
        geom_point(color = 'blue') +
        geom_errorbar(aes(ymax = coef, ymin = coef), color = "#AA0000") +
    theme_bw() + 
    labs(x = "Parameter name", 
         y = "Starting parameter",
         title = "Comparison of starting parameters (blue points) to\nbest coefficient estimate (red lines)")
    return(plot)
}

# Estimate a MNL model in the WTP Space using a multistart:
data(yogurt)
model = logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
  priceName  = 'price',
  modelSpace = 'wtp',
  options = list(
    numMultiStarts = 20,
    keepAllRuns    = TRUE,
    startParBounds = c(-10, 10)))

plotMultistartPars(model)
