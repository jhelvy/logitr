# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library(logitr)

# Preview the yogurt data
head(yogurt)

# ============================================================================
# Estimate heterogeneous MXL models

# Multistart MXL model in the Preference Space
model <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'hiland', 'yoplait', 'dannon'),
  randPars   = c(feat = 'n', hiland = 'n', yoplait = 'n', dannon = 'n'),
  options = list(numDraws = 50, xtol_rel = 1.0e-6, xtol_abs = 1.0e-6,
                 ftol_rel = 1.0e-6, ftol_abs = 1.0e-6))

summary(model)

cbind(coef(model1), coef(model2), coef(model3))
c(model1$logLik, model2$logLik, model3$logLik)
