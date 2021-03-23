
library(logitr)
library(tidyverse)

devtools::load_all()

data       = yogurt
choiceName = 'choice'
obsIDName  = 'newIDs'
parNames   = c('price', 'feat', 'hiland', 'yoplait', 'dannon')
priceName = NULL
randPars = NULL
randPrice = NULL
modelSpace = "pref"
weightsName = NULL
options = list()



