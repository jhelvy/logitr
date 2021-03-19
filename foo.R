
library(logitr)
library(tidyverse)

obsIDs <- unique(yogurt$obsID)
newIDs1 <- sample(seq(3000), length(obsIDs), replace = FALSE)

yogurt_new <- yogurt %>%
  left_join(
    data.frame(
      obsID = obsIDs,
      newIDs = newIDs1
    ), by = "obsID"
  )

model <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'hiland', 'yoplait', 'dannon')
)

model_new1 <- logitr(
  data       = yogurt_new,
  choiceName = 'choice',
  obsIDName  = 'newIDs',
  parNames   = c('price', 'feat', 'hiland', 'yoplait', 'dannon')
)

model_new2 <- logitr(
  data       = yogurt_new %>% arrange(newIDs),
  choiceName = 'choice',
  obsIDName  = 'newIDs',
  parNames   = c('price', 'feat', 'hiland', 'yoplait', 'dannon')
)

summary(model)
summary(model_new1)
summary(model_new2)

cbind(coef(model), coef(model_new1), coef(model_new2))





devtools::load_all()

data       = yogurt_new1
choiceName = 'choice'
obsIDName  = 'newIDs'
parNames   = c('price', 'feat', 'hiland', 'yoplait', 'dannon')
priceName = NULL
randPars = NULL
randPrice = NULL
modelSpace = "pref"
weightsName = NULL
options = list()



