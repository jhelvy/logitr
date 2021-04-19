# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Reset the factors for "brand" to the default levels
yogurt$brand <- factor(yogurt$brand, levels = c(
  "dannon", "weight", "hiland", "yoplait"))

mnl_pref_dannon <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'brand')
)

# Set the factors for "brand" so that "weight" is the reference level
yogurt$brand <- factor(yogurt$brand, levels = c(
  "weight", "hiland", "yoplait", "dannon"))

mnl_pref_weight <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'brand')
)

yogurt <- dummyCode(yogurt, "brand")

mnl_pref_dummies <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c(
    'price', 'feat', 'brandyoplait', 'branddannon', 'brandweight')
)

# Save results
saveRDS(mnl_pref_dannon,
  here::here('inst', 'extdata', 'mnl_pref_dannon.Rds'))
saveRDS(mnl_pref_weight,
  here::here('inst', 'extdata', 'mnl_pref_weight.Rds'))
saveRDS(mnl_pref_dummies,
  here::here('inst', 'extdata', 'mnl_pref_dummies.Rds'))
