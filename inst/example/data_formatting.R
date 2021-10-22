# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

mnl_pref_dannon <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand')
)

# Set the factors for "brand" so that "weight" is the reference level
yogurt$brand <- factor(yogurt$brand, levels = c(
  "weight", "hiland", "yoplait", "dannon"))

mnl_pref_weight <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand')
)

yogurt <- fastDummies::dummy_cols(yogurt, "brand")

mnl_pref_dummies <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c(
    'price', 'feat', 'brand_yoplait', 'brand_dannon', 'brand_weight')
)

# Save results
saveRDS(mnl_pref_dannon,
  here::here('inst', 'extdata', 'mnl_pref_dannon.Rds'))
saveRDS(mnl_pref_weight,
  here::here('inst', 'extdata', 'mnl_pref_weight.Rds'))
saveRDS(mnl_pref_dummies,
  here::here('inst', 'extdata', 'mnl_pref_dummies.Rds'))
