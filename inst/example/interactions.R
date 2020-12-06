# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the yogurt data
head(yogurt)

# ============================================================================
# Estimate homogeneous MNL models with interactions

# Continuous variable interactions
model_price_feat <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'brand', 'price*feat')
)

# The model now has an estimated coefficient for the `price*feat` effect:
summary(model_price_feat)

# Discrete variable interactions
model_price_brand <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'brand', 'price*brand')
)

# The model now has three estimated coefficients for the `price*brand` effect:
summary(model_price_brand)

# Save results
saveRDS(model_price_feat,
  here::here('inst', 'extdata', 'int_model_price_feat.Rds'))
saveRDS(model_price_brand,
  here::here('inst', 'extdata', 'int_model_price_brand.Rds'))


