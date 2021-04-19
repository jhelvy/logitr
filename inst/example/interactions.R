# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the yogurt data
head(yogurt)

# Set the factors for "brand" so that "weight" is the reference level
yogurt$brand <- factor(yogurt$brand, levels = c(
  "weight", "hiland", "yoplait", "dannon"))

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

# ============================================================================
# Estimate heterogeneous MXL models with interactions

model_price_feat_mxl <- logitr(
  data       = yogurt,
  choiceName = 'choice',
  obsIDName  = 'obsID',
  parNames   = c('price', 'feat', 'brand', 'price*feat'),
  randPars   = c(feat = "n")
)

# The interaction term of price*feat is interpreted as a difference in the
# feat_mu value depending on price
summary(model_price_feat_mxl)

# Save results
saveRDS(model_price_feat,
  here::here('inst', 'extdata', 'int_model_price_feat.Rds'))
saveRDS(model_price_brand,
  here::here('inst', 'extdata', 'int_model_price_brand.Rds'))
saveRDS(model_price_feat_mxl,
  here::here('inst', 'extdata', 'model_price_feat_mxl.Rds'))
