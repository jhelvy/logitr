# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# ============================================================================
# Estimate homogeneous MNL models with interactions

# Continuous variable interactions
model_price_feat <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand', 'price*feat')
)

# The model now has an estimated coefficient for the `price*feat` effect:
summary(model_price_feat)

# Discrete variable interactions
model_price_brand <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand', 'price*brand')
)

# The model now has three estimated coefficients for the `price*brand` effect:
summary(model_price_brand)

# ============================================================================
# Estimate homogeneous MNL models with individual-specific interactions

# In this case, you don't want a coefficient for a specific group,
# but rather just an interaction term with individuals from that group

# Create group A dummies
yogurt$groupA <- ifelse(yogurt$obsID %% 2 == 0, 1, 0)

# Create dummy coefficients for group interaction with price
yogurt$price_groupA <- yogurt$price*yogurt$groupA

# Continuous variable interactions
model_price_group <- logitr(
  data    = yogurt,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c('price', 'feat', 'brand', 'price_groupA')
)

summary(model_price_group)

# ============================================================================
# Estimate heterogeneous MXL models with interactions

model_price_feat_mxl <- logitr(
  data     = yogurt,
  outcome  = 'choice',
  obsID    = 'obsID',
  pars     = c('price', 'feat', 'brand', 'price*feat'),
  randPars = c(feat = "n")
)

# The interaction term of price*feat is interpreted as a difference in the
# feat_mu value depending on price
summary(model_price_feat_mxl)

# ============================================================================
# Save results
saveRDS(model_price_feat,
  here::here('inst', 'extdata', 'int_model_price_feat.Rds'))
saveRDS(model_price_brand,
  here::here('inst', 'extdata', 'int_model_price_brand.Rds'))
saveRDS(model_price_group,
  here::here('inst', 'extdata', 'model_price_group.Rds'))
saveRDS(model_price_feat_mxl,
  here::here('inst', 'extdata', 'model_price_feat_mxl.Rds'))
