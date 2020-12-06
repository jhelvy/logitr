# # Install logitr package from github
# devtools::install_github('jhelvy/logitr')

# Load logitr package
library('logitr')

# Preview the yogurt data
head(yogurt)

# ============================================================================
# Estimate MNL models with different encodings

model_default <- logitr(
  data       = cars_us,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'price', 'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100',
    'bev150', 'american', 'japanese', 'chinese', 'skorean',
    'phevFastcharge', 'bevFastcharge','opCost', 'accelTime'))

# In this model, since the price variable is a "double" variable type,
# it is modeled as a continuous variable by default:
typeof(cars_us$price)
summary(model_default)

# To model price as a categorical variable, simple change it to a
# "character" or "factor" type:
cars_us$price <- as.character(cars_us$price)
typeof(cars_us$price)
model_character_price <- logitr(
  data       = cars_us,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'price', 'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100',
    'bev150', 'american', 'japanese', 'chinese', 'skorean',
    'phevFastcharge', 'bevFastcharge','opCost', 'accelTime'))

# Now price is modeled with a coefficient for all but the first level:
typeof(cars_us$price)
summary(model_character_price)

## 2) Create dummy-coded variables
cars_us_dummy <- dummyCode(df = cars_us, vars = "price")
model_dummy_price <- logitr(
  data       = cars_us_dummy,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    "price_15", "price_18", "price_23", "price_32",
    'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100',
    'bev150', 'american', 'japanese', 'chinese', 'skorean',
    'phevFastcharge', 'bevFastcharge','opCost', 'accelTime'))

# Save results
saveRDS(model_default,
  here::here('inst', 'extdata', 'encoding_model_default.Rds'))
saveRDS(model_character_price,
  here::here('inst', 'extdata', 'encoding_model_character_price.Rds'))
saveRDS(model_dummy_price,
  here::here('inst', 'extdata', 'encoding_model_dummy_price.Rds'))
