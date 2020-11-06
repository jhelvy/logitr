# Load libraries and data -----------------------------------------------------

# Main library for model estimation is "logitr"- documentation here:
# https://jhelvy.github.io/logitr

# To install, install the "remotes" library, then install "logitr" from github:
# # install.packages("remotes")
# remotes::install_github('jhelvy/logitr')

library('logitr')
library(readr)
options(dplyr.width = Inf)

china_data <- read_csv(here::here('data', 'china_cars.csv'))
us_data    <- read_csv(here::here('data', 'us_cars.csv'))

# MNL Models ------------------------------------------------------------------

# --- China models ---

# Preference space
mnl_pref_china <- logitr(
  data       = china_data,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'price', 'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  weightsName = 'weights'
)

# WTP space
mnl_wtp_china <- logitr(
  data       = china_data,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  priceName  = 'price',
  modelSpace = 'wtp',
  weightsName = 'weights',
  options    = list(
    numMultiStarts = 10,
    startParBounds = c(-10, 10))
)

# Compare WTP
wtpCompare(mnl_pref_china, mnl_wtp_china, "price")

# --- US models ---

# Preference space
mnl_pref_us <- logitr(
  data       = us_data,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'price', 'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  weightsName = 'weights'
)

# WTP space
mnl_wtp_us <- logitr(
  data       = us_data,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  priceName  = 'price',
  modelSpace = 'wtp',
  weightsName = 'weights',
  options = list(
    numMultiStarts = 10,
    startParBounds = c(-10, 10)))

# Compare WTP
wtpCompare(mnl_pref_us, mnl_wtp_us, "price")

# MXL Models ------------------------------------------------------------------

# --- China models ---

mxl_pref_china <- logitr(
  data       = china_data,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'price', 'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  randPars   = c(
    hev = 'n', phev10 = 'n', phev20 = 'n', phev40 = 'n', bev75 = 'n',
    bev100 = 'n', bev150 = 'n', american = 'n', japanese = 'n', chinese = 'n',
    skorean = 'n', phevFastcharge = 'n', bevFastcharge = 'n', accelTime = 'n',
    opCost = 'n'),
  modelSpace = 'pref',
  weightsName = 'weights',
  options    = list(
    numMultiStarts = 5,
    startVals      = mnl_wtp_china$Estimate,
    startParBounds = c(-5, 5),
    numDraws       = 125))

mxl_wtp_china <- logitr(
  data       = china_data,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'price', 'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  priceName  = 'price',
  randPars   = c(
    hev = 'n', phev10 = 'n', phev20 = 'n', phev40 = 'n', bev75 = 'n',
    bev100 = 'n', bev150 = 'n', american = 'n', japanese = 'n', chinese = 'n',
    skorean = 'n', phevFastcharge = 'n', bevFastcharge = 'n', accelTime = 'n',
    opCost = 'n'),
  modelSpace = 'wtp',
  weightsName = 'weights',
  options    = list(
    numMultiStarts = 5,
    startVals      = mnl_wtp_china$Estimate,
    startParBounds = c(-5, 5),
    numDraws       = 125))

# --- US models ---

mxl_pref_us <- logitr(
  data       = us_data,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'price', 'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  randPars   = c(
    hev = 'n', phev10 = 'n', phev20 = 'n', phev40 = 'n', bev75 = 'n',
    bev100 = 'n', bev150 = 'n', american = 'n', japanese = 'n', chinese = 'n',
    skorean = 'n', phevFastcharge = 'n', bevFastcharge = 'n', accelTime = 'n',
    opCost = 'n'),
  modelSpace = 'pref',
  weightsName = 'weights',
  options    = list(
    numMultiStarts = 5,
    startVals      = mnl_wtp_us$Estimate,
    startParBounds = c(-5, 5),
    numDraws       = 125))

mxl_wtp_us <- logitr(
  data       = us_data,
  choiceName = 'choice',
  obsIDName  = 'obsnum',
  parNames   = c(
    'price', 'hev', 'phev10', 'phev20', 'phev40', 'bev75', 'bev100', 'bev150',
    'american', 'japanese', 'chinese', 'skorean', 'phevFastcharge',
    'bevFastcharge','opCost', 'accelTime'),
  priceName  = 'price',
  randPars   = c(
    hev = 'n', phev10 = 'n', phev20 = 'n', phev40 = 'n', bev75 = 'n',
    bev100 = 'n', bev150 = 'n', american = 'n', japanese = 'n', chinese = 'n',
    skorean = 'n', phevFastcharge = 'n', bevFastcharge = 'n', accelTime = 'n',
    opCost = 'n'),
  modelSpace = 'wtp',
  weightsName = 'weights',
  options    = list(
    numMultiStarts = 5,
    startVals      = mnl_wtp_us$Estimate,
    startParBounds = c(-5, 5),
    numDraws       = 125))

# ---------------------------------------------------------------------------
# Save estimated model objects

models_weighted <- list(
  mnl_pref_china = mnl_pref_china,
  mnl_wtp_china  = mnl_wtp_china,
  mnl_pref_us    = mnl_pref_us,
  mnl_wtp_us     = mnl_wtp_us,
  mxl_pref_china = mxl_pref_china,
  mxl_wtp_china  = mxl_wtp_china,
  mxl_pref_us    = mxl_pref_us,
  mxl_wtp_us     = mxl_wtp_us
)

saveRDS(models_weighted, here::here('results', 'models_weighted.Rds'))
