context("Analytic gradients are correct")

library(logitr)

# Common settings across all test
data            = as.data.frame(yogurt)
outcome         = 'choice'
obsID           = 'obsID'
weights         = NULL
robust          = FALSE
startParBounds  = c(-1, 1)
startVals       = NULL
numMultiStarts  = 5
standardDraws   = NULL
numDraws        = 100
numCores        = 1
vcov            = FALSE
predict         = TRUE
useAnalyticGrad = TRUE
call            = "foo"
options         = list(
  print_level = 0,
  xtol_rel    = 1.0e-6,
  xtol_abs    = 1.0e-6,
  ftol_rel    = 1.0e-6,
  ftol_abs    = 1.0e-6,
  maxeval     = 1000,
  algorithm   = "NLOPT_LD_LBFGS"
)

# Function for comparing gradients - only includes arguments to vary in tests
grad_check <- function(
  pars        = c('price', 'feat', 'brand'),
  randPars    = NULL,
  price       = NULL,
  modelSpace  = "pref",
  correlation = FALSE,
  randPrice   = NULL,
  panelID     = NULL,
  clusterID   = NULL,
  scaleInputs = TRUE
) {
  mi <- getModelInputs(
    data, outcome, obsID, pars, randPars, price, randPrice, modelSpace,
    weights, panelID, clusterID, robust, startParBounds, startVals,
    numMultiStarts, useAnalyticGrad, scaleInputs, standardDraws, numDraws,
    numCores, vcov, predict, correlation, call, options
  )
  # Creates random starting points
  mi <- makeModelInputsList(mi, numMultiStarts)[[2]]
  pars <- mi$model$startPars
  # Compute gradients
  grad_analytic <- as.vector(mi$evalFuncs$negGradLL(pars, mi))
  grad_numeric <- as.vector(getNumericNegGradLL(pars, mi))
  # Compare
  check <- data.frame(
      analytic = grad_analytic,
      numeric = grad_numeric
  )
  check$diff <- round(check$analytic - check$numeric, 4)
  test <- sum(check$diff)
  return(test == 0)
}

test_that("Gradients for preference space MNL", {

  # Scaled? TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = NULL,
    price       = NULL,
    modelSpace  = "pref",
    correlation = FALSE,
    randPrice   = NULL,
    panelID     = NULL,
    clusterID   = NULL,
    scaleInputs = TRUE
  ))

  # Scaled? FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = NULL,
    price       = NULL,
    modelSpace  = "pref",
    correlation = FALSE,
    randPrice   = NULL,
    panelID     = NULL,
    clusterID   = NULL,
    scaleInputs = FALSE
  ))
})

test_that("Gradients for WTP space MNL", {

  # Scaled? TRUE
  expect_true(grad_check(
    pars        = c('feat', 'brand'),
    randPars    = NULL,
    price       = "price",
    modelSpace  = "wtp",
    correlation = FALSE,
    randPrice   = NULL,
    panelID     = NULL,
    clusterID   = NULL,
    scaleInputs = TRUE
  ))

  # Scaled? FALSE
  expect_true(grad_check(
    pars        = c('feat', 'brand'),
    randPars    = NULL,
    price       = "price",
    modelSpace  = "wtp",
    correlation = FALSE,
    randPrice   = NULL,
    panelID     = NULL,
    clusterID   = NULL,
    scaleInputs = FALSE
  ))

})

test_that("Gradients for preference space MXL, normal parameters", {

  # Scaled?      FALSE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    price       = NULL,
    modelSpace  = "pref",
    correlation = FALSE,
    randPrice   = NULL,
    panelID     = NULL,
    clusterID   = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    price       = NULL,
    modelSpace  = "pref",
    correlation = FALSE,
    randPrice   = NULL,
    panelID     = NULL,
    clusterID   = NULL,
    scaleInputs = TRUE
  ))

  # Scaled?      FALSE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    price       = NULL,
    modelSpace  = "pref",
    correlation = TRUE,
    randPrice   = NULL,
    panelID     = NULL,
    clusterID   = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      FALSE
  # Correlation? FALSE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    price       = NULL,
    modelSpace  = "pref",
    correlation = FALSE,
    randPrice   = NULL,
    panelID     = 'id',
    clusterID   = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    price       = NULL,
    modelSpace  = "pref",
    correlation = TRUE,
    randPrice   = NULL,
    panelID     = NULL,
    clusterID   = NULL,
    scaleInputs = TRUE
  ))

  # Scaled?      FALSE
  # Correlation? TRUE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    price       = NULL,
    modelSpace  = "pref",
    correlation = TRUE,
    randPrice   = NULL,
    panelID     = 'id',
    clusterID   = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    price       = NULL,
    modelSpace  = "pref",
    correlation = FALSE,
    randPrice   = NULL,
    panelID     = 'id',
    clusterID   = NULL,
    scaleInputs = TRUE
  ))

  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    price       = NULL,
    modelSpace  = "pref",
    correlation = TRUE,
    randPrice   = NULL,
    panelID     = 'id',
    clusterID   = NULL,
    scaleInputs = TRUE
  ))

})
