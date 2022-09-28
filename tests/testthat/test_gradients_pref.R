context("Preference space gradients")

library(logitr)

# Common settings across all test
yogurt$neg_price <- -1*yogurt$price
data            = as.data.frame(yogurt)
data            = subset(data, data$id < 30)
outcome         = 'choice'
obsID           = 'obsID'
scalePar        = NULL
randScale       = NULL
clusterID       = NULL
weights         = NULL
robust          = FALSE
startValBounds  = c(-1, 1)
startVals       = NULL
numMultiStarts  = 5
standardDraws   = NULL
drawType        = 'halton'
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
  correlation = FALSE,
  panelID     = NULL,
  scaleInputs = TRUE
) {
  # Creates random starting points
  mi <- getModelInputs(
    data, outcome, obsID, pars, randPars, scalePar, randScale,
    weights, panelID, clusterID, robust, startValBounds, startVals,
    numMultiStarts, useAnalyticGrad, scaleInputs, standardDraws, drawType,
    numDraws, numCores, vcov, predict, correlation, call, options
  )
  mi <- makeModelInputsList(mi, numMultiStarts)[[1]]
  pars <- mi$model$startVals

  # Compute gradients
  grad_analytic <- as.vector(mi$evalFuncs$negGradLL(pars, mi))
  grad_numeric <- as.vector(getNumericNegGradLL(pars, mi))

  # Compare
  check <- data.frame(
      analytic = grad_analytic,
      numeric = grad_numeric
  )
  check$diff <- round(check$analytic - check$numeric, 4)
  return(sum(check$diff) == 0)
}

test_that("Gradients for MNL", {

  # Scaled? TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = NULL,
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

  # Scaled? FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = NULL,
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = FALSE
  ))
})

test_that("Gradients MXL, normal parameters", {

  # Scaled?      FALSE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

  # Scaled?      FALSE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    correlation = TRUE,
    panelID     = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      FALSE
  # Correlation? FALSE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    correlation = FALSE,
    panelID     = 'id',
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    correlation = TRUE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

  # Scaled?      FALSE
  # Correlation? TRUE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    correlation = TRUE,
    panelID     = 'id',
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    correlation = FALSE,
    panelID     = 'id',
    scaleInputs = TRUE
  ))

  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "n", brand = "n"),
    correlation = TRUE,
    panelID     = 'id',
    scaleInputs = TRUE
  ))

})

test_that("Gradients for MXL, log-normal parameters", {

  # Scaled?      FALSE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "ln", brand = "ln"),
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "ln", brand = "ln"),
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

  # Scaled?      FALSE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "ln", brand = "ln"),
    correlation = TRUE,
    panelID     = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      FALSE
  # Correlation? FALSE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "ln", brand = "ln"),
    correlation = FALSE,
    panelID     = 'id',
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "ln", brand = "ln"),
    correlation = TRUE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

  # Scaled?      FALSE
  # Correlation? TRUE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "ln", brand = "ln"),
    correlation = TRUE,
    panelID     = 'id',
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "ln", brand = "ln"),
    correlation = FALSE,
    panelID     = 'id',
    scaleInputs = TRUE
  ))

  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "ln", brand = "ln"),
    correlation = TRUE,
    panelID     = 'id',
    scaleInputs = TRUE
  ))

  # Additional test with negative of price being ln
  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('neg_price', 'feat', 'brand'),
    randPars    = c(neg_price = "ln", feat = "n", brand = "ln"),
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

  # Additional test with negative of price being ln
  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('neg_price', 'feat', 'brand'),
    randPars    = c(neg_price = "ln", feat = "n", brand = "ln"),
    correlation = TRUE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

})

test_that("Gradients for MXL, censored-normal parameters", {

  # Scaled?      FALSE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "cn", brand = "cn"),
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "cn", brand = "cn"),
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

  # Scaled?      FALSE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "cn", brand = "cn"),
    correlation = TRUE,
    panelID     = NULL,
    scaleInputs = FALSE
  ))

  # Scaled?      FALSE
  # Correlation? FALSE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "cn", brand = "cn"),
    correlation = FALSE,
    panelID     = 'id',
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "cn", brand = "cn"),
    correlation = TRUE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

  # Scaled?      FALSE
  # Correlation? TRUE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "cn", brand = "cn"),
    correlation = TRUE,
    panelID     = 'id',
    scaleInputs = FALSE
  ))

  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "cn", brand = "cn"),
    correlation = FALSE,
    panelID     = 'id',
    scaleInputs = TRUE
  ))

  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       TRUE
  expect_true(grad_check(
    pars        = c('price', 'feat', 'brand'),
    randPars    = c(price = "n", feat = "cn", brand = "cn"),
    correlation = TRUE,
    panelID     = 'id',
    scaleInputs = TRUE
  ))

  # Additional test with negative of price being cn
  # Scaled?      TRUE
  # Correlation? FALSE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('neg_price', 'feat', 'brand'),
    randPars    = c(neg_price = "cn", feat = "n", brand = "cn"),
    correlation = FALSE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

  # Additional test with negative of price being ln
  # Scaled?      TRUE
  # Correlation? TRUE
  # panel?       FALSE
  expect_true(grad_check(
    pars        = c('neg_price', 'feat', 'brand'),
    randPars    = c(neg_price = "cn", feat = "n", brand = "cn"),
    correlation = TRUE,
    panelID     = NULL,
    scaleInputs = TRUE
  ))

})
