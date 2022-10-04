context("Starting parameters")
library(logitr)

test_that("getStartVals returns all 0s for first iteration in a multistart loop (and not other iterations)", {
  modelInputs <- getModelInputs(
    data = yogurt,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat"),
    scalePar = NULL,
    randPars = NULL,
    randScale = NULL,
    weights = NULL,
    panelID = NULL,
    clusterID = NULL,
    robust = FALSE,
    startValBounds = c(-1, 1),
    startVals = NULL,
    numMultiStarts  = 1,
    useAnalyticGrad = TRUE,
    scaleInputs = TRUE,
    standardDraws = NULL,
    drawType = 'halton',
    numDraws = 50,
    numCores = NULL,
    vcov = FALSE,
    predict = TRUE,
    correlation = FALSE,
    call = NULL,
    options         = list(
      print_level = 0,
      xtol_rel    = 1.0e-6,
      xtol_abs    = 1.0e-6,
      ftol_rel    = 1.0e-6,
      ftol_abs    = 1.0e-6,
      maxeval     = 2000,
      algorithm   = "NLOPT_LD_LBFGS"
    )
  )
  startVals1 <- getStartVals(modelInputs, i = 1)
  startVals2 <- getStartVals(modelInputs, i = 2)
  expect_identical(startVals1, c(price = 0, feat = 0))
  expect_false(identical(startVals2, c(price = 0, feat = 0)))
  expect_equal(length(startVals1), 2)
  expect_equal(length(startVals2), 2)
})

test_that("getStartVals returns user-provided starting parameters for first iteration in a multistart loop (and not other iterations)", {
  modelInputs <- getModelInputs(
    data = yogurt,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat"),
    scalePar = NULL,
    randPars = NULL,
    randScale = NULL,
    weights = NULL,
    panelID = NULL,
    clusterID = NULL,
    robust = FALSE,
    startValBounds = c(-1, 1),
    startVals = c(1, 1),
    numMultiStarts = 1,
    useAnalyticGrad = TRUE,
    scaleInputs = TRUE,
    standardDraws = NULL,
    drawType = 'halton',
    numDraws = 50,
    numCores = NULL,
    vcov = FALSE,
    predict = TRUE,
    correlation = FALSE,
    call = NULL,
    options         = list(
      print_level = 0,
      xtol_rel    = 1.0e-6,
      xtol_abs    = 1.0e-6,
      ftol_rel    = 1.0e-6,
      ftol_abs    = 1.0e-6,
      maxeval     = 2000,
      algorithm   = "NLOPT_LD_LBFGS"
    )
  )
  startVals1 <- getStartVals(modelInputs, i = 1)
  startVals2 <- getStartVals(modelInputs, i = 2)
  expect_identical(startVals1, c(price = 1, feat = 1))
  expect_false(identical(startVals2, c(price = 0, feat = 0)))
  expect_equal(length(startVals1), 2)
  expect_equal(length(startVals2), 2)
})
