context("Starting parameters")
library(logitr)

test_that("getStartPars returns all 0s for first iteration in a multistart loop (and not other iterations)", {
  modelInputs <- getModelInputs(
    data = yogurt,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat"),
    price = NULL,
    randPars = NULL,
    randPrice = NULL,
    modelSpace = "pref",
    weights = NULL,
    panelID = NULL,
    clusterID = NULL,
    robust = FALSE,
    startParBounds = c(-1, 1),
    startVals = NULL,
    numMultiStarts  = 1,
    useAnalyticGrad = TRUE,
    scaleInputs = TRUE,
    standardDraws = NULL,
    numDraws = 50,
    numCores = NULL,
    vcov = FALSE,
    predict = TRUE,
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
  startPars1 <- getStartPars(modelInputs, i = 1)
  startPars2 <- getStartPars(modelInputs, i = 2)
  expect_identical(startPars1, c(price = 0, feat = 0))
  expect_false(identical(startPars2, c(price = 0, feat = 0)))
  expect_equal(length(startPars1), 2)
  expect_equal(length(startPars2), 2)
})

test_that("getStartPars returns user-provided starting parameters for first iteration in a multistart loop (and not other iterations)", {
  modelInputs <- getModelInputs(
    data = yogurt,
    outcome = "choice",
    obsID = "obsID",
    pars = c("price", "feat"),
    price = NULL,
    randPars = NULL,
    randPrice = NULL,
    modelSpace = "pref",
    weights = NULL,
    panelID = NULL,
    clusterID = NULL,
    robust = FALSE,
    startParBounds = c(-1, 1),
    startVals = c(1, 1),
    numMultiStarts = 1,
    useAnalyticGrad = TRUE,
    scaleInputs = TRUE,
    standardDraws = NULL,
    numDraws = 50,
    numCores = NULL,
    vcov = FALSE,
    predict = TRUE,
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
  startPars1 <- getStartPars(modelInputs, i = 1)
  startPars2 <- getStartPars(modelInputs, i = 2)
  expect_identical(startPars1, c(price = 1, feat = 1))
  expect_false(identical(startPars2, c(price = 0, feat = 0)))
  expect_equal(length(startPars1), 2)
  expect_equal(length(startPars2), 2)
})
