context("Starting parameters")
library(logitr)

test_that("getStartPars returns all 0s for first iteration in a multistart loop (and not other iterations)", {
  modelInputs <- getModelInputs(
    data = yogurt,
    choiceName = "choice",
    obsIDName = "obsID",
    parNames = c("price", "feat"),
    priceName = NULL,
    randPars = NULL,
    randPrice = NULL,
    modelSpace = "pref",
    weightsName = NULL,
    clusterName = NULL,
    robust = FALSE,
    call = NULL,
    options = list()
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
    choiceName = "choice",
    obsIDName = "obsID",
    parNames = c("price", "feat"),
    priceName = NULL,
    randPars = NULL,
    randPrice = NULL,
    modelSpace = "pref",
    weightsName = NULL,
    clusterName = NULL,
    robust = FALSE,
    call = NULL,
    options = list(startVals = c(1, 1))
  )
  startPars1 <- getStartPars(modelInputs, i = 1)
  startPars2 <- getStartPars(modelInputs, i = 2)
  expect_identical(startPars1, c(price = 1, feat = 1))
  expect_false(identical(startPars2, c(price = 0, feat = 0)))
  expect_equal(length(startPars1), 2)
  expect_equal(length(startPars2), 2)
})
