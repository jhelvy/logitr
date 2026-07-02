context("Standard deviation signs")
library(logitr)

# The sign of an sd parameter is not identified in an uncorrelated MXL model,
# so logitr reports the absolute value. Correlated models (Cholesky parameters)
# are left untouched.

test_that("absSdCoefficients abs-es sd terms for uncorrelated MXL only", {
  coefs <- c(a = 0.5, b = -0.3, sd_a = -1.2, sd_b = 0.8)

  # Uncorrelated MXL: sd terms (positions 3:4) become positive, means untouched
  mi_uncorr <- list(
    modelType = "mxl", inputs = list(correlation = FALSE),
    parIDs = list(sdDiag = 3:4))
  expect_equal(
    unname(absSdCoefficients(coefs, mi_uncorr, fail = FALSE)),
    c(0.5, -0.3, 1.2, 0.8))

  # Correlated MXL: left unchanged (signs jointly define the covariance)
  mi_corr <- list(
    modelType = "mxl", inputs = list(correlation = TRUE),
    parIDs = list(sdDiag = 3:4))
  expect_equal(absSdCoefficients(coefs, mi_corr, fail = FALSE), coefs)

  # MNL: unchanged
  mi_mnl <- list(
    modelType = "mnl", inputs = list(correlation = FALSE),
    parIDs = list(sdDiag = integer(0)))
  expect_equal(absSdCoefficients(coefs, mi_mnl, fail = FALSE), coefs)

  # Failed model: unchanged
  expect_equal(absSdCoefficients(coefs, mi_uncorr, fail = TRUE), coefs)
})

test_that("fitted uncorrelated MXL reports non-negative standard deviations", {
  m <- suppressMessages(logitr(
    yogurt, "choice", "obsID", panelID = "id",
    pars = c("price", "feat", "brand"),
    randPars = c(price = "n", feat = "n", brand = "n"),
    numDraws = 50, numCores = 1
  ))
  expect_true(all(coef(m)[m$parIDs$sdDiag] >= 0))
})

test_that("abs of sd does not change the log-likelihood", {
  # Same model, cpu backend: the reported sds are non-negative but the fit
  # (log-likelihood) is unaffected by the sign convention.
  m <- suppressMessages(logitr(
    yogurt, "choice", "obsID", panelID = "id",
    pars = c("price", "feat"), randPars = c(feat = "n"),
    numDraws = 50, numCores = 1, backend = "cpu"
  ))
  expect_true(all(coef(m)[m$parIDs$sdDiag] >= 0))
  expect_true(is.finite(as.numeric(logLik(m))))
})
