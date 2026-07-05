context("Standard deviation signs")
library(logitr)

# The sign of an sd parameter is not identified in an uncorrelated MXL model,
# so logitr reports the absolute value. Because the SIMULATED log-likelihood is
# not exactly symmetric in the sd sign (a finite draw set is not
# sign-symmetric), the flip is done as an exact relabeling: the corresponding
# standard draw columns (and stored partials) are negated along with the
# coefficient, so (mean, -s, draws) == (mean, +s, -draws) draw-for-draw.
# Correlated models (Cholesky parameters) are left untouched.

test_that("absSdCoefficients flips sds and relabels the draws", {
  coefs <- c(a = 0.5, b = -0.3, sd_a = -1.2, sd_b = 0.8)
  draws <- matrix(1:8, nrow = 4, ncol = 2)

  # Uncorrelated MXL: negative sd terms become positive, and the draw column
  # of each flipped sd (var 1 for sd_a) is negated; unflipped columns untouched
  mi_uncorr <- list(
    modelType = "mxl", inputs = list(correlation = FALSE),
    parIDs = list(sdDiag = 3:4, r = 1:2), standardDraws = draws)
  res <- absSdCoefficients(coefs, mi_uncorr, fail = FALSE)
  expect_equal(unname(res$coefficients), c(0.5, -0.3, 1.2, 0.8))
  expect_equal(res$mi$standardDraws[, 1], -draws[, 1])
  expect_equal(res$mi$standardDraws[, 2], draws[, 2])

  # All-positive sds: nothing changes
  coefs_pos <- c(a = 0.5, b = -0.3, sd_a = 1.2, sd_b = 0.8)
  res <- absSdCoefficients(coefs_pos, mi_uncorr, fail = FALSE)
  expect_equal(res$coefficients, coefs_pos)
  expect_equal(res$mi$standardDraws, draws)

  # Correlated MXL: left unchanged (signs jointly define the covariance)
  mi_corr <- list(
    modelType = "mxl", inputs = list(correlation = TRUE),
    parIDs = list(sdDiag = 3:4, r = 1:2), standardDraws = draws)
  res <- absSdCoefficients(coefs, mi_corr, fail = FALSE)
  expect_equal(res$coefficients, coefs)
  expect_equal(res$mi$standardDraws, draws)

  # MNL: unchanged
  mi_mnl <- list(
    modelType = "mnl", inputs = list(correlation = FALSE),
    parIDs = list(sdDiag = integer(0)))
  expect_equal(absSdCoefficients(coefs, mi_mnl, fail = FALSE)$coefficients,
               coefs)

  # Failed model: unchanged
  expect_equal(absSdCoefficients(coefs, mi_uncorr, fail = TRUE)$coefficients,
               coefs)
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

test_that("the sd flip is an exact relabeling of the converged model", {
  # Regression test: flipping only the sd coefficient (without relabeling the
  # draws) moves the reported point off the converged optimum, because the
  # simulated LL is not symmetric in the sd sign for a finite draw set. That
  # produced large gradients, an indefinite hessian, and NaN standard errors.
  # Start the sds negative so the converged solution needs flipping.
  m <- suppressMessages(logitr(
    yogurt, "choice", "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 50, numCores = 1,
    startVals = c(price = -0.4, feat = 0.6, brandhiland = -3,
                  brandweight = -0.6, brandyoplait = 0.7, sd_feat = -1)
  ))
  expect_true(all(coef(m)[m$parIDs$sdDiag] >= 0))

  # The reported logLik must be reproduced exactly by evaluating the LL at the
  # reported coefficients with the model's (relabeled) stored draws
  mi <- suppressMessages(getModelInputs(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    scalePar = NULL, randScale = NULL, panelID = "id",
    correlation = FALSE, weights = NULL, clusterID = NULL, robust = FALSE,
    startValBounds = c(-1, 1), startVals = NULL, numMultiStarts = 1,
    useAnalyticGrad = TRUE, scaleInputs = FALSE,
    standardDraws = m$standardDraws, drawType = "sobol", numDraws = 50,
    numCores = 1, vcov = FALSE, predict = FALSE, call = NULL,
    backend = "cpu",
    options = list(print_level = 0, xtol_rel = 1e-6, xtol_abs = 1e-6,
                   ftol_rel = 1e-6, ftol_abs = 1e-6, maxeval = 1000,
                   algorithm = "NLOPT_LD_LBFGS")
  ))
  expect_equal(
    -mi$evalFuncs$negLL(coef(m), mi),
    as.numeric(logLik(m)),
    tolerance = 1e-8
  )
  # And the reported gradient at the reported coefficients is ~0 (converged)
  expect_true(max(abs(m$gradient)) < 1)
})
