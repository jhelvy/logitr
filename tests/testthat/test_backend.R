context("Computational backend")
library(logitr)

# The `backend` argument is an extension point for future faster backends. For
# now only "cpu" (the default, logitr's native R implementation) is supported.
# These tests lock in that (1) the default and explicit "cpu" are identical,
# (2) the choice is recorded on the model, (3) reserved/unknown backends error
# informatively, and (4) models saved before `backend` existed still work.

test_that('default backend is "cpu" and is recorded on the model', {
  model <- logitr(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand")
  )
  expect_equal(model$inputs$backend, "cpu")
})

test_that('default and explicit backend = "cpu" give identical MNL results', {
  args <- list(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand"), vcov = TRUE
  )
  m_def <- do.call(logitr, args)
  m_cpu <- do.call(logitr, c(args, backend = "cpu"))
  expect_equal(logLik(m_def), logLik(m_cpu))
  expect_equal(coef(m_def), coef(m_cpu))
  expect_equal(m_def$vcov, m_cpu$vcov)
})

test_that('default and explicit backend = "cpu" give identical MXL results', {
  args <- list(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 30, numMultiStarts = 1, numCores = 1, vcov = TRUE
  )
  m_def <- do.call(logitr, args)
  m_cpu <- do.call(logitr, c(args, backend = "cpu"))
  expect_equal(logLik(m_def), logLik(m_cpu))
  expect_equal(coef(m_def), coef(m_cpu))
})

test_that("reserved-but-unavailable backends error informatively", {
  for (b in c("rust", "torch", "xlogit")) {
    expect_error(
      logitr(
        data = yogurt, outcome = "choice", obsID = "obsID",
        pars = c("price", "feat"), backend = b
      ),
      "not yet available"
    )
  }
})

test_that("unknown backends error informatively", {
  expect_error(
    logitr(
      data = yogurt, outcome = "choice", obsID = "obsID",
      pars = c("price", "feat"), backend = "bogus"
    ),
    "Unknown backend"
  )
})

test_that("checkBackend validates type and length", {
  expect_error(checkBackend(NULL), "single character string")
  expect_error(checkBackend(c("cpu", "cpu")), "single character string")
  expect_error(checkBackend(1), "single character string")
  expect_equal(checkBackend("cpu"), "cpu")
})

test_that("vcov() works on legacy models without a backend field", {
  model <- logitr(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat")
  )
  # Simulate a model estimated before the `backend` argument existed
  model$inputs$backend <- NULL
  v <- vcov(model)
  expect_true(is.matrix(v))
  expect_true(all(is.finite(diag(v))))
})
