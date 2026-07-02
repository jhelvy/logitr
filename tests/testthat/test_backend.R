context("Computational backend")
library(logitr)

# The `backend` argument selects the computational backend. The default is
# "cpp" (compiled) for MXL models and "cpu" (native R) for MNL models (MNL
# always uses the R path). These tests lock in that (1) the default is recorded
# on the model, (2) MNL default matches "cpu" exactly, (3) MXL default ("cpp")
# matches "cpu" to floating-point tolerance, (4) reserved/unknown backends error
# informatively, and (5) models saved before `backend` existed still work.

test_that('the backend choice is recorded on the model (default "cpp")', {
  model <- logitr(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand")
  )
  expect_equal(model$inputs$backend, "cpp")
})

test_that('MNL uses the R path, so default and "cpu" are identical', {
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

test_that('MXL default ("cpp") matches "cpu" to tolerance', {
  args <- list(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 30, numMultiStarts = 1, numCores = 1
  )
  m_def <- suppressMessages(do.call(logitr, args))
  m_cpu <- suppressMessages(do.call(logitr, c(args, backend = "cpu")))
  expect_equal(as.numeric(logLik(m_def)), as.numeric(logLik(m_cpu)), tolerance = 1e-4)
  expect_equal(unname(coef(m_def)), unname(coef(m_cpu)), tolerance = 1e-4)
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
