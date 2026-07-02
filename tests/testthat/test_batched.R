context("Draw-batched (streaming) MXL estimation")
library(logitr)

# The draw-batched path streams draws in chunks to keep memory bounded for
# large draw counts. It is a feature of the native R ("cpu") backend, so these
# tests pin backend = "cpu". It must reproduce the non-streaming stored-partials
# path (to floating-point reordering tolerance), while leaving the non-streaming
# path untouched for typical models.

# Fit the same MXL model with the fast path and with forced streaming, and
# compare coefficients / logLik / standard errors.
expect_stream_matches <- function(..., numDrawsBatch = 8, tol = 1e-5) {
  base_args <- c(list(...), list(backend = "cpu"))
  fast <- suppressMessages(do.call(logitr, base_args))
  strm <- suppressMessages(do.call(logitr, c(base_args, numDrawsBatch = numDrawsBatch)))
  expect_equal(as.numeric(logLik(fast)), as.numeric(logLik(strm)), tolerance = tol)
  expect_equal(unname(coef(fast)), unname(coef(strm)), tolerance = tol)
  if (!is.null(fast$se)) {
    expect_equal(unname(fast$se), unname(strm$se), tolerance = 1e-3)
  }
}

test_that("default (small model) does not stream", {
  m <- logitr(
    yogurt, "choice", "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 30, numCores = 1, backend = "cpu"
  )
  expect_false(m$batchPlan$stream)
})

test_that("forced streaming is recorded on the model", {
  m <- suppressMessages(logitr(
    yogurt, "choice", "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 30, numDrawsBatch = 8, numCores = 1, backend = "cpu"
  ))
  expect_true(m$batchPlan$stream)
  expect_equal(m$batchPlan$batchSize, 8L)
})

test_that("streaming matches the fast path: preference space, panel", {
  expect_stream_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 40, numCores = 1, vcov = TRUE
  )
})

test_that("streaming matches the fast path: log-normal and censored-normal", {
  expect_stream_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "ln"),
    numDraws = 40, numCores = 1
  )
  expect_stream_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "cn"),
    numDraws = 40, numCores = 1
  )
})

test_that("streaming matches the fast path: correlated heterogeneity", {
  expect_stream_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n", price = "n"),
    correlation = TRUE, numDraws = 40, numCores = 1
  )
})

test_that("streaming matches the fast path: WTP space", {
  expect_stream_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("feat", "brand"), scalePar = "price", randPars = c(feat = "n"),
    numDraws = 40, numCores = 1
  )
})

test_that("getBatchPlan streams only when needed", {
  # Small model: keep the fast (stored-partials) path
  expect_false(getBatchPlan(NULL, list(pars = 6, rowX = 7000, draws = 50))$stream)
  # Large model: auto-stream with a bounded batch size
  big <- getBatchPlan(NULL, list(pars = 16, rowX = 40000, draws = 5000))
  expect_true(big$stream)
  expect_true(big$batchSize >= 1 && big$batchSize < 5000)
  # User override forces the batch size
  usr <- getBatchPlan(100, list(pars = 16, rowX = 40000, draws = 5000))
  expect_true(usr$stream)
  expect_equal(usr$batchSize, 100L)
  # numDrawsBatch >= numDraws disables streaming
  expect_false(getBatchPlan(60, list(pars = 6, rowX = 7000, draws = 50))$stream)
})

test_that("a message is emitted when streaming engages", {
  expect_message(
    logitr(
      yogurt, "choice", "obsID", panelID = "id",
      pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
      numDraws = 30, numDrawsBatch = 8, numCores = 1, backend = "cpu"
    ),
    "draw-batched streaming"
  )
})

test_that("numDrawsBatch is validated", {
  expect_error(
    logitr(yogurt, "choice", "obsID", pars = c("price", "feat"),
           randPars = c(feat = "n"), numDrawsBatch = -1),
    "positive integer"
  )
  expect_error(
    logitr(yogurt, "choice", "obsID", pars = c("price", "feat"),
           randPars = c(feat = "n"), numDrawsBatch = c(5, 10)),
    "positive integer"
  )
})
