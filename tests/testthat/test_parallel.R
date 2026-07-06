context("Parallel core allocation")
library(logitr)

# When running a parallel multistart, the cores are spent running many models
# at once and each model is estimated single-threaded (this is faster than
# using threads to speed up individual models, per benchmarking). When there is
# a single model, the cores are used to thread the draw loop instead.

test_that("setNumThreads uses a single thread for a parallel multistart", {
  expect_equal(setNumThreads(NULL, numMultiStarts = 10), 1L)
  expect_equal(setNumThreads(NULL, numMultiStarts = 2), 1L)
})

test_that("setNumThreads uses threads for a single-start model", {
  n <- setNumThreads(NULL, numMultiStarts = 1)
  expect_true(is.integer(n) && n >= 1)
})

test_that("setNumThreads respects an explicit value", {
  expect_equal(setNumThreads(1, numMultiStarts = 1), 1L)
})

test_that("setNumThreads warns on invalid input and falls back to 1", {
  expect_warning(res <- setNumThreads(-1, numMultiStarts = 1))
  expect_equal(res, 1L)
})

test_that("the multistart header explains how the cores are used", {
  expect_message(
    printMultistartHeader(NULL, numMultiStarts = 10, numCores = 4, numThreads = 1),
    "multistart iterations in parallel")
  expect_message(
    printMultistartHeader(NULL, numMultiStarts = 10, numCores = 4, numThreads = 1),
    "single core")
})
