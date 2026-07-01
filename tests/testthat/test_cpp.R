context("Compiled (cpp) backend")
library(logitr)

# The cpp backend routes the MXL log-likelihood + gradient through a compiled
# Rcpp kernel. It must reproduce the native R (cpu) path to optimizer tolerance
# across the model types it supports, and error clearly on the ones it does not
# yet support (WTP space, correlated heterogeneity).

expect_cpp_matches <- function(..., tol = 1e-4) {
  args <- list(...)
  cpu <- suppressMessages(do.call(logitr, c(args, backend = "cpu")))
  cpp <- suppressMessages(do.call(logitr, c(args, backend = "cpp")))
  expect_equal(as.numeric(logLik(cpu)), as.numeric(logLik(cpp)), tolerance = tol)
  expect_equal(unname(coef(cpu)), unname(coef(cpp)), tolerance = tol)
}

test_that("cpp matches cpu: preference space, panel, normal", {
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 40, numCores = 1
  )
})

test_that("cpp matches cpu: non-panel, and fixed + random mix", {
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 40, numCores = 1
  )
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n", price = "n"),
    numDraws = 40, numCores = 1
  )
})

test_that("cpp matches cpu: log-normal and censored-normal", {
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "ln"),
    numDraws = 40, numCores = 1
  )
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "cn"),
    numDraws = 40, numCores = 1
  )
})

test_that("cpp matches cpu: weighted model", {
  d <- yogurt
  # Weights must be constant within a panel (id); vary them by respondent
  d$wts <- ifelse(d$id %% 2 == 0, 1, 2)
  expect_cpp_matches(
    data = d, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    weights = "wts", numDraws = 40, numCores = 1
  )
})

test_that("cpp backend errors on unsupported model features", {
  expect_error(
    logitr(yogurt, "choice", "obsID",
           pars = c("feat", "brand"), scalePar = "price",
           randPars = c(feat = "n"), backend = "cpp"),
    "WTP"
  )
  expect_error(
    logitr(yogurt, "choice", "obsID", panelID = "id",
           pars = c("price", "feat", "brand"),
           randPars = c(feat = "n", price = "n"), correlation = TRUE,
           backend = "cpp"),
    "correlated"
  )
})
