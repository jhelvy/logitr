context("Compiled (cpp) backend")
library(logitr)

# The cpp backend routes the MXL log-likelihood + gradient through a compiled
# Rcpp kernel. It must reproduce the native R (cpu) path across all supported
# MXL model types (preference and WTP space, uncorrelated and correlated).

expect_cpp_matches <- function(..., tol = 1e-4) {
  args <- list(...)
  cpu <- suppressMessages(do.call(logitr, c(args, backend = "cpu")))
  cpp <- suppressMessages(do.call(logitr, c(args, backend = "cpp")))
  expect_equal(as.numeric(logLik(cpu)), as.numeric(logLik(cpp)), tolerance = tol)
  expect_equal(unname(coef(cpu)), unname(coef(cpp)), tolerance = tol)
}

# Deterministic kernel check: compare the objective + analytic gradient at a
# fixed parameter point. This is the right test for hard, non-convex models
# (e.g. correlated random-scale WTP) where a single-start end-to-end fit can
# land at different local optima even when the gradient is identical.
expect_cpp_grad_matches <- function(..., par_val = 0.3) {
  defaults <- list(
    data = yogurt, outcome = "choice", obsID = "obsID",
    randPars = NULL, scalePar = NULL, randScale = NULL, panelID = NULL,
    correlation = FALSE, weights = NULL, clusterID = NULL, robust = FALSE,
    startValBounds = c(-1, 1), startVals = NULL, numMultiStarts = 1,
    useAnalyticGrad = TRUE, scaleInputs = TRUE, standardDraws = NULL,
    drawType = "halton", numDraws = 50, numCores = 1, vcov = FALSE,
    predict = FALSE, call = NULL,
    options = list(print_level = 0, xtol_rel = 1e-6, xtol_abs = 1e-6,
                   ftol_rel = 1e-6, ftol_abs = 1e-6, maxeval = 1000,
                   algorithm = "NLOPT_LD_LBFGS"))
  args <- utils::modifyList(defaults, list(...))
  mi_cpu <- suppressMessages(do.call(getModelInputs, c(args, list(backend = "cpu"))))
  mi_cpp <- suppressMessages(do.call(getModelInputs, c(args, list(backend = "cpp"))))
  p <- stats::setNames(rep(par_val, length(mi_cpu$parNames$all)), mi_cpu$parNames$all)
  r1 <- mi_cpu$evalFuncs$objective(p, mi_cpu)
  r2 <- mi_cpp$evalFuncs$objective(p, mi_cpp)
  expect_equal(as.numeric(r1$objective), as.numeric(r2$objective), tolerance = 1e-8)
  expect_equal(as.numeric(r1$gradient), as.numeric(r2$gradient), tolerance = 1e-8)
}

test_that("cpp matches cpu: preference space, panel, normal", {
  skip_on_cran()
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 40, numCores = 1
  )
})

test_that("cpp matches cpu: non-panel, and fixed + random mix", {
  skip_on_cran()
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
  skip_on_cran()
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
  skip_on_cran()
  d <- yogurt
  # Weights must be constant within a panel (id); vary them by respondent
  d$wts <- ifelse(d$id %% 2 == 0, 1, 2)
  expect_cpp_matches(
    data = d, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    weights = "wts", numDraws = 40, numCores = 1
  )
})

test_that("cpp matches cpu: WTP space", {
  skip_on_cran()
  # Fixed scale, panel and non-panel
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("feat", "brand"), scalePar = "price", randPars = c(feat = "n"),
    numDraws = 40, numCores = 1
  )
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("feat", "brand"), scalePar = "price", randPars = c(feat = "n"),
    numDraws = 40, numCores = 1
  )
  # Log-normal WTP coefficient
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("feat", "brand"), scalePar = "price", randPars = c(feat = "ln"),
    numDraws = 40, numCores = 1
  )
})

test_that("cpp matches cpu: correlated heterogeneity", {
  skip_on_cran()
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n", price = "n"),
    correlation = TRUE, numDraws = 40, numCores = 1
  )
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n", price = "n"),
    correlation = TRUE, numDraws = 40, numCores = 1
  )
  # Correlated with a log-normal parameter
  expect_cpp_matches(
    data = yogurt, outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "ln", price = "n"),
    correlation = TRUE, numDraws = 40, numCores = 1
  )
})

test_that("cpp threaded results match serial (gradient parity)", {
  skip_on_cran()
  grad_at <- function(numThreads, ...) {
    defaults <- list(
      data = yogurt, outcome = "choice", obsID = "obsID",
      randPars = NULL, scalePar = NULL, randScale = NULL, panelID = NULL,
      correlation = FALSE, weights = NULL, clusterID = NULL, robust = FALSE,
      startValBounds = c(-1, 1), startVals = NULL, numMultiStarts = 1,
      useAnalyticGrad = TRUE, scaleInputs = TRUE, standardDraws = NULL,
      drawType = "halton", numDraws = 100, numCores = 1, vcov = FALSE,
      predict = FALSE, call = NULL, backend = "cpp", numThreads = numThreads,
      options = list(print_level = 0, xtol_rel = 1e-6, xtol_abs = 1e-6,
                     ftol_rel = 1e-6, ftol_abs = 1e-6, maxeval = 1000,
                     algorithm = "NLOPT_LD_LBFGS"))
    mi <- suppressMessages(do.call(getModelInputs, utils::modifyList(defaults, list(...))))
    p <- stats::setNames(rep(0.3, length(mi$parNames$all)), mi$parNames$all)
    mi$evalFuncs$objective(p, mi)
  }
  check <- function(...) {
    r1 <- grad_at(1, ...)
    r2 <- grad_at(2, ...)
    expect_equal(as.numeric(r1$objective), as.numeric(r2$objective), tolerance = 1e-8)
    expect_equal(as.numeric(r1$gradient), as.numeric(r2$gradient), tolerance = 1e-8)
  }
  check(panelID = "id", pars = c("price", "feat", "brand"),
        randPars = c(feat = "n", price = "n"), correlation = TRUE)
  check(panelID = "id", pars = c("feat", "brand"), scalePar = "price",
        randScale = "n", randPars = c(feat = "n", brand = "n"), correlation = TRUE)
})

test_that("censored-normal randScale gradients are correct (regression)", {
  skip_on_cran()
  # Regression: with randScale = "cn", draws where the scale (lambda) censors
  # to zero used to produce NaN gradients on the cpu path (0/0 in V/lambda,
  # contaminating the lambda mean and sd slots) and a lambda-mean partial
  # missing the censoring indicator on the cpp path (finite but wrong).
  # Validate every path against the numeric gradient of the log-likelihood.
  make_mi <- function(backend, ...) {
    defaults <- list(
      data = yogurt, outcome = "choice", obsID = "obsID",
      pars = c("feat", "brand"), scalePar = "price",
      randPars = c(feat = "n"), randScale = "cn",
      panelID = NULL, correlation = FALSE, weights = NULL, clusterID = NULL,
      robust = FALSE, startValBounds = c(-1, 1), startVals = NULL,
      numMultiStarts = 1, useAnalyticGrad = TRUE, scaleInputs = TRUE,
      standardDraws = NULL, drawType = "halton", numDraws = 50, numCores = 1,
      vcov = FALSE, predict = FALSE, call = NULL, backend = backend,
      numDrawsBatch = NULL,
      options = list(print_level = 0, xtol_rel = 1e-6, xtol_abs = 1e-6,
                     ftol_rel = 1e-6, ftol_abs = 1e-6, maxeval = 1000,
                     algorithm = "NLOPT_LD_LBFGS"))
    suppressMessages(do.call(
      getModelInputs, utils::modifyList(defaults, list(...))))
  }
  # At pars = 0.3 the scale censors to zero on a subset of the draws, which is
  # exactly the case that used to break
  mi0 <- make_mi("cpu")
  expect_true(any(0.3 + 0.3 * mi0$standardDraws[, 1] <= 0))

  check <- function(...) {
    mi_cpu <- make_mi("cpu", ...)
    p <- rep(0.3, mi_cpu$n$pars)
    g_num <- as.numeric(getNumericNegGradLL(p, mi_cpu))
    g_cpu <- as.numeric(mi_cpu$evalFuncs$negGradLL(p, mi_cpu))
    mi_cpp <- make_mi("cpp", ...)
    g_cpp <- as.numeric(mi_cpp$evalFuncs$negGradLL(p, mi_cpp))
    expect_true(all(is.finite(g_cpu)))
    expect_equal(g_cpu, g_num, tolerance = 1e-5)
    expect_equal(g_cpp, g_num, tolerance = 1e-5)
  }
  check()                # uncorrelated, non-panel
  check(panelID = "id")  # panel
  # Correlated block including lambda (exercises the lambda off-diagonals)
  check(panelID = "id", randPars = c(feat = "n", brand = "n"),
        correlation = TRUE)

  # Draw-batched streaming path (cpu backend)
  mi_strm <- make_mi("cpu", numDrawsBatch = 8)
  p <- rep(0.3, mi_strm$n$pars)
  expect_equal(
    as.numeric(mi_strm$evalFuncs$negGradLL(p, mi_strm)),
    as.numeric(getNumericNegGradLL(p, mi_strm)),
    tolerance = 1e-5
  )

  # Tight cpu/cpp parity at the same censoring point
  expect_cpp_grad_matches(
    pars = c("feat", "brand"), scalePar = "price",
    randPars = c(feat = "n"), randScale = "cn"
  )
})

test_that("cluster-robust SEs work with the cpp backend (regression)", {
  skip_on_cran()
  # Regression test: computing cluster-robust standard errors on a model
  # estimated with backend = "cpp" used to route the per-cluster gradients
  # through the C++ kernel, which reads/writes out of bounds on cluster
  # subsets (unindexed obsID/panelID, stale nObs) and segfaulted. The robust
  # path must always use the cpu eval functions.
  robust_se <- function(backend, ...) {
    m <- suppressMessages(logitr(
      data = yogurt, outcome = "choice", obsID = "obsID",
      pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
      numDraws = 30, numCores = 1, vcov = TRUE, backend = backend, ...
    ))
    sqrt(diag(m$vcov))
  }
  # Panel + clustered
  se_cpu <- robust_se("cpu", panelID = "id", clusterID = "id")
  se_cpp <- robust_se("cpp", panelID = "id", clusterID = "id")
  expect_true(all(is.finite(se_cpp)))
  expect_equal(unname(se_cpu), unname(se_cpp), tolerance = 1e-3)
  # Non-panel + robust (clusters on obsID via setupClusterID)
  se_cpu <- robust_se("cpu", robust = TRUE)
  se_cpp <- robust_se("cpp", robust = TRUE)
  expect_true(all(is.finite(se_cpp)))
  expect_equal(unname(se_cpu), unname(se_cpp), tolerance = 1e-3)
  # Clustering on a non-panel column (used to error in makeClusterID(), which
  # compared clusterID against a NULL panelID)
  se_cpu <- robust_se("cpu", clusterID = "id")
  se_cpp <- robust_se("cpp", clusterID = "id")
  expect_true(all(is.finite(se_cpp)))
  expect_equal(unname(se_cpu), unname(se_cpp), tolerance = 1e-3)
})

test_that("cpp matches cpu: correlated WTP space (gradient parity)", {
  skip_on_cran()
  # These models are non-convex, so compare the objective + gradient at a fixed
  # point rather than end-to-end fits (which can reach different local optima).
  expect_cpp_grad_matches(
    pars = c("feat", "brand"), scalePar = "price", panelID = "id",
    randPars = c(feat = "n", brand = "n"), correlation = TRUE
  )
  # Random scale in the correlated block
  expect_cpp_grad_matches(
    pars = c("feat", "brand"), scalePar = "price", panelID = "id",
    randScale = "n", randPars = c(feat = "n", brand = "n"), correlation = TRUE
  )
  # Random scale, non-panel
  expect_cpp_grad_matches(
    pars = c("feat", "brand"), scalePar = "price",
    randScale = "n", randPars = c(feat = "n", brand = "n"), correlation = TRUE
  )
})
