context("Panel log-likelihood")
library(logitr)

# The simulated log-likelihood is computed differently for panel data: within
# each draw, the probabilities of all choice observations belonging to the same
# panel (individual) are multiplied together BEFORE averaging over draws, i.e.
#
#   non-panel:  LL = sum_obs   w * log( mean_r P_obs(r) )
#   panel:      LL = sum_panel w * log( mean_r prod_{obs in panel} P_obs(r) )
#
# The tests below validate both against an independent, from-scratch reference
# implementation (rather than just checking internal consistency), for every
# evaluation path: cpu, cpp, and the draw-batched streaming path.

make_mi <- function(backend, panelID = NULL, numDrawsBatch = NULL) {
  args <- list(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    scalePar = NULL, randScale = NULL, panelID = panelID,
    correlation = FALSE, weights = NULL, clusterID = NULL, robust = FALSE,
    startValBounds = c(-1, 1), startVals = NULL, numMultiStarts = 1,
    useAnalyticGrad = TRUE, scaleInputs = TRUE, standardDraws = NULL,
    drawType = "halton", numDraws = 25, numCores = 1, vcov = FALSE,
    predict = FALSE, call = NULL, backend = backend,
    numDrawsBatch = numDrawsBatch,
    options = list(print_level = 0, xtol_rel = 1e-6, xtol_abs = 1e-6,
                   ftol_rel = 1e-6, ftol_abs = 1e-6, maxeval = 1000,
                   algorithm = "NLOPT_LD_LBFGS"))
  suppressMessages(do.call(getModelInputs, args))
}

# Textbook simulated negative log-likelihood, written independently of the
# package internals: loop over draws, form the per-draw coefficients, compute
# the differenced-logit probability of each observation, multiply within panel
# (if panel), then average over draws and sum the weighted logs.
ref_negLL <- function(pars, mi) {
  d <- mi$data_diff
  draws <- mi$standardDraws
  nV <- mi$n$vars
  means <- pars[seq_len(nV)]
  sds <- numeric(nV)
  sds[mi$parIDs$r] <- pars[(nV + 1):mi$n$pars]
  nObs <- length(unique(d$obsID))
  probs <- matrix(0, nObs, nrow(draws))
  for (r in seq_len(nrow(draws))) {
    beta <- means + sds * draws[r, ]
    expV <- exp(as.vector(d$X %*% beta))
    sumExp <- rowsum(expV, d$obsID)
    probs[, r] <- 1 / (1 + sumExp)
  }
  if (!is.null(d$panelID)) {
    pHat <- rowMeans(exp(rowsum(log(probs), d$panelID)))
  } else {
    pHat <- rowMeans(probs)
  }
  -sum(d$weights * log(pHat))
}

test_that("panel MXL negLL matches an independent reference (all paths)", {
  mi_cpu <- make_mi("cpu", panelID = "id")
  pars <- rep(0.2, mi_cpu$n$pars)
  ref <- ref_negLL(pars, mi_cpu)
  expect_equal(mi_cpu$evalFuncs$negLL(pars, mi_cpu), ref, tolerance = 1e-10)

  mi_cpp <- make_mi("cpp", panelID = "id")
  expect_equal(mi_cpp$evalFuncs$negLL(pars, mi_cpp), ref, tolerance = 1e-8)

  mi_strm <- make_mi("cpu", panelID = "id", numDrawsBatch = 8)
  expect_true(mi_strm$batchPlan$stream)
  expect_equal(mi_strm$evalFuncs$negLL(pars, mi_strm), ref, tolerance = 1e-10)
})

test_that("non-panel MXL negLL matches an independent reference (all paths)", {
  mi_cpu <- make_mi("cpu")
  pars <- rep(0.2, mi_cpu$n$pars)
  ref <- ref_negLL(pars, mi_cpu)
  expect_equal(mi_cpu$evalFuncs$negLL(pars, mi_cpu), ref, tolerance = 1e-10)

  mi_cpp <- make_mi("cpp")
  expect_equal(mi_cpp$evalFuncs$negLL(pars, mi_cpp), ref, tolerance = 1e-8)

  mi_strm <- make_mi("cpu", numDrawsBatch = 8)
  expect_true(mi_strm$batchPlan$stream)
  expect_equal(mi_strm$evalFuncs$negLL(pars, mi_strm), ref, tolerance = 1e-10)
})

test_that("panel and non-panel MXL are genuinely different objectives", {
  mi_panel <- make_mi("cpu", panelID = "id")
  mi_nopanel <- make_mi("cpu")
  pars <- rep(0.2, mi_panel$n$pars)
  expect_false(isTRUE(all.equal(
    mi_panel$evalFuncs$negLL(pars, mi_panel),
    mi_nopanel$evalFuncs$negLL(pars, mi_nopanel)
  )))
})

test_that("MNL log-likelihood is unaffected by panelID", {
  # For MNL there is no simulation, so observations are independent and the
  # panel structure must not change the log-likelihood.
  args <- list(
    data = yogurt, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand")
  )
  m1 <- do.call(logitr, args)
  m2 <- do.call(logitr, c(args, panelID = "id"))
  expect_equal(as.numeric(logLik(m1)), as.numeric(logLik(m2)), tolerance = 1e-8)
  expect_equal(unname(coef(m1)), unname(coef(m2)), tolerance = 1e-6)
})
