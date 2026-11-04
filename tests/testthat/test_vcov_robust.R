context("Robust (clustered) covariance matrix")
library(logitr)

# The robust/clustered covariance matrix is a sandwich D %*% M %*% D, where M is
# built from the per-cluster score (gradient) contributions. Those contributions
# are computed by re-evaluating the gradient on one cluster's data at a time,
# which means the per-cluster data can be degenerate in ways the full data never
# is: a respondent who answered a single choice question contributes a single row
# to the differenced data, and a model with one parameter has a single column.
# Both used to drop to a vector and error (issue #58).

# Builds an unbalanced panel of binary choice questions in which some
# respondents answered exactly one question. Two alternatives per question is
# what makes the singleton case degenerate: it leaves a single row in the
# differenced data (with four alternatives, as in the full yogurt data, even a
# singleton respondent still has three differenced rows).
make_unbalanced <- function(n_singletons = 3) {
  binary <- do.call(rbind, lapply(unique(yogurt$obsID), function(o) {
    alts <- yogurt[yogurt$obsID == o, ]
    alts[c(which(alts$choice == 1)[1], which(alts$choice == 0)[1]), ]
  }))
  ids <- unique(binary$id)
  singles <- ids[seq_len(n_singletons)]
  do.call(rbind, lapply(ids, function(i) {
    obs <- unique(binary$obsID[binary$id == i])
    if (i %in% singles) obs <- obs[1]
    binary[binary$obsID %in% obs, ]
  }))
}

fit <- function(data, ...) {
  set.seed(1234)
  suppressMessages(logitr(
    data = data, outcome = "choice", obsID = "obsID",
    pars = c("price", "feat"), randPars = c(feat = "n"),
    panelID = "id", numMultiStarts = 1, numCores = 1, numDraws = 25, ...
  ))
}

test_that("unbalanced panel with singleton respondents has a finite clustered vcov", {
  df <- make_unbalanced()
  expect_true(any(table(unique(df[c("id", "obsID")])$id) == 1))
  expect_true(all(table(df$obsID) == 2)) # binary choices
  model <- fit(df, clusterID = "id")
  v <- vcov(model)
  expect_true(is.matrix(v))
  expect_equal(dim(v), rep(model$n$pars, 2))
  expect_true(all(is.finite(v)))
  expect_error(summary(model), NA)
})

test_that("weights alone also give a finite vcov on an unbalanced panel", {
  df <- make_unbalanced()
  df$w <- ifelse(df$id %% 2 == 0, 0.8, 1.2)
  model <- fit(df, weights = "w")
  expect_true(model$inputs$robust)
  expect_true(all(is.finite(vcov(model))))
})

test_that("a single-parameter mixed logit has a finite clustered vcov", {
  set.seed(1234)
  model <- suppressMessages(logitr(
    data = yogurt, outcome = "choice", obsID = "obsID", pars = "price",
    randPars = c(price = "n"), panelID = "id", clusterID = "id",
    numMultiStarts = 1, numCores = 1, numDraws = 25
  ))
  expect_true(all(is.finite(vcov(model))))
})

test_that("per-cluster scores sum to the full gradient", {
  # The sandwich is only correct if the per-cluster gradient contributions add
  # up to the gradient of the full log-likelihood. This checks the quantity the
  # fix actually feeds into M, not just that no error is thrown.
  df <- make_unbalanced()
  model <- fit(df, clusterID = "id")
  mi <- list(
    logitFuncs = logitr:::setLogitFunctions(model$modelSpace),
    evalFuncs = logitr:::setEvalFunctions(
      model$modelType, model$inputs$useAnalyticGrad, "cpu"),
    inputs = model$inputs, modelType = model$modelType,
    modelSpace = model$modelSpace, n = model$n, parSetup = model$parSetup,
    parIDs = model$parIDs, panel = !is.null(model$inputs$panelID),
    standardDraws = model$standardDraws,
    data_diff = logitr:::makeDiffData(model$data, model$modelType)
  )
  clusterID <- mi$data_diff$clusterID
  pars <- coef(model)
  scores <- t(vapply(seq_len(model$n$clusters), function(i) {
    tempMI <- logitr:::getClusterModelInputs(which(clusterID == i), mi, i)
    as.vector(-1 * mi$evalFuncs$negGradLL(pars, tempMI))
  }, numeric(length(pars))))
  # model$gradient is the gradient of the log-likelihood (not the negative)
  expect_equal(colSums(scores), as.vector(model$gradient),
               tolerance = 1e-6, ignore_attr = TRUE)
})
