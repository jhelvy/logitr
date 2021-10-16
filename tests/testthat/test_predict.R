context("predict method")
library(logitr)

model <- logitr(
    data   = yogurt,
    choice = 'choice',
    obsID  = 'obsID',
    pars   = c('price', 'feat', 'brand')
)

data <- subset(
  yogurt, obsID %in% c(42, 13),
  select = c('obsID', 'alt', 'price', 'feat', 'brand'))

test_that("predict() uses model data if newdata == NULL", {
  p <- predict(model)
  expect_equal(nrow(p), nrow(yogurt))
})

test_that("predict() uses newdata if provided", {
  p <- predict(model, newdata = data, obsID = "obsID")
  expect_equal(nrow(p), nrow(data))
})

test_that("predict() returns the correct user-specified prediction types", {
  x <- predict(model, newdata = data, obsID = "obsID")
  expect_true(
    (! "choice_predict" %in% names(x)) &
    ("prob_predict" %in% names(x))
  )
  x <- predict(model, newdata = data, obsID = "obsID", type = "probs")
  expect_true(
    (! "choice_predict" %in% names(x)) &
    ("prob_predict" %in% names(x))
  )
  x <- predict(model, newdata = data, obsID = "obsID", type = "choices")
  expect_true(
    ("choice_predict" %in% names(x)) &
    (! "prob_predict" %in% names(x))
  )
  x <- predict(
    model, newdata = data, obsID = "obsID",
    type = c("choices", "probs")
  )
  expect_true(
    ("choice_predict" %in% names(x)) &
    ("prob_predict" %in% names(x))
  )
  x <- predict(
    model, newdata = data, obsID = "obsID",
    type = c("probs", "choices")
  )
  expect_true(
    ("choice_predict" %in% names(x)) &
    ("prob_predict" %in% names(x))
  )
})
