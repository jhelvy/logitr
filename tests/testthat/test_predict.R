context("predict method")
library(logitr)

model <- logitr(
    data   = yogurt,
    choice = 'choice',
    obsID  = 'obsID',
    pars   = c('price', 'feat', 'brand')
)

alts <- subset(
  yogurt, obsID %in% c(42, 13),
  select = c('obsID', 'alt', 'price', 'feat', 'brand'))

test_that("predict() uses model data if newdata == NULL", {
  p <- predict(model)
  expect_equal(nrow(p), nrow(yogurt))
})
