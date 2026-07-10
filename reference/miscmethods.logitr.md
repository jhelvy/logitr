# Methods for logitr objects

Miscellaneous methods for `logitr` class objects.

## Usage

``` r
# S3 method for class 'logitr'
logLik(object, ...)

# S3 method for class 'logitr'
terms(x, ...)

# S3 method for class 'logitr'
coef(object, ...)

# S3 method for class 'summary.logitr'
coef(object, ...)

# S3 method for class 'logitr'
summary(object, ...)

# S3 method for class 'logitr'
print(
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
)

# S3 method for class 'summary.logitr'
print(
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
)

# S3 method for class 'logitr_wtp'
print(
  x,
  digits = max(3, getOption("digits") - 2),
  width = getOption("width"),
  ...
)

# S3 method for class 'logitr_validation'
print(x, ...)
```

## Arguments

- object:

  is an object of class `logitr` (a model estimated using the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function).

- ...:

  further arguments.

- x:

  is an object of class `logitr`.

- digits:

  the number of digits for printing, defaults to `3`.

- width:

  the width of the printing.

## Value

[`logLik()`](https://rdrr.io/r/stats/logLik.html) returns an object of
class `logLik` containing the log-likelihood of the model at the
estimated parameters.

[`terms()`](https://rdrr.io/r/stats/terms.html) returns the `terms`
object of the model formula.

[`coef()`](https://rdrr.io/r/stats/coef.html) returns a named numeric
vector of the estimated model coefficients, and
[`coef()`](https://rdrr.io/r/stats/coef.html) on a `summary.logitr`
object returns the coefficient table as a matrix with columns for the
estimate, standard error, z-value, and p-value.

[`summary()`](https://rdrr.io/r/base/summary.html) returns an object of
class `summary.logitr`.

[`model.frame()`](https://rdrr.io/r/stats/model.frame.html) returns a
data frame with the variables used to fit the model.

The [`print()`](https://rdrr.io/r/base/print.html) methods are called
for their side effect of printing to the console and invisibly return
their `x` argument.
