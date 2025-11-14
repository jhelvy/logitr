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
  'logitr()\` function).

- ...:

  further arguments.

- x:

  is an object of class `logitr`.

- digits:

  the number of digits for printing, defaults to `3`.

- width:

  the width of the printing.
