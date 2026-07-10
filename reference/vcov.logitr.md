# Calculate the variance-covariance matrix

Returns the variance-covariance matrix of the main parameters of a
fitted model object.

## Usage

``` r
# S3 method for class 'logitr'
vcov(object, ...)
```

## Arguments

- object:

  is an object of class `logitr` (a model estimated using the
  [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
  function).

- ...:

  further arguments.

## Value

A numeric matrix of the estimated variance-covariance matrix of the
model coefficients, with one row and one column per coefficient.
