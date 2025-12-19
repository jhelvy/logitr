# Returns a list of the design matrix `X` and updated `pars` and `randPars` to include any dummy-coded categorical or interaction variables.

Recodes the data and returns a list of the encoded design matrix (`X`)
as well as two vectors (`pars` and `randPars`) with discrete
(categorical) variables and interaction variables added to `X`, `pars`,
and `randPars`.

## Usage

``` r
recodeData(data, pars, randPars)
```

## Arguments

- data:

  The data, formatted as a `data.frame` object.

- pars:

  The names of the parameters to be estimated in the model. Must be the
  same as the column names in the `data` argument. For WTP space models,
  do not include price in `pars` - it should instead be defined by the
  `scalePar` argument.

- randPars:

  A named vector whose names are the random parameters and values the
  distribution: `'n'` for normal or `'ln'` for log-normal. Defaults to
  `NULL`.

## Value

A list of the design matrix (`X`) and two vectors (`pars` and
`randPars`) with discrete (categorical) variables and interaction
variables added.

## Examples

``` r
library(logitr)

data(yogurt)

# Recode the yogurt data
result <- recodeData(
    data = yogurt,
    pars = c("price", "feat", "brand", "price*brand"),
    randPars = c(feat = "n", brand = "n")
)

result$formula
#> ~price + feat + brand + price * brand
#> <environment: 0x55a14cdaad38>
result$pars
#> [1] "price"              "feat"               "brandhiland"       
#> [4] "brandweight"        "brandyoplait"       "price:brandhiland" 
#> [7] "price:brandweight"  "price:brandyoplait"
result$randPars
#>         feat  brandhiland  brandweight brandyoplait 
#>          "n"          "n"          "n"          "n" 
head(result$X)
#>   price feat brandhiland brandweight brandyoplait price:brandhiland
#> 1   8.1    0           0           0            0               0.0
#> 2   6.1    0           1           0            0               6.1
#> 3   7.9    0           0           1            0               0.0
#> 4  10.8    0           0           0            1               0.0
#> 5   9.8    0           0           0            0               0.0
#> 6   6.4    0           1           0            0               6.4
#>   price:brandweight price:brandyoplait
#> 1               0.0                0.0
#> 2               0.0                0.0
#> 3               7.9                0.0
#> 4               0.0               10.8
#> 5               0.0                0.0
#> 6               0.0                0.0
```
