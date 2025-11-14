# Validate data formatting for logitr models

This function checks that data is properly formatted for use with
logitr() and provides detailed diagnostic information about potential
issues.

## Usage

``` r
validate_data(
  data,
  outcome,
  obsID,
  pars = NULL,
  scalePar = NULL,
  panelID = NULL
)
```

## Arguments

- data:

  The data frame to validate

- outcome:

  The name of the column that identifies the outcome variable

- obsID:

  The name of the column that identifies each observation

- pars:

  Optional. The names of parameters to check (for additional validation)

- scalePar:

  Optional. The name of the scale parameter column (for WTP models)

- panelID:

  Optional. The name of the panel ID column (for panel data)

## Value

An object of class 'logitr_validation' containing validation results

## Examples

``` r
library(logitr)

# Validate the yogurt dataset
validate_data(yogurt, outcome = "choice", obsID = "obsID")
#> === LOGITR DATA VALIDATION ===
#> 
#> Data Overview:
#>   Rows: 9648 
#>   Columns: 7 
#>   Outcome variable: choice 
#>   Observation ID: obsID 
#> 
#> Data Structure:
#>   Total observations: 2412 
#>   Total alternatives: 9648 
#>   Valid choices: 2412 
#>   Alternatives per observation:
#>      4 alternatives: 2412 observations
#> 
#> === VALIDATION RESULTS ===
#> ✓ Data validation PASSED - no issues found!
#> ✓ Data appears ready for use with logitr()!

# Validate with parameters specified
validate_data(yogurt, outcome = "choice", obsID = "obsID",
              pars = c("price", "feat", "brand"))
#> === LOGITR DATA VALIDATION ===
#> 
#> Data Overview:
#>   Rows: 9648 
#>   Columns: 7 
#>   Outcome variable: choice 
#>   Observation ID: obsID 
#>   Parameters: price, feat, brand 
#> 
#> Data Structure:
#>   Total observations: 2412 
#>   Total alternatives: 9648 
#>   Valid choices: 2412 
#>   Alternatives per observation:
#>      4 alternatives: 2412 observations
#> 
#> Parameter Information:
#>    price ( numeric ): range [ 0.3 , 19.3 ]
#>    feat ( numeric ): range [ 0 , 1 ]
#>    brand ( character ):  4 levels - dannon, hiland, weight, yoplait
#> 
#> === VALIDATION RESULTS ===
#> ✓ Data validation PASSED - no issues found!
#> ✓ Data appears ready for use with logitr()!
```
