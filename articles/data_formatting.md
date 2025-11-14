# Data Formatting and Encoding

## Basic required format

The {logitr} package requires that data be structured in a `data.frame`
and arranged in a “long” format \[@Wickham2014\] where each row contains
data on a single alternative from a choice observation. The choice
observations do not have to be symmetric, meaning they can have a
“ragged” structure where different choice observations have different
numbers of alternatives. The data must also include variables for each
of the following:

- **Outcome**: A dummy-coded variable that identifies which alternative
  was chosen (`1` is chosen, `0` is not chosen). Only one alternative
  should have a `1` per choice observation.
- **Observation ID**: A sequence of repeated numbers that identifies
  each unique choice observation. For example, if the first three choice
  observations had 2 alternatives each, then the first 6 rows of the
  `obsID` variable would be `1, 1, 2, 2, 3, 3`.
- **Covariates**: Other variables that will be used as model covariates.

The {logitr} package contains several example data sets that illustrate
this data structure. For example, the `yogurt` contains observations of
yogurt purchases by a panel of 100 households \[@Jain1994\]. Choice is
identified by the `choice` column, the observation ID is identified by
the `obsID` column, and the columns `price`, `feat`, and `brand` can be
used as model covariates:

``` r
library("logitr")

head(yogurt)
#>   id obsID alt choice price feat   brand
#> 1  1     1   1      0   8.1    0  dannon
#> 2  1     1   2      0   6.1    0  hiland
#> 3  1     1   3      1   7.9    0  weight
#> 4  1     1   4      0  10.8    0 yoplait
#> 5  1     2   1      1   9.8    0  dannon
#> 6  1     2   2      0   6.4    0  hiland
```

This data set also includes an `alt` variable that determines the
alternatives included in the choice set of each observation and an `id`
variable that determines the individual as the data have a panel
structure containing multiple choice observations from each individual.

## Continuous versus discrete variables

Variables are modeled as either continuous or discrete based on their
data *type*. Numeric variables are by default estimated with a single
“slope” coefficient. For example, consider a data frame that contains a
`price` variable with the levels \$10, \$15, and \$20. Adding `price` to
the `pars` argument in the main
[`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md)
function would result in a single `price` coefficient for the “slope” of
the change in price.

In contrast, categorical variables (i.e. `character` or `factor` type
variables) are by default estimated with a coefficient for all but the
first level, which serves as the reference level. The default reference
level is determined alphabetically, but it can also be set by modifying
the factor levels for that variable. For example, the default reference
level for the `brand` variable is `"dannon"` as it is alphabetically
first. To set `"weight"` as the reference level, the factor levels can
be modified using the [`factor()`](https://rdrr.io/r/base/factor.html)
function:

``` r
yogurt2 <- yogurt

brands <- c("weight", "hiland", "yoplait", "dannon")
yogurt2$brand <- factor(yogurt2$brand, levels = brands)
```

## Creating dummy coded variables

If you wish to make dummy-coded variables yourself to use them in a
model, I recommend using the `dummy_cols()` function from the
[{fastDummies}](https://github.com/jacobkap/fastDummies) package. For
example, in the code below, I create dummy-coded columns for the `brand`
variable and then use those variables as covariates in a model:

``` r
yogurt2 <- fastDummies::dummy_cols(yogurt2, "brand")
```

The `yogurt2` data frame now has new dummy-coded columns for brand:

``` r
head(yogurt2)
#> # A tibble: 6 × 11
#>      id obsID   alt choice price  feat brand   brand_weight brand_hiland
#>   <dbl> <int> <int>  <dbl> <dbl> <dbl> <fct>          <int>        <int>
#> 1     1     1     1      0  8.1      0 dannon             0            0
#> 2     1     1     2      0  6.10     0 hiland             0            1
#> 3     1     1     3      1  7.90     0 weight             1            0
#> 4     1     1     4      0 10.8      0 yoplait            0            0
#> 5     1     2     1      1  9.80     0 dannon             0            0
#> 6     1     2     2      0  6.40     0 hiland             0            1
#> # ℹ 2 more variables: brand_yoplait <int>, brand_dannon <int>
```

Now I can use those columns as covariates:

``` r
mnl_pref_dummies <- logitr(
  data    = yogurt2,
  outcome = 'choice',
  obsID   = 'obsID',
  pars    = c(
    'price', 'feat', 'brand_yoplait', 'brand_dannon', 'brand_weight'
  )
)

summary(mnl_pref_dummies)
#> =================================================
#> 
#> Model estimated on: Fri Nov 14 17:02:10 2025 
#> 
#> Using logitr version: 1.1.3 
#> 
#> Call:
#> logitr(data = yogurt2, outcome = "choice", obsID = "obsID", pars = c("price", 
#>     "feat", "brand_yoplait", "brand_dannon", "brand_weight"))
#> 
#> Frequencies of alternatives:
#>        1        2        3        4 
#> 0.402156 0.029436 0.229270 0.339138 
#> 
#> Exit Status: 3, Optimization stopped because ftol_rel or ftol_abs was reached.
#>                                 
#> Model Type:    Multinomial Logit
#> Model Space:          Preference
#> Model Run:                1 of 1
#> Iterations:                   18
#> Elapsed Time:        0h:0m:0.03s
#> Algorithm:        NLOPT_LD_LBFGS
#> Weights Used?:             FALSE
#> Robust?                    FALSE
#> 
#> Model Coefficients: 
#>                Estimate Std. Error z-value  Pr(>|z|)    
#> price         -0.366581   0.024366 -15.045 < 2.2e-16 ***
#> feat           0.491412   0.120063   4.093 4.259e-05 ***
#> brand_yoplait  4.450197   0.187118  23.783 < 2.2e-16 ***
#> brand_dannon   3.715575   0.145419  25.551 < 2.2e-16 ***
#> brand_weight   3.074399   0.145384  21.147 < 2.2e-16 ***
#> ---
#> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>                                      
#> Log-Likelihood:         -2656.8878788
#> Null Log-Likelihood:    -3343.7419990
#> AIC:                     5323.7757575
#> BIC:                     5352.7168000
#> McFadden R2:                0.2054148
#> Adj McFadden R2:            0.2039195
#> Number of Observations:  2412.0000000
```

## Validating data before estimation

Before estimating a model, it is often helpful to validate that your
data is properly formatted. The
[`validate_data()`](https://jhelvy.github.io/logitr/reference/validate_data.md)
function checks for common formatting errors and provides detailed
diagnostic information. This can save time by catching errors before you
attempt to estimate a model.

### Basic validation

At a minimum, you should validate the `outcome` and `obsID` columns:

``` r
validation <- validate_data(
  data = yogurt,
  outcome = "choice",
  obsID = "obsID"
)

validation
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
```

The function returns a validation object that indicates whether the data
is valid and provides summary information about the data structure.

### Validation with parameters

You can also validate specific parameters to check for missing values or
other issues:

``` r
validation <- validate_data(
  data = yogurt,
  outcome = "choice",
  obsID = "obsID",
  pars = c("price", "feat", "brand")
)

validation
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

### Panel data validation

For panel data, you can validate the panel structure:

``` r
validation <- validate_data(
  data = yogurt,
  outcome = "choice",
  obsID = "obsID",
  pars = c("price", "feat", "brand"),
  panelID = "id"
)

validation
#> === LOGITR DATA VALIDATION ===
#> 
#> Data Overview:
#>   Rows: 9648 
#>   Columns: 7 
#>   Outcome variable: choice 
#>   Observation ID: obsID 
#>   Parameters: price, feat, brand 
#>   Panel ID: id 
#> 
#> Data Structure:
#>   Total observations: 2412 
#>   Total alternatives: 9648 
#>   Valid choices: 2412 
#>   Alternatives per observation:
#>      4 alternatives: 2412 observations
#>   Panel structure:
#>     Individuals: 100 
#>     Observations per individual: Min = 4 , Max = 185 , Mean = 24.1 
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

### Common errors detected

The
[`validate_data()`](https://jhelvy.github.io/logitr/reference/validate_data.md)
function checks for several common formatting errors:

1.  **Multiple choices per observation**: Each `obsID` should have
    exactly one choice (outcome = 1)
2.  **No choice in observation**: Each `obsID` must have at least one
    choice
3.  **Non-contiguous observation blocks**: All rows with the same
    `obsID` must be grouped together
4.  **Invalid outcome values**: The outcome variable must only contain 0
    and 1 (or TRUE and FALSE)
5.  **Missing values**: Checks for missing values in required columns

Here’s an example of detecting an error:

``` r
# Create problematic data with multiple choices in one observation
bad_data <- yogurt
bad_data$choice[1:2] <- 1

validation <- validate_data(
  data = bad_data,
  outcome = "choice",
  obsID = "obsID"
)

validation
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
#>   Valid choices: 2411 
#>   Alternatives per observation:
#>      4 alternatives: 2412 observations
#> 
#> === VALIDATION RESULTS ===
#> ✗ ERRORS found:
#>    1 . Multiple choices (>1) found in obsID(s): 1  
#> 
#>   Detailed locations for multiple choices:
#>     ObsID 1 - rows: 1, 2, 3, 4 
#> 
#>  Please fix the errors above before using with logitr().
```
