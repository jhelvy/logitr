# Choice observations of yogurt purchases by 100 households

Data from Jain et al. (1994) containing 2,412 choice observations from a
series of yogurt purchases by a panel of 100 households in Springfield,
Missouri, over a roughly two-year period. The data were collected by
optical scanners and contain information about the price, brand, and a
"feature" variable, which identifies whether a newspaper advertisement
was shown to the customer. There are four brands of yogurt: Yoplait,
Dannon, Weight Watchers, and Hiland, with market shares of 34%, 40%, 23%
and 3%, respectively.

## Usage

``` r
data(yogurt)
```

## Format

|          |                                                                                       |
|----------|---------------------------------------------------------------------------------------|
| Variable | Description                                                                           |
| `id`     | individual identifiers                                                                |
| `obsID`  | identifier for unique choice observation                                              |
| `alt`    | alternative in each choice observation                                                |
| `choice` | dummy code for choice (1 or 0)                                                        |
| `price`  | price of yogurt                                                                       |
| `feat`   | dummy for whether a newspaper advertisement was shown to the customer (`1` or `0`)    |
| `brand`  | yogurt brand: `"yoplait"`, `"dannon"`, `"hiland"`, or `"weight"` (for weight watcher) |

## Source

Raw data downloaded from the package mlogit v0.3-0 by Yves Croissant
[archive](https://www.rdocumentation.org/packages/mlogit/versions/0.3-0/topics/Yogurt)

## References

Dipak C. Jain, Naufel J. Vilcassim & Pradeep K. Chintagunta (1994) A
Random-Coefficients Logit Brand-Choice Model Applied to Panel Data,
Journal of Business & Economic Statistics, 12:3, 317-328,
[doi:10.1080/07350015.1994.10524547](https://doi.org/10.1080/07350015.1994.10524547)

## Examples

``` r
data(yogurt)

head(yogurt)
#> # A tibble: 6 × 7
#>      id obsID   alt choice price  feat brand  
#>   <dbl> <int> <int>  <dbl> <dbl> <dbl> <chr>  
#> 1     1     1     1      0  8.1      0 dannon 
#> 2     1     1     2      0  6.10     0 hiland 
#> 3     1     1     3      1  7.90     0 weight 
#> 4     1     1     4      0 10.8      0 yoplait
#> 5     1     2     1      1  9.80     0 dannon 
#> 6     1     2     2      0  6.40     0 hiland 
```
