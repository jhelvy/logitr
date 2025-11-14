# Stated preference data for the choice of electricity suppliers (from mlogit package)

A sample of 2308 households in the United States.

## Usage

``` r
data(electricity)
```

## Format

|          |                                                                                                                                                                                                                                                                                                                                                                                            |
|----------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Variable | Description                                                                                                                                                                                                                                                                                                                                                                                |
| `id`     | individual identifiers                                                                                                                                                                                                                                                                                                                                                                     |
| `obsID`  | identifier for unique choice observation                                                                                                                                                                                                                                                                                                                                                   |
| `choice` | dummy code for choice (1 or 0)                                                                                                                                                                                                                                                                                                                                                             |
| `alt`    | alternative in each choice observation                                                                                                                                                                                                                                                                                                                                                     |
| `pf`     | fixed price at a stated cents per kWh, with the price varying over suppliers and experiments, for scenario i=(1, 2, 3, 4),                                                                                                                                                                                                                                                                 |
| `cl`     | the length of contract that the supplier offered, in years (such as 1 year or 5 years.) During this contract period, the supplier guaranteed the prices and the buyer would have to pay a penalty if he/she switched to another supplier. The supplier could offer no contract in which case either side could stop the agreement at any time. This is recorded as a contract length of 0. |
| `loc`    | is the supplier a local company.                                                                                                                                                                                                                                                                                                                                                           |
| `wk`     | is the supplier a well-known company.                                                                                                                                                                                                                                                                                                                                                      |
| `tod`    | a time-of-day rate under which the price is 11 cents per kWh from 8am to 8pm and 5 cents per kWh from 8pm to 8am. These TOD prices did not vary over suppliers or experiments: whenever the supplier was said to offer TOD, the prices were stated as above.                                                                                                                               |
| `seas`   | a seasonal rate under which the price is 10 cents per kWh in the summer, 8 cents per kWh in the winter, and 6 cents per kWh in the spring and fall. Like TOD rates, these prices did not vary. Note that the price is for the electricity only, not transmission and distribution, which is supplied by the local regulated utility.                                                       |

## Source

[Kenneth Train's home page](https://eml.berkeley.edu/~train/)

## References

Croissant, Y. (2020). Estimation of Random Utility Models in R: The
mlogit Package. Journal of Statistical Software, 95(11), 1–41.
[doi:10.18637/jss.v095.i11](https://doi.org/10.18637/jss.v095.i11)

## Examples

``` r
data(electricity)

head(electricity)
#>   id obsID choice alt pf cl loc wk tod seas
#> 1  1     1      0   1  7  5   0  1   0    0
#> 2  1     1      0   2  9  1   1  0   0    0
#> 3  1     1      0   3  0  0   0  0   0    1
#> 4  1     1      1   4  0  5   0  1   1    0
#> 5  1     2      0   1  7  0   0  1   0    0
#> 6  1     2      0   2  9  5   0  1   0    0
```
