# Data frame of run times for logitr benchmark

This data frame contains the run times for a benchmark comparing the
relative computation time to estimate a preference space mixed logit
model using the following R packages: logitr, mixl, mlogit, gmnl, and
apollo. The run times are exported from the Google colab notebook here:
https://colab.research.google.com/drive/1vYlBdJd4xCV43UwJ33XXpO3Ys8xWkuxx?usp=sharing

## Usage

``` r
data(runtimes)
```

## Format

|            |                                                    |
|------------|----------------------------------------------------|
| Variable   | Description                                        |
| `package`  | Package name.                                      |
| `time_sec` | The estimation time in seconds.                    |
| `numDraws` | The number of random draws used during estimation. |

## Source

[This](https://colab.research.google.com/drive/1vYlBdJd4xCV43UwJ33XXpO3Ys8xWkuxx?usp=sharing)
Google colab notebook

## Examples

``` r
data(runtimes)

head(runtimes)
#> # A tibble: 6 × 3
#>   package         time_sec numDraws
#>   <chr>              <dbl>    <dbl>
#> 1 logitr              2.75       50
#> 2 mixl (1 core)      10.6        50
#> 3 mixl (2 cores)      8.93       50
#> 4 mlogit             11.9        50
#> 5 gmnl               10.6        50
#> 6 apollo (1 core)    17.3        50
```
