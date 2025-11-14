# Simulated SP dataset of mode choice (from the apollo package).

A simulated dataset containing 7,000 mode choices among four
alternatives. Data comes from 500 individuals, each with 14 stated
stated preference (SP) observations. There are 7,000 choices in total.
Each observation contains attributes for the alternatives, availability
of alternatives, and characteristics of the individuals.

## Usage

``` r
data(apolloModeChoiceData)
```

## Format

|                     |                                                                        |
|---------------------|------------------------------------------------------------------------|
| Variable            | Description                                                            |
| `ID`                | individual identifiers                                                 |
| `obsID`             | identifier for unique choice observation                               |
| `altID`             | alternative in each choice observation                                 |
| `qID`               | Numeric. Consecutive ID of SP choice tasks.                            |
| `choice`            | dummy code for choice (1 or 0)                                         |
| `mode`              | Character describing mode: "air", "rail", "car", "bus"                 |
| `time`              | Travel time in minutes.                                                |
| `cost`              | cost (in GBP) of trip.                                                 |
| `access`            | Access time in minutes.                                                |
| `service`           | Numeric. Additional services: 1 for no-frills, 2 for wifi, 3 for food. |
| `mode_air`          | Dummy coefficient for "air" mode.                                      |
| `mode_bus`          | Dummy coefficient for "bus" mode.                                      |
| `mode_car`          | Dummy coefficient for "car" mode.                                      |
| `mode_rail`         | Dummy coefficient for "rail" mode.                                     |
| `service_no_frills` | Dummy coefficient for "no-frills" additional service.                  |
| `service_wifi`      | Dummy coefficient for "wifi" additional service.                       |
| `service_food`      | Dummy coefficient for "food" additional service.                       |
| `time_car`          | Travel time (in minutes) for car trip.                                 |
| `time_bus`          | Travel time (in minutes) for bus trip.                                 |
| `time_air`          | Travel time (in minutes) for air trip.                                 |
| `time_rail`         | Travel time (in minutes) for rail trip.                                |
| `female`            | Numeric. Sex of individual. 1 for female, 0 for male.                  |
| `business`          | Numeric. Purpose of the trip. 1 for business, 0 for other.             |
| `income`            | Numeric. Income (in GBP per annum) of the individual.                  |

## Source

Data imported from the apollo package
[archive](https://www.rdocumentation.org/packages/apollo/versions/0.2.6/topics/apollo_modeChoiceData)

## References

Hess, S. & Palma, D. (2019), Apollo: a flexible, powerful and
customisable freeware package for choice model estimation and
application, Journal of Choice Modelling, Volume 32, September 2019.
[doi:10.1016/j.jocm.2019.100170](https://doi.org/10.1016/j.jocm.2019.100170)

## Examples

``` r
data(apolloModeChoiceData)

head(apolloModeChoiceData)
#> # A tibble: 6 × 24
#>      ID obsID altID   qID choice mode   time  cost access service mode_air
#>   <int> <int> <int> <int>  <dbl> <chr> <int> <int>  <dbl>   <dbl>    <int>
#> 1     1     1     1     1      0 air      50    50     55       3        1
#> 2     1     1     2     1      1 rail    170    35      5       2        0
#> 3     1     2     1     2      0 air      90    65     45       1        1
#> 4     1     2     2     2      1 rail    120    75      5       3        0
#> 5     1     3     1     3      0 air      70   110     40       1        1
#> 6     1     3     2     3      1 rail    155    75     25       2        0
#> # ℹ 13 more variables: mode_bus <int>, mode_car <int>, mode_rail <int>,
#> #   service_no_frills <int>, service_wifi <int>, service_food <int>,
#> #   time_car <int>, time_bus <int>, time_air <int>, time_rail <int>,
#> #   female <int>, business <int>, income <int>
```
