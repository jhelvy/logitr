#' Choice observations of yogurt purchases by 100 households
#'
#' Data from Jain et al. (1994) containing 2,412 choice observations from a
#' series of yogurt purchases by a panel of 100 households in Springfield,
#' Missouri, over a roughly two-year period. The data were collected by
#' optical scanners and contain information about the price, brand, and a
#' "feature" variable, which identifies whether a newspaper advertisement was
#' shown to the customer. There are four brands of yogurt: Yoplait, Dannon,
#' Weight Watchers, and Hiland, with market shares of 34%, 40%, 23% and 3%,
#' respectively.
#'
#' @format
#' Variable | Description
#' -------- | ---------------------------------------------
#' `id` | individual identifiers
#' `obsID` | identifier for unique choice observation
#' `alt` | alternative in each choice observation
#' `choice` | dummy code for choice (1 or 0)
#' `price` | price of yogurt
#' `feat` | dummy for whether a newspaper advertisement was shown to the customer (`1` or `0`)
#' `brand` | yogurt brand: `"yoplait"`, `"dannon"`, `"hiland"`, or `"weight"` (for weight watcher)
#'
#' @docType data
#'
#' @usage data(yogurt)
#'
#' @keywords datasets
#'
#' @references Dipak C. Jain, Naufel J. Vilcassim & Pradeep K. Chintagunta (1994) A Random-Coefficients Logit Brand-Choice Model Applied to Panel Data, Journal of Business & Economic Statistics, 12:3, 317-328,
#' \doi{10.1080/07350015.1994.10524547}
#'
#' @source Raw data downloaded from the package mlogit v0.3-0 by Yves
#' Croissant \href{https://www.rdocumentation.org/packages/mlogit/versions/0.3-0/topics/Yogurt}{archive}
#'
#' @examples
#' data(yogurt)
#'
#' head(yogurt)
"yogurt"

#' Stated car choice observations by US car buyers
#'
#' Data from Helveston et al. (2015) containing 448 stated choice observations
#' from Chinese car buyers and 384 stated choice observations from US car
#' buyers. Conjoint surveys were fielded in 2012 in four major Chinese cities
#' (Beijing, Shanghai, Shenzhen, and Chengdu), online in the US on Amazon
#' Mechanical Turk, and in person at the Pittsburgh Auto show. Participants
#' were asked to select a vehicle from a set of three alternatives. Each
#' participant answered 15 choice questions.
#'
#' @format
#' Variable | Description
#' -------- | ---------------------------------------------
#' `id`             | individual identifiers
#' `obsnum`         | identifier for unique choice observation
#' `choice`         | dummy code for choice (`1` or `0`)
#' `hev`            | dummy code for HEV vehicle type (`1` or `0`)
#' `phev10`         | dummy code for PHEV vehicle type w/10 mile electric driving range (`1` or `0`)
#' `phev20`         | dummy code for PHEV vehicle type w/20 mile electric driving range (`1` or `0`)
#' `phev40`         | dummy code for PHEV vehicle type w/40 mile electric driving range (`1` or `0`)
#' `bev75`          | dummy code for BEV vehicle type w/75 mile electric driving range (`1` or `0`)
#' `bev100`         | dummy code for BEV vehicle type w/100 mile electric driving range (`1` or `0`)
#' `bev150`         | dummy code for BEV vehicle type w/150 mile electric driving range (`1` or `0`)
#' `phevFastcharge` | dummy code for whether PHEV vehicle had fast charging capability (`1` or `0`)
#' `bevFastcharge`  | dummy code for whether BEV vehicle had fast charging capability (`1` or `0`)
#' `price`          | price of vehicle ($USD)
#' `opCost`         | operating cost of vehicle (US cents / mile)
#' `accelTime`      | 0-60 mph acceleration time (seconds)
#' `american`       | dummy code for whether American brand (`1` or `0`)
#' `japanese`       | dummy code for whether Japanese brand (`1` or `0`)
#' `chinese`        | dummy code for whether Chinese brand (`1` or `0`)
#' `skorean`        | dummy code for whether S. Korean brand (`1` or `0`)
#' `weights`        | weights for each individual computed so that the sample age and income demographics matched with those of the general car-buying population
#'
#' @docType data
#'
#' @usage
#' data(cars_us)
#'
#' @keywords datasets
#'
#' @references Helveston, J. P., Liu, Y., Feit, E. M., Fuchs, E. R. H.,
#' Klampfl, E., & Michalek, J. J. (2015). "Will Subsidies Drive Electric
#' Vehicle Adoption? Measuring Consumer Preferences in the U.S. and China."
#' Transportation Research Part A: Policy and Practice, 73, 96–112.
#' \doi{10.1016/j.tra.2015.01.002}
#'
#' @source Raw data downloaded from \href{https://github.com/jhelvy/tra2015}{this repo}
#'
#' @examples
#' data(cars_us)
#'
#' head(cars_us)
"cars_us"

#' Stated car choice observations by Chinese car buyers
#'
#' Data from Helveston et al. (2015) containing 448 stated choice observations
#' from Chinese car buyers and 384 stated choice observations from US car
#' buyers. Conjoint surveys were fielded in 2012 in four major Chinese cities
#' (Beijing, Shanghai, Shenzhen, and Chengdu), online in the US on Amazon
#' Mechanical Turk, and in person at the Pittsburgh Auto show. Participants
#' were asked to select a vehicle from a set of three alternatives. Each
#' participant answered 15 choice questions.
#'
#' @format
#' Variable | Description
#' -------- | ---------------------------------------------
#' `id`             | individual identifiers
#' `obsnum`         | identifier for unique choice observation
#' `choice`         | dummy code for choice (`1` or `0`)
#' `hev`            | dummy code for HEV vehicle type (`1` or `0`)
#' `phev10`         | dummy code for PHEV vehicle type w/10 mile electric driving range (`1` or `0`)
#' `phev20`         | dummy code for PHEV vehicle type w/20 mile electric driving range (`1` or `0`)
#' `phev40`         | dummy code for PHEV vehicle type w/40 mile electric driving range (`1` or `0`)
#' `bev75`          | dummy code for BEV vehicle type w/75 mile electric driving range (`1` or `0`)
#' `bev100`         | dummy code for BEV vehicle type w/100 mile electric driving range (`1` or `0`)
#' `bev150`         | dummy code for BEV vehicle type w/150 mile electric driving range (`1` or `0`)
#' `phevFastcharge` | dummy code for whether PHEV vehicle had fast charging capability (`1` or `0`)
#' `bevFastcharge`  | dummy code for whether BEV vehicle had fast charging capability (`1` or `0`)
#' `price`          | price of vehicle ($USD)
#' `opCost`         | operating cost of vehicle (US cents / mile)
#' `accelTime`      | 0-60 mph acceleration time (seconds)
#' `american`       | dummy code for whether American brand (`1` or `0`)
#' `japanese`       | dummy code for whether Japanese brand (`1` or `0`)
#' `chinese`        | dummy code for whether Chinese brand (`1` or `0`)
#' `skorean`        | dummy code for whether S. Korean brand (`1` or `0`)
#' `weights`        | weights for each individual computed so that the sample age and income demographics matched with those of the general car-buying population
#'
#' @docType data
#'
#' @usage
#' data(cars_china)
#'
#' @keywords datasets
#'
#' @references Helveston, J. P., Liu, Y., Feit, E. M., Fuchs, E. R. H.,
#' Klampfl, E., & Michalek, J. J. (2015). "Will Subsidies Drive Electric
#' Vehicle Adoption? Measuring Consumer Preferences in the U.S. and China."
#' Transportation Research Part A: Policy and Practice, 73, 96–112.
#' \doi{10.1016/j.tra.2015.01.002}
#'
#' @source Raw data downloaded from \href{https://github.com/jhelvy/tra2015}{this repo}
#'
#' @examples
#' data(cars_china)
#'
#' head(cars_china)
"cars_china"

#' Simulated SP dataset of mode choice (from the {apollo} package).
#'
#' A simulated dataset containing 7,000 mode choices among four alternatives.
#' Data comes from 500 individuals, each with 14 stated stated preference
#' (SP) observations. There are 7,000 choices in total.
#' Each observation contains attributes for the alternatives,
#' availability of alternatives, and characteristics of the
#' individuals.
#' @format
#' Variable | Description
#' -------- | ---------------------------------------------
#' `ID`     | individual identifiers
#' `obsID`  | identifier for unique choice observation
#' `altID`  | alternative in each choice observation
#' `qID`    | Numeric. Consecutive ID of SP choice tasks.
#' `choice` | dummy code for choice (1 or 0)
#' `mode`   | Character describing mode: "air", "rail", "car", "bus"
#' `time`   | Travel time in minutes.
#' `cost`   | cost (in GBP) of trip.
#' `access` | Access time in minutes.
#' `service` | Numeric. Additional services: 1 for no-frills, 2 for wifi, 3 for food.
#' `mode_air` | Dummy coefficient for "air" mode.
#' `mode_bus` | Dummy coefficient for "bus" mode.
#' `mode_car` | Dummy coefficient for "car" mode.
#' `mode_rail` | Dummy coefficient for "rail" mode.
#' `service_no_frills` | Dummy coefficient for "no-frills" additional service.
#' `service_wifi` | Dummy coefficient for "wifi" additional service.
#' `service_food` | Dummy coefficient for "food" additional service.
#' `time_car` | Travel time (in minutes) for car trip.
#' `time_bus` | Travel time (in minutes) for bus trip.
#' `time_air` | Travel time (in minutes) for air trip.
#' `time_rail` | Travel time (in minutes) for rail trip.
#' `female` | Numeric. Sex of individual. 1 for female, 0 for male.
#' `business` | Numeric. Purpose of the trip. 1 for business, 0 for other.
#' `income` | Numeric. Income (in GBP per annum) of the individual.
#'
#' @docType data
#'
#' @usage data(apolloModeChoiceData)
#'
#' @keywords datasets
#'
#' @references Hess, S. & Palma, D. (2019), Apollo: a flexible, powerful and customisable freeware package for choice model estimation and application, Journal of Choice Modelling, Volume 32, September 2019.
#' \doi{10.1016/j.jocm.2019.100170}
#'
#' @source Data imported from the apollo package \href{https://www.rdocumentation.org/packages/apollo/versions/0.2.6/topics/apollo_modeChoiceData}{archive}
#'
#' @examples
#' data(apolloModeChoiceData)
#'
#' head(apolloModeChoiceData)
"apolloModeChoiceData"












#' Stated preference data for the choice of electricity suppliers (from {mlogit} package)
#'
#' A sample of 2308 households in the United States.
#'
#' @format
#' Variable | Description
#' -------- | ---------------------------------------------
#' `id` | individual identifiers
#' `obsID` | identifier for unique choice observation
#' `choice` | dummy code for choice (1 or 0)
#' `alt` | alternative in each choice observation
#' `pf` | fixed price at a stated cents per kWh, with the price varying over suppliers and experiments, for scenario i=(1, 2, 3, 4),
#' `cl` | the length of contract that the supplier offered, in years (such as 1 year or 5 years.) During this contract period, the supplier guaranteed the prices and the buyer would have to pay a penalty if he/she switched to another supplier. The supplier could offer no contract in which case either side could stop the agreement at any time. This is recorded as a contract length of 0.
#' `loc` | is the supplier a local company.
#' `wk` | is the supplier a well-known company.
#' `tod` | a time-of-day rate under which the price is 11 cents per kWh from 8am to 8pm and 5 cents per kWh from 8pm to 8am. These TOD prices did not vary over suppliers or experiments: whenever the supplier was said to offer TOD, the prices were stated as above.
#' `seas` | a seasonal rate under which the price is 10 cents per kWh in the summer, 8 cents per kWh in the winter, and 6 cents per kWh in the spring and fall. Like TOD rates, these prices did not vary. Note that the price is for the electricity only, not transmission and distribution, which is supplied by the local regulated utility.
#'
#' @docType data
#'
#' @usage data(electricity)
#'
#' @keywords datasets
#'
#' @references Croissant, Y. (2020). Estimation of Random Utility Models in R: The mlogit Package. Journal of Statistical Software, 95(11), 1–41.
#' \doi{10.18637/jss.v095.i11}
#'
#' @source [Kenneth Train's home page](https://elsa.berkeley.edu/~train/)
#'
#' @examples
#' data(electricity)
#'
#' head(electricity)
"electricity"
