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
#' `feat` | dummy for whether a newspaper advertisement was shown to the
#' customer (`1` or `0`)
#' `brand` | yogurt brand: `"yoplait"`, `"dannon"`, `"hiland"`, or `"weight"`
#' (for weight watcher)
#' `dannon` | dummy variable for the `"dannon"` brand (`1` or `0`)
#' `hiland` | dummy variable for the `"hiland"` brand (`1` or `0`)
#' `weight` | dummy variable for the `"weight"` brand (`1` or `0`)
#' `yoplait` | dummy variable for the `"yoplait"` brand (`1` or `0`)
#'
#' @docType data
#'
#' @usage data(yogurt)
#'
#' @keywords datasets
#'
#' @references Jain, Dipak C., Naufel J. Vilcassim and Pradeep K. Chintagunta
#' (1994) "A random–coefficients logit brand–choice model applied to panel
#' data", Journal of Business and Economics Statistics, 12(3), 317.
#' (\href{https://www.jstor.org/stable/1392088}{jstor})
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
#' `phev10`         | dummy code for PHEV vehicle type w/10 mile electric
#' driving range (`1` or `0`)
#' `phev20`         | dummy code for PHEV vehicle type w/20 mile electric
#' driving range (`1` or `0`)
#' `phev40`         | dummy code for PHEV vehicle type w/40 mile electric
#' driving range (`1` or `0`)
#' `bev75`          | dummy code for BEV vehicle type w/75 mile electric
#' driving range (`1` or `0`)
#' `bev100`         | dummy code for BEV vehicle type w/100 mile electric
#' driving range (`1` or `0`)
#' `bev150`         | dummy code for BEV vehicle type w/150 mile electric
#' driving range (`1` or `0`)
#' `phevFastcharge` | dummy code for whether PHEV vehicle had fast charging
#' capability (`1` or `0`)
#' `bevFastcharge`  | dummy code for whether BEV vehicle had fast charging
#' capability (`1` or `0`)
#' `price`          | price of vehicle ($USD)
#' `opCost`         | operating cost of vehicle (US cents / mile)
#' `accelTime`      | 0-60 mph acceleration time (seconds)
#' `american`       | dummy code for whether American brand (`1` or `0`)
#' `japanese`       | dummy code for whether Japanese brand (`1` or `0`)
#' `chinese`        | dummy code for whether Chinese brand (`1` or `0`)
#' `skorean`        | dummy code for whether S. Korean brand (`1` or `0`)
#' `weights`        | weights for each individual computed so that the sample
#' age and income demographics matched with those of the general car-buying population
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
#' \href{https://doi.org/10.1016/j.tra.2015.01.002}{https://doi.org/10.1016/j.tra.2015.01.002}
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
#' `phev10`         | dummy code for PHEV vehicle type w/10 mile electric
#' driving range (`1` or `0`)
#' `phev20`         | dummy code for PHEV vehicle type w/20 mile electric
#' driving range (`1` or `0`)
#' `phev40`         | dummy code for PHEV vehicle type w/40 mile electric
#' driving range (`1` or `0`)
#' `bev75`          | dummy code for BEV vehicle type w/75 mile electric
#' driving range (`1` or `0`)
#' `bev100`         | dummy code for BEV vehicle type w/100 mile electric
#' driving range (`1` or `0`)
#' `bev150`         | dummy code for BEV vehicle type w/150 mile electric
#' driving range (`1` or `0`)
#' `phevFastcharge` | dummy code for whether PHEV vehicle had fast charging
#' capability (`1` or `0`)
#' `bevFastcharge`  | dummy code for whether BEV vehicle had fast charging
#' capability (`1` or `0`)
#' `price`          | price of vehicle ($USD)
#' `opCost`         | operating cost of vehicle (US cents / mile)
#' `accelTime`      | 0-60 mph acceleration time (seconds)
#' `american`       | dummy code for whether American brand (`1` or `0`)
#' `japanese`       | dummy code for whether Japanese brand (`1` or `0`)
#' `chinese`        | dummy code for whether Chinese brand (`1` or `0`)
#' `skorean`        | dummy code for whether S. Korean brand (`1` or `0`)
#' `weights`        | weights for each individual computed so that the sample age
#' and income demographics matched with those of the general car-buying population
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
#' \href{https://doi.org/10.1016/j.tra.2015.01.002}{https://doi.org/10.1016/j.tra.2015.01.002}
#'
#' @source Raw data downloaded from \href{https://github.com/jhelvy/tra2015}{this repo}
#'
#' @examples
#' data(cars_china)
#'
#' head(cars_china)
"cars_china"
