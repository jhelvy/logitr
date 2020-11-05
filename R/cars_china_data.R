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
#' \href{https://doi.org/10.1016/j.tra.2015.01.002}{https://doi.org/10.1016/j.tra.2015.01.002}
#'
#' @source Raw data downloaded from \href{https://github.com/jhelvy/tra2015}{this repo}
#'
#' @examples
#' data(cars_china)
#'
#' head(cars_china)
"cars_china"


