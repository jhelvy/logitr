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
