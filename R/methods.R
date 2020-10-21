# ============================================================================
# S3 Methods
# ============================================================================

#' Returns the computed WTP from a preference space model.
#' @keywords logitr, wtp
#'
#' @param model The output of a "preference space" model estimated using the `logitr()` function.
#' @param priceName The name of the parameter that identifies price.
#'
#' @details
#' Willingness to pay is computed by dividing the estimated parameters of a utility model in the "preference" space by the price parameter. Uncertainty is handled via simulation.
#'
#' @return A data frame of the WTP estimates.#'
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' library(logitr)
#'
#' mnl_pref = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Get the WTP implied from the preference space model
#' wtp(mnl_pref, priceName = 'price')
#'
wtp <- function(model, priceName) {
    UseMethod('wtp', model)
}

#' Returns a comparison of the WTP between a preference space and WTP space
#' model.
#' @keywords logitr, wtp
#'
#' @param model_pref The output of a "preference space" model estimated using the `logitr()` function.
#' @param model_wtp The output of a "willingness to pay space" model estimated model using the `logitr()` function.
#' @param priceName The name of the parameter that identifies price.
#'
#' @details
#' Willingness to pay (WTP) is first computed from the preference space model by dividing the estimated parameters by the price parameter. Then those estimates are compared against the WTP values directly estimated from the "WTP" space model. Uncertainty is handled via simulation.
#'
#' @return A data frame comparing the WTP estimates from preference space and WTP space models.
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' library(logitr)
#'
#' mnl_pref = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Get the WTP implied from the preference space model
#' wtp_mnl_pref = wtp(mnl_pref, priceName = 'price')
#'
#' # Run a MNL model in the WTP Space:
#' mnl_wtp = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
#'   priceName  = 'price',
#'   modelSpace = 'wtp',
#'   options = list(startVals = mnl_pref_wtp$Estimate))
#'
#' # Compare the WTP between the two spaces:
#' wtpCompare(mnl_pref, mnl_wtp, priceName = 'price')
#'
wtpCompare <- function(model_pref, model_wtp, priceName) {
    UseMethod('wtpCompare', model_pref)
}

#' Simulate expected market shares
#'
#' Returns the expected shares of a specific set of alternatives based
#' on an estimated model.
#' @keywords logitr, simluation
#'
#' @param model The output of a model estimated using the `logitr()` function.
#' @param alts A data frame of a set of alternatives for which to simulate shares. Each row is an alternative and each column an attribute corresponding to parameter names in the estimated model.
#' @param priceName The name of the parameter that identifies price.
#' @param alpha The sensitivity of the computed confidence interval, e.g. a 90% CI is obtained with `alpha = 0.05`. Defaults to `alpha = 0.025`.
#'
#' @return A data frame with the estimated shares for each alternative in `alts`.
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' library(logitr)
#'
#' mnl_pref = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Create a set of alternatives for which to simulate shares:
#' market = subset(yogurt, obsID == 42,
#'        select=c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
#' row.names(market) = c('dannon', 'hiland', 'weight', 'yoplait')
#' market
#'
#' # Run the simulation using the estimated preference space MNL model:
#' simulateShares(mnl_pref, market)
#'
simulateShares <- function(model, alts, priceName, alpha) {
    UseMethod('simulateShares', model)
}

#' Get the model coefficients
#'
#' Returns the coefficients of an estimated model of the 'logitr' class.
#' @keywords logitr, coef
#' @param model The output of a model estimated using the `logitr()` function.
#' @return A vector of the coefficients from a model estimated using the `logitr()` function.
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' data(yogurt)
#'
#' mnl_pref = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Get the model coefficients:
#' coef(mnl_pref)
coef.logitr = function(model) {
    model = allRunsCheck(model)
    return(model$coef)
}

