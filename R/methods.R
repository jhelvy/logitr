# ============================================================================
# S3 Methods
# ============================================================================

#' Returns the computed WTP from a preference space model.
#'
#' Returns the computed WTP from a preference space model.
#' @keywords logitr, wtp
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' data(yogurt)
#'
#' mnl.pref = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Get the WTP implied from the preference space model
#' wtp(mnl.pref, priceName='price')
wtp <- function(model, priceName) {
    UseMethod('wtp', model)
}

#' Returns a comparison of the WTP between a preference space and WTP space
#' model.
#'
#' Returns a comparison of the WTP between a preference space and WTP space
#' model.
#' @keywords logitr, wtp
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' data(yogurt)
#'
#' mnl.pref = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Get the WTP implied from the preference space model
#' mnl.pref.wtp = wtp(mnl.pref, priceName='price')
#'
#' # Run a MNL model in the WTP Space:
#' mnl.wtp = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
#'   priceName  = 'price',
#'   modelSpace = 'wtp',
#'   options = list(startVals = mnl.pref.wtp$Estimate))
#'
#' # Compare the WTP between the two spaces:
#' wtpCompare(mnl.pref, mnl.wtp, priceName='price')
wtpCompare <- function(model.pref, model.wtp, priceName) {
    UseMethod('wtpCompare', model.pref)
}

#' Returns the expected market shares of a specific set of alternatives based
#' on an estimated model.
#'
#' Returns the expected market shares of a specific set of alternatives based
#' on an estimated model.
#' @keywords logitr, simluation
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' data(yogurt)
#'
#' mnl.pref = logitr(
#'   data       = yogurt,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Create a market to simulate.
#' market = subset(yogurt, obsID==1,
#'          select=c('feat', 'price', 'dannon', 'hiland', 'yoplait'))
#'
#' # Run the simulation using the estimated preference space MNL model:
#' marketSimulation(mnl.pref, market, alpha=0.025)
marketSimulation <- function(model, market, priceName, alpha) {
    UseMethod('marketSimulation', model)
}


