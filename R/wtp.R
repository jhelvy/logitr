# ============================================================================
# Functions for computing the WTP from estimated models
# ============================================================================

#' Get WTP from a preference space model
#'
#' Returns the computed WTP from a preference space model.
#' @keywords logitr wtp
#'
#' @param model The output of a "preference space" model estimated
#' using the `logitr()` function.
#' @param priceName The name of the parameter that identifies price.
#'
#' @details
#' Willingness to pay is computed by dividing the estimated parameters of a
#' utility model in the "preference" space by the price parameter.
#' Uncertainty is handled via simulation.
#'
#' @return A data frame of the WTP estimates.
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' library(logitr)
#'
#' mnl_pref <- logitr(
#'   data = yogurt,
#'   choiceName = "choice",
#'   obsIDName = "obsID",
#'   parNames = c("price", "feat", "dannon", "hiland", "yoplait")
#' )
#'
#' # Get the WTP implied from the preference space model
#' wtp(mnl_pref, priceName = "price")
wtp <- function(model, priceName) {
  wtpInputsCheck(model, priceName)
  coefs <- model$coef
  priceID <- which(names(coefs) == priceName)
  pricePar <- -1 * coefs[priceID]
  wtp_mean <- coefs / pricePar
  wtp_mean[priceID] <- -1 * coefs[priceID]
  names(wtp_mean)[priceID] <- "lambda"
  # Compute standErrs using simulation (draws from the varcov matrix)
  draws <- getUncertaintyDraws(model, 10^5)
  priceDraws <- repmatCol(-1 * draws[priceName], ncol(draws))
  wtpDraws <- draws / priceDraws
  wtpDraws[, priceID] <- draws[, priceID]
  wtp_se <- apply(wtpDraws, 2, stats::sd)
  return(getCoefTable(wtp_mean, wtp_se))
}

#' Compare WTP from preference and WTP space models
#'
#' Returns a comparison of the WTP between a preference space and WTP space
#' model.
#' @keywords logitr wtp
#'
#' @param model_pref The output of a "preference space" model estimated using
#' the `logitr()` function.
#' @param model_wtp The output of a "willingness to pay space" model estimated
#' using the `logitr()` function.
#' @param priceName The name of the parameter that identifies price.
#'
#' @details
#' Willingness to pay (WTP) is first computed from the preference space model
#' by dividing the estimated parameters by the price parameter. Then those
#' estimates are compared against the WTP values directly estimated from the
#' "WTP" space model. Uncertainty is handled via simulation.
#'
#' @return A data frame comparing the WTP estimates from preference space and
#' WTP space models.
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' library(logitr)
#'
#' mnl_pref <- logitr(
#'   data = yogurt,
#'   choiceName = "choice",
#'   obsIDName = "obsID",
#'   parNames = c("price", "feat", "dannon", "hiland", "yoplait")
#' )
#'
#' # Get the WTP implied from the preference space model
#' wtp_mnl_pref <- wtp(mnl_pref, priceName = "price")
#'
#' # Run a MNL model in the WTP Space:
#' mnl_wtp <- logitr(
#'   data = yogurt,
#'   choiceName = "choice",
#'   obsIDName = "obsID",
#'   parNames = c("feat", "dannon", "hiland", "yoplait"),
#'   priceName = "price",
#'   modelSpace = "wtp",
#'   options = list(startVals = wtp_mnl_pref$Estimate)
#' )
#'
#' # Compare the WTP between the two spaces:
#' wtpCompare(mnl_pref, mnl_wtp, priceName = "price")
wtpCompare <- function(model_pref, model_wtp, priceName) {
  wtpCompareInputsCheck(model_pref, model_wtp, priceName)
  pref <- as.data.frame(wtp(model_pref, priceName))$Estimate
  pref <- c(pref, model_pref$logLik)
  wtp <- model_wtp$coef
  wtp <- c(wtp, model_wtp$logLik)
  names(pref)[length(pref)] <- "logLik"
  names(wtp)[length(wtp)] <- "logLik"
  compare <- data.frame(pref = pref, wtp = wtp)
  compare$difference <- round(compare$wtp - compare$pref, 8)
  compare <- format(compare, scientific = FALSE)
  return(compare)
}
