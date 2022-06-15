# ============================================================================
# Functions for computing the WTP from estimated models
# ============================================================================

#' Get WTP estimates a preference space model
#'
#' Returns the computed WTP from a preference space model.
#' @keywords logitr wtp
#'
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param scalePar The name of the column that identifies the scale variable,
#' which is typically "price" for WTP space models, but could be any
#' continuous variable, such as "time".
#'
#' @details
#' Willingness to pay is computed by dividing the estimated parameters of a
#' utility model in the "preference" space by the scale parameter, which is
#' should be price to obtain WTP estimates. Uncertainty is handled via
#' simulation.
#'
#' @return A data frame of the WTP estimates.
#' @export
#' @examples
#' library(logitr)
#'
#' # Estimate a preference space model
#' mnl_pref <- logitr(
#'   data    = yogurt,
#'   outcome = "choice",
#'   obsID   = "obsID",
#'   pars    = c("price", "feat", "brand")
#' )
#'
#' # Compute the WTP implied from the preference space model
#' wtp(mnl_pref, scalePar = "price")
wtp <- function(object, scalePar) {
  UseMethod("wtp")
}

#' Get WTP estimates a preference space model
#'
#' Returns the computed WTP from a preference space model.
#' @keywords logitr wtp
#'
#' @param object is an object of class `logitr` (a model estimated using
#' the 'logitr()` function).
#' @param scalePar The name of the column that identifies the scale variable,
#' which is typically "price" for WTP space models, but could be any
#' continuous variable, such as "time".
#'
#' @details
#' Willingness to pay is computed by dividing the estimated parameters of a
#' utility model in the "preference" space by the scale parameter, which is
#' should be price to obtain WTP estimates. Uncertainty is handled via
#' simulation.
#'
#' @return A data frame of the WTP estimates.
#' @export
#' @examples
#' library(logitr)
#'
#' # Estimate a preference space model
#' mnl_pref <- logitr(
#'   data    = yogurt,
#'   outcome = "choice",
#'   obsID   = "obsID",
#'   pars    = c("price", "feat", "brand")
#' )
#'
#' # Compute the WTP implied from the preference space model
#' wtp(mnl_pref, scalePar = "price")
wtp.logitr <- function(object, scalePar) {
  wtpInputsCheck(object, scalePar)
  coefs <- stats::coef(object)
  scaleParID <- which(names(coefs) == scalePar)
  wtp_mean <- coefs / (-1 * coefs[scaleParID])
  wtp_mean[scaleParID] <- -1 * coefs[scaleParID]
  names(wtp_mean)[scaleParID] <- "scalePar"
  # Compute standErrs using simulation (draws from the varcov matrix)
  draws <- getUncertaintyDraws(object, 10^5)
  scaleParDraws <- repmatCol(-1 * draws[scalePar], ncol(draws))
  wtpDraws <- draws / scaleParDraws
  wtpDraws[, scaleParID] <- draws[, scaleParID]
  wtp_se <- apply(wtpDraws, 2, stats::sd)
  result <- getCoefTable(wtp_mean, wtp_se)
  class(result) <- c("logitr_wtp", "data.frame")
  return(result)
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
#' @param scalePar The name of the column that identifies the scale variable,
#' which is typically "price" for WTP space models, but could be any
#' continuous variable, such as "time".
#'
#' @details
#' Willingness to pay (WTP) is first computed from the preference space model
#' by dividing the estimated parameters by the scale parameter (typically
#' "price" to obtain WTP estimates). Then those estimates are compared against
#' the WTP values directly estimated from the "WTP" space model. Uncertainty is
#' handled via simulation.
#'
#' @return A data frame comparing the WTP estimates from preference space and
#' WTP space models.
#' @export
#' @examples
#' library(logitr)
#'
#' # Estimate a MNL model in the Preference space
#' mnl_pref <- logitr(
#'   data    = yogurt,
#'   outcome = "choice",
#'   obsID   = "obsID",
#'   pars    = c("price", "feat", "brand")
#' )
#'
#' # Compute the WTP implied from the preference space model
#' wtp_mnl_pref <- wtp(mnl_pref, scalePar = "price")
#'
#' # Estimate a MNL model in the WTP Space, using the computed WTP values
#' # from the preference space model as starting points
#' mnl_wtp <- logitr(
#'   data       = yogurt,
#'   outcome    = "choice",
#'   obsID      = "obsID",
#'   pars       = c("feat", "brand"),
#'   scalePar   = "price",
#'   modelSpace = "wtp",
#'   startVals  = wtp_mnl_pref$Estimate
#' )
#'
#' # Compare the WTP between the two spaces
#' wtpCompare(mnl_pref, mnl_wtp, scalePar = "price")
wtpCompare <- function(model_pref, model_wtp, scalePar) {
  wtpCompareInputsCheck(model_pref, model_wtp, scalePar)
  pref <- wtp(model_pref, scalePar)$Estimate
  pref <- c(pref, model_pref$logLik)
  wtp <- stats::coef(model_wtp)
  wtp <- c(wtp, stats::logLik(model_wtp))
  names(pref)[length(pref)] <- "logLik"
  names(wtp)[length(wtp)] <- "logLik"
  compare <- data.frame(pref = pref, wtp = wtp)
  compare$difference <- round(compare$wtp - compare$pref, 8)
  compare <- format(compare, scientific = FALSE)
  return(compare)
}
