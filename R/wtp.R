#' Returns the computed WTP from a preference space model.
#'
#' Returns the computed WTP from a preference space model.
#' @keywords logitr, wtp
#' @export
#' @examples
#' # Run a MNL model in the Preference Space:
#' choiceData = read.csv(
#'     file   = 'https://raw.github.com/jhelvy/logitr/master/example/yogurt.csv', header = TRUE)
#'
#' mnl.pref = logitr(
#'   data       = choiceData,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Get the WTP implied from the preference space model
#' wtp.logitr(mnl.pref, priceName='price')
wtp.logitr = function(model, priceName) {
    if (!('logitr' %in% class(model))) {
        stop('Model must be estimated using the"logitr" package')
    }
    if (is.null(priceName)) {
        stop('Must provide priceName to compute WTP')
    }
    if ('logitr.multistart' %in% class(model)) {
        cat('**Using results for the best model from the multistart**',
            '\n', sep='')
        model = model$bestModel
    }
    if (model$modelSpace == 'pref') {
        return(getPrefSpaceWtp(model, priceName))
    } else if (model$modelSpace == 'wtp') {
        wtp.mean = coef(model)
        wtp.se   = model$standErrs
        return(getCoefTable(wtp.mean, wtp.se, model$numObs, model$numParams))
    }
}

getPrefSpaceWtp = function(model, priceName) {
    # Compute mean WTP
    coefs             = coef(model)
    priceID           = which(names(coefs)==priceName)
    pricePar          = -1*coefs[priceID]
    wtp.mean          = coefs / pricePar
    wtp.mean[priceID] = -1*coefs[priceID]
    names(wtp.mean)[priceID] = 'lambda'
    # Compute standErrs using simulation (draws from the varcov matrix)
    draws      = getUncertaintyDraws(model)
    priceDraws = repmatCol(-1*draws[priceName], ncol(draws))
    wtpDraws   = draws / priceDraws
    wtpDraws[,priceID] = draws[,priceID]
    wtp.se = apply(wtpDraws, 2, sd)
    return(getCoefTable(wtp.mean, wtp.se, model$numObs, model$numParams))
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
#' choiceData = read.csv(
#'     file   = 'https://raw.github.com/jhelvy/logitr/master/example/yogurt.csv', header = TRUE)
#'
#' mnl.pref = logitr(
#'   data       = choiceData,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'))
#'
#' # Get the WTP implied from the preference space model
#' wtp.logitr(mnl.pref, priceName='price')
#'
#' # Run a MNL model in the WTP Space:
#' mnl.wtp = logitr(
#'   data       = choiceData,
#'   choiceName = 'choice',
#'   obsIDName  = 'obsID',
#'   parNames   = c('feat', 'dannon', 'hiland', 'yoplait'),
#'   priceName  = 'price',
#'   modelSpace = 'wtp',
#'   options = list(startVals = mnl.pref.wtp$Estimate))
#'
#' # Compare the WTP between the two spaces:
#' wtpCompare(mnl.pref, mnl.wtp, priceName='price')
wtpCompare = function(model.pref, model.wtp, priceName) {
    if (!('logitr' %in% class(model.pref)) |
        !('logitr' %in% class(model.wtp))) {
        stop('Models must be estimated using the"logitr" package')
    }
    if ('logitr.multistart' %in% class(model.pref)) {
        cat('**Using results for the best model from the model.pref ',
            'multistart**', '\n', sep='')
        model.pref = model.pref$bestModel
    }
    if ('logitr.multistart' %in% class(model.wtp)) {
        cat('**Using results for the best model from the model.wtp ',
            'multistart**', '\n', sep='')
        model.wtp = model.wtp$bestModel
    }
    pref = wtp.logitr(model.pref, priceName)$Estimate
    pref = c(pref, model.pref$logLik)
    wtp  = coef(model.wtp)
    wtp  = c(wtp, model.wtp$logLik)
    names(pref)[length(pref)] = 'logLik'
    names(wtp)[length(wtp)]   = 'logLik'
    compare = data.frame(pref=pref, wtp=wtp)
    compare$difference = round(compare$wtp - compare$pref, 8)
    return(compare)
}
