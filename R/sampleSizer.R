# ============================================================================
# Functions for assessing sample size
# ============================================================================

#' Preview standard errors from models before you collect choice data
#'
#' This function allows you to estimate a model on a design of experiment
#' that you have not yet used to collect data. This allows you to learn
#' about deficiencies in your design of experiment and also assess the sample
#' size needed to achieve parameter precision levels before you go out and
#' use the design to collect data. The function fills out the survey with
#' random choices and estimates a model. It does this multiple times with an
#' increasing number of observations, set by the `nbreaks` argument. While the
#' coefficients in those models are meaningless, the _standard errors_ on the
#' coefficients are informative. The example below estimates 10 separate models
#' and then plots the standard errors against the number of observations. In
#' this example, assume that the yogurt data was not a completed survey but
#' rather a blank design of experiment with no observed choices.
#' @keywords logitr, mnl, mxl, logit, sample size
#'
#' @param data The choice data, formatted as a `data.frame` object.
#' @param obsIDName The name of the column that identifies the `obsID` variable.
#' @param parNames The names of the parameters to be estimated in the model. Must be the same as the column names in the `data` argument. For WTP space models, do not include price in `parNames`.
#' @param nbreaks The number of different sample size groups.
#' @param priceName The name of the column that identifies the `price` variable. Only required for WTP space models. Defaults to `NULL`.
#' @param randPars A named vector whose names are the random parameters and values the distribution: `'n'` for normal or `'ln'` for log-normal. Defaults to `NULL`.
#' @param randPrice The random distribution for the price parameter: `'n'` for normal or `'ln'` for log-normal. Only used for WTP space MXL models. Defaults to `NULL`.
#' @param modelSpace Set to `'wtp'` for WTP space models. Defaults to `"pref"`.
#' @param plot Creates a plot of the sample size results. Defaults to `TRUE`.
#' @param options A list of options.
#' @return Returns a data frame of the standard error values for different sample sizes.
#' @export
#' @examples
#'
#' test <- sampleSizer(
#'     data       = yogurt,
#'     obsIDName  = 'obsID',
#'     parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'),
#'     nbreaks    = 10,
#'     plot       = TRUE)
#' head(test)
sampleSizer = function(data, obsIDName, parNames, nbreaks = 10, plot = TRUE,
                       priceName = NULL, randPars = NULL, randPrice = NULL,
                       modelSpace = 'pref', options = list()) {
    maxObs <- max(data[obsIDName])
    nobs <- ceiling(seq(ceiling(maxObs/nbreaks), maxObs, length.out = nbreaks))
    models <- list()
    for (i in 1:nbreaks) {
        tempData <- getTempData(data, obsIDName, nobs[i])
        model <- logitr(
            data       = tempData,
            choiceName = 'choice',
            obsIDName  = obsIDName,
            parNames   = parNames,
            priceName  = priceName,
            randPars   = randPars,
            randPrice  = randPrice,
            modelSpace = modelSpace,
            weights    = NULL,
            options    = options)
        models[[i]] <- getSE(model, nobs[i])
    }
    result <- do.call(rbind, models)
    if (plot) {
        plot(result$size, result$se, ylab = 'Standard Error',
             xlab = 'Number of observations')
    }
    return(result)
}

getTempData <- function(data, obsIDName, size) {
    tempData <- data[which(data[obsIDName] < size),]
    tempData$choice <- generateChoices(tempData, obsIDName, size)
    return(tempData)
}

generateChoices <- function(data, obsIDName, size) {
    nrows <- table(data[obsIDName])
    choices <- list()
    for (i in 1:length(nrows)) {
        choice <- rep(0, nrows[i])
        choice[sample(seq(nrows[i]), 1)] <- 1
        choices[[i]] <- choice
    }
    return(unlist(choices))
}

getSE <- function(model, size) {
    se <- data.frame(
        size = size,
        se   = model$standErrs)
    se$coef = row.names(se)
    row.names(se) <- NULL
    return(se)
}
