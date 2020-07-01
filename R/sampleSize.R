# ============================================================================
# Functions for assessing sample size
# ============================================================================

#' Examine the standard errors from models before you collect choice data
#'
#' This function allows you to estimate a model on a design of experiment
#' that you have not yet used to collect data. This allows you to learn
#' about deficiencies in your design of experiment and also assess the sample
#' size needed to achieve parameter precision levels before you go out and
#' use the design to collect data. The function fills out the survey with 
#' random choices and estimates a model. It does this multiple times with an 
#' increasing number of observations, set by the "nbreaks" argument. While the 
#' coefficients in those models are meaningless, the standard errors on the 
#' coefficients are informative. The example below estimates 10 separate models 
#' and then plots the standard errors against the number of observations. In 
#' this example, assume that the yogurt data was not a completed survey but 
#' rather a blank design of experiment with no observed choices.
#' @keywords logitr, mnl, mxl, logit, sample size
#'
#' @export
#' @examples
#' 
#' test <- sampleSizer(
#'     data       = yogurt,
#'     obsIDName  = 'obsID',
#'     parNames   = c('price', 'feat', 'dannon', 'hiland', 'yoplait'), 
#'     nbreaks    = 10, 
#'     plot       = TRUE)

sampleSizer = function(data, obsIDName, parNames, nbreaks=10, plot=TRUE, 
                       priceName=NULL, randPars=NULL, randPrice=NULL, 
                       modelSpace='pref', options=list()) {
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
