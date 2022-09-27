# Methods for logitr objects in broom package

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance

#' @importFrom generics augment
#' @export
generics::augment

#' Tidy a `logitr` class object
#'
#' @param x is an object of class `logitr`.
#' @param conf.int Logical indicating whether or not to include
#'   a confidence interval in the tidied output. Defaults to FALSE.
#' @param conf.level The confidence level to use for the confidence
#'   interval if conf.int = TRUE. Must be strictly greater than 0
#'   and less than 1. Defaults to 0.95, which corresponds to a
#'   95 percent confidence interval.
#' @param ... Unused, included for generic consistency only.
#' @return A tidy [tibble::tibble()] summarizing component-level
#'   information about the model
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
#' # Extract a tibble of the model coefficients
#' tidy(mnl_pref)
#'
#' # Extract a tibble of the model coefficients with confidence intervals
#' tidy(mnl_pref, conf.int = TRUE)
#'
#' @export
tidy.logitr <- function(
    x,
    conf.int = FALSE,
    conf.level = 0.95,
    ...
) {

    result <- stats::coef(summary(x)) %>%
        tibble::as_tibble(rownames = "term") %>%
        dplyr::rename(estimate = Estimate,
                      std.error = `Std. Error`,
                      statistic = `z-value`,
                      p.value = `Pr(>|z|)`)

    if (conf.int) {
        ci <- stats::confint(x, level = conf.level) %>%
            tibble::as_tibble(rownames = "term")
        result <- dplyr::left_join(result, ci, by = "term")
    }

    return(result)
}

#' Glance a `logitr` class object
#'
#' @param x is an object of class `logitr`.
#' @param ... further arguments.
#'
#' @return A tibble of the model summary statistics.
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
#' # Extract a tibble of the model summary statistics
#' glance(mnl_pref)
#'
#' @export
glance.logitr <- function(x, ...) {
    result <- as_tibble(t(summary(x)$statTable))
    names(result) <- c(
        'logLik', 'null.logLik', 'AIC', 'BIC', 'r.squared', 'adj.r.squared',
        'nobs'
    )
    return(result)
}

#' Glance a `logitr` class object
#'
#' @param x is an object of class `logitr`.
#' @param newdata a `data.frame`. Each row is an alternative and each column an
#' attribute corresponding to parameter names in the estimated model. Defaults
#' to `NULL`, in which case predictions are made on the original data used to
#' estimate the model.
#' @param obsID The name of the column that identifies each set of
#' alternatives in the data. Required if newdata != NULL. Defaults to `NULL`,
#' in which case the value for `obsID` from the data in `object` is used.
#' @param type A character vector defining what to predict: `prob` for
#' probabilities, `outcomes` for outcomes. If you want both outputs, use
#' `c("prob", "outcome")`. Outcomes are predicted randomly according to the
#' predicted probabilities. Defaults to `"prob"`.
#' @param ... further arguments.
#'
#' @return A tibble of ...
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
#' # Extract a tibble of the model summary statistics
#' augment(mnl_pref)
#'
#' @export
augment.logitr <- function(
    x,
    newdata = NULL,
    obsID   = NULL,
    type    = "prob",
    ...
) {
  if (is.null(obsID)) {
      obsIDName <- x$inputs$obsID
  }
  if (is.null(newdata)) {
      result <- predict(x, newdata = newdata, obsID = obsID, type = type) %>%
          dplyr::left_join(x$fitted.values, by = obsIDName) %>%
          rename(.fitted = fitted_value) %>%
          cbind(.resid = x$residuals$residual)
  } else {
      result <- predict(x, newdata = newdata, obsID = obsID, type = type)
  }
    return(result)
}
