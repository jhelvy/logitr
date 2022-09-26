# Methods for logitr objects in broom package

#' @importFrom generics tidy
#' @export
generics::tidy

#' @importFrom generics glance
#' @export
generics::glance


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
tidy.logitr <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {

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
