# Methods for logitr objects in broom package

#' @importFrom generics tidy
#' @export
generics::tidy

tidy.lm <- function(x, conf.int = FALSE, conf.level = 0.95, ...) {

    result <- stats::coef(summary(x)) %>%
        tibble::as_tibble(rownames = "term") %>%
        dplyr::rename(estimate = Estimate,
                      std.error = `Std. Error`,
                      statistic = `z-value`,
                      p.value = `Pr(>|z|)`)

    if (conf.int) {
        ci <- confint(x, level = conf.level)
        result <- dplyr::left_join(result, ci, by = "term")
    }

    return(result)
}
