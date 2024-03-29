% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/broom.R
\name{glance.logitr}
\alias{glance.logitr}
\title{Glance a \code{logitr} class object}
\usage{
\method{glance}{logitr}(x, ...)
}
\arguments{
\item{x}{is an object of class \code{logitr}.}

\item{...}{further arguments.}
}
\value{
A tibble of the model summary statistics.
}
\description{
Glance a \code{logitr} class object
}
\examples{
library(logitr)

# Estimate a preference space model
mnl_pref <- logitr(
  data    = yogurt,
  outcome = "choice",
  obsID   = "obsID",
  pars    = c("price", "feat", "brand")
)

# Extract a tibble of the model summary statistics
glance(mnl_pref)

}
