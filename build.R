# For restarting
rm(list = ls())
.rs.restartR()
devtools::load_all()
devtools::test()

# Create the documentation for the package
devtools::document()

# Install the package
devtools::install(force = TRUE)

# Run all examples to save results
source(here::here("inst", "example", "mnl_models.R"))
source(here::here("inst", "example", "mnl_models_weighted.R"))
source(here::here("inst", "example", "interactions.R"))
source(here::here("inst", "example", "mxl_models.R"))
source(here::here("inst", "example", "simulations.R"))
source(here::here("inst", "example", "predictions.R"))
source(here::here("inst", "example", "data_formatting.R"))

# Build the pkgdown site
pkgdown::build_site()

# Check package
devtools::check()
devtools::check_win_release()
devtools::check_win_devel()
devtools::check_rhub()

# Load the package and view the summary
library(logitr)
help(package = 'logitr')

# Install from github
devtools::install_github('jhelvy/logitr')

# Submit to CRAN
devtools::release(check = TRUE)
