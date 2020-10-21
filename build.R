# Create the documentation for the package
devtools::document()

# Build the pkgdown site
pkgdown::build_site()

# Install the package
devtools::install(force = TRUE)

# Load the package and view the summary
library(logitr)
help(package = 'logitr')

# Install from github
# devtools::install_github('jhelvy/logitr')
