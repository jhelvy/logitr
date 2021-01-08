# Build README.md
rmarkdown::render(input = 'README.Rmd', output_format = 'github_document')

# Create the documentation for the package
devtools::document()

# Install the package
devtools::install(force = TRUE)

# Build the pkgdown site
pkgdown::build_site()

# Check package
devtools::check()
devtools::check_win_release()
devtools::check_win_devel()

# Load the package and view the summary
library(logitr)
help(package = 'logitr')

# Install from github
devtools::install_github('jhelvy/logitr')

# Submit to CRAN
devtools::release(check = TRUE)
