# Create the documentation for the package
devtools::document()

# Install the package
devtools::install(force = TRUE)

# Build the pkgdown site
pkgdown::build_site()

# Check package
devtools::check()

# Load the package and view the summary
library(logitr)
help(package = 'logitr')

# Install from github
# devtools::install_github('jhelvy/logitr')


# Make hex sticker --------------------

library(hexSticker)
library(latex2exp)
library(ggplot2)
library(showtext)
p <- ggplot() +
    theme_void() +
    theme_transparent() +
    annotate(
        geom = "text", x = 0, y = 0,
        label = TeX('$\\hat{\\omega} = \\frac{\\hat{\\beta}}{\\hat{\\alpha}}$'),
        size = 6)
font_add_google("Fira Sans Condensed")
showtext_auto()
sticker(p,
    package = "logitr",
    h_fill = "#d9d9d9", h_color = "#007bff",
    p_color = "black", p_size = 10,
    s_x = 1, s_y = .75, s_width = 1, s_height=1,
    p_family = "Fira Sans Condensed", filename = "man/figures/logitr-hex.png")
