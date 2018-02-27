# logitr
*logitr* estimates multinomial (MNL) and mixed logit (MXL) models and allows for models in the "preference space" or "willingness to pay (WTP) space." The main optimization loop uses the `nloptr` function to minimize the negative log-likelihood function. I chose to use `nloptr` over the Base R `optim` because it allows for both the objective and gradient functions to be included in one function, which speeds things up since both functions share many of the same calculations.

The current version includes support for:
- Homogeneous multinomial logit (MNL) models
- Heterogeneous mixed logit (MXL) models (support for normal and log-normal parameter distributions).
- Preference space models
- WTP space models.
- A multi-start option for searching the solution space from multiple random different starting points (useful for non-convex problems, such as those for WTP space models or MXL models).

The MXL models assume uncorrelated heterogeneity covariances. The MXL models are estimated using maximum simulated likelihood based on [Kenneth Train's](http://eml.berkeley.edu/~train/) book [*Discrete Choice Methods with Simulation, 2nd Edition (New York: Cambridge University Press, 2009).*](http://eml.berkeley.edu/books/choice2.html).

# Installation
First, make sure you have the `devtools` library installed:

`install.packages('devtools')`

Then run these commands to install and load the `logitr` package:

```
library('devtools')
install_github('jhelvy/logitr')
library('logitr')
```

# Required Libraries
The following libraries are required to run *logitr*:
- `randtoolbox` (for taking Halton draws in MXL models)
- `nloptr` (the main optimization)

# Contents
This package contains the following functions:

* `logitr()`: The main function for running the logitr program.
* `logitr.summary()`: Install over 300 frequently-used packages (see myPackageList.csv for list of libraries).
* `logitr.statusCodes()`: Prints a summary of an estimated model using the logitr package.

# Author and License Information
- Author: *John Paul Helveston* - www.jhelvy.com/logitr
- Date First Written: *Sunday, September 28, 2014*
- Most Recent Update: *Monday, February 27, 2018*
- License: GPL-3.
