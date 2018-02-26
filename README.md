# logitr
*logitr* estimates both multinomial (MNL) and mixed logit (MXL) models in the preference and willingness to pay (WTP) spaces. The main optimization loop uses the nloptr function to minimize the negative log-likelihood function.

The current version includes support for:
- Homogeneous multinomial logit (MNL) models
- Heterogeneous mixed logit (MXL) models (support for normal and log-normal parameter distributions).
- Preference space and WTP space models.
- A multi-start option for searching the solution space from multiple random different starting points (primarily useful for non-convex problems, such as those for the WTP space models).

The MXL models assume uncorrelated heterogeneity covariances. The main estimation algorithms are based those in [Kenneth Train's](http://eml.berkeley.edu/~train/) book [*Discrete Choice Methods with Simulation, 2nd Edition (New York: Cambridge University Press, 2009).*](http://eml.berkeley.edu/books/choice2.html) The mixed logit models are estimated through maximum simulated likelihood. The main optimization loop uses the *optim* function by default to minimize the negative log-likelihood function, but *optimx* can also be used if desired.

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
- `data.table` (for logit computations)
- `randtoolbox` (for taking Halton draws in MXL models)
- `nloptr` (for the main optimization)

# Contents
This package contains the following functions:

* `logitr()`: The main function for running the logitr program.
* `logitr.summary()`: Install over 300 frequently-used packages (see myPackageList.csv for list of libraries).
* `logitr.statusCodes()`: Prints a summary of an estimated model using the logitr package.

# Author and License Information
- Author: *John Paul Helveston* - www.jhelvy.com/logitr
- Date First Written: *Sunday, September 28, 2014*
- Most Recent Update: *Monday, February 26, 2018*
- License: GPL-3.
