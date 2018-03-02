# logitr
*logitr* estimates multinomial (MNL) and mixed logit (MXL) models and allows for models in the "preference space" or "willingness to pay (WTP) space." The current version includes support for:
- Homogeneous multinomial logit (MNL) models
- Heterogeneous mixed logit (MXL) models (support for normal and log-normal parameter distributions).
- Preference space models
- WTP space models.
- A multi-start option for searching the solution space from multiple random different starting points (useful for non-convex problems, such as those for WTP space models or MXL models).

The MXL models assume uncorrelated heterogeneity covariances. The MXL models are estimated using maximum simulated likelihood based on [Kenneth Train's](http://eml.berkeley.edu/~train/) book [*Discrete Choice Methods with Simulation, 2nd Edition (New York: Cambridge University Press, 2009)*](http://eml.berkeley.edu/books/choice2.html).

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
- `nloptr` (for doing the optimization)
- `randtoolbox` (for taking Halton draws in MXL models)

The main optimization loop uses the `nloptr` function to minimize the negative log-likelihood function. `nloptr` was used instead of the Base R `optim` because it allows for both the objective and gradient functions to be included in one function, which speeds things up since both functions share many of the same calculations.

# Contents
This package contains the following functions:

- `logitr()`: The main function for running the logitr program.
- `logitr.summary()`: Prints a summary of an estimated model using the logitr package.
- `logitr.statusCodes()`: Prints the status codes from the nloptr optimization routine.

# Usage
(See the 'example' folder for an example)

The main function is the `logitr` function:

```
logitr(data, choiceName, obsIDName, betaNames, priceName=NULL,
       betaDist=NULL, priceDist=NULL, prefSpaceModel=NULL, standardDraws=NULL,
       options=list(...))
```

##Arguments:

##Options:

##Values:

# Data File Setup
The data must be arranged such that each row is an alternative from a choice observation. The choice observations do not have to be symmetric (i.e. they could each have a different number of alternatives). The columns must include all variables that will be used as model covariates. In addition, the data must include each of the following variables:

`obsID`: A sequence of numbers that identifies each unique choice occasion. For example, if the first three choice occasions had 2 alternatives each, then the first 9 rows of the \emph{obsID} variable would be 1,1,2,2,3,3.
`choice`: A dummy variable that identifies which alternative was chosen (1=chosen, 0 = not chosen).

For WTP space models, you must include a `price` variable (entries should be the price value).

# Author, Version, and License Information
- Author: *John Paul Helveston* (www.jhelvy.com/logitr)
- Date First Written: *Sunday, September 28, 2014*
- Most Recent Update: *Friday, March 2, 2018*
- License: GPL-3
- Latest Version: 0.5.0
