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

## Required Libraries
*logitr* requires the following libraries:
- `nloptr` (for doing the optimization)
- `randtoolbox` (for taking Halton draws in MXL models)

The main optimization loop uses the `nloptr` function to minimize the negative log-likelihood function. `nloptr` is used instead of the Base R `optim` because it allows for both the objective and gradient functions to be included in one function. This speeds things up because both the objective and gradient functions share many of the same calculations.

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
`data`: The choice data, formatted as a data.frame object (see the [Data File Setup](# Data File Setup) Section for details).
`choiceName`: The name of the column that identifies the `choice` variable.
`obsIDName`: The name of the column that identifies the `obsID` variable.
`betaNames`: The names of the parameters to be estimated in the model. Must be the same as the column names in the `data` argument. For WTP space models, do not include price in betaNames.
`priceName`: The name of the column that identifies the price variable. Only required for WTP space models. Defaults to NULL if left unspecified.
`betaDist`: A vector describing the distributional assumptions on each parameter. 0=fixed, 1=normal, 2=log-normal. Only required for MXL models. Defaults to NULL if left unspecified.
`priceDist`: A number describing the distributional assumptions on the price parameter. 0=fixed, 1=normal, 2=log-normal. Only required for WTP space MXL models. Defaults to NULL if left unspecified.
`prefSpaceModel`: The user can provide an estimated preference space model as an input to a WTP space model. If included, the model will use the computed WTP from the preference space model as the starting parameter values for the first multistart run of the WTP space model. Also, a comparison of the computed WTP from the preference space model with the estimated WTP space model results will be provided. Defaults to NULL if left unspecified.
`standardDraws`: The user can provide a matrix of standard draws to be used for MXL models. Defaults to NULL if left unspecified.
`options`: A list of options (see the [Options](##Options) Section for details).

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
