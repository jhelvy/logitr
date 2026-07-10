# logitr: Logit Models w/Preference & WTP Space Utility Parameterizations

Fast estimation of multinomial logit (MNL) and mixed logit (MXL) models.
Models can be estimated using preference space or willingness-to-pay
(WTP) space utility parameterizations. Weighted models can also be
estimated. An option is available to run a parallelized multistart
optimization loop with random starting points in each iteration, which
is useful for non-convex problems like MXL models or models with WTP
space utility parameterizations. The main optimization loop uses the
'nloptr' package to minimize the negative log-likelihood function.
Additional functions are available for computing and comparing WTP from
both preference space and WTP space models and for predicting expected
choices and choice probabilities for sets of alternatives based on an
estimated model. Mixed logit models can include uncorrelated or
correlated heterogeneity covariances and are estimated using maximum
simulated likelihood based on the algorithms in Train (2009)
[doi:10.1017/CBO9780511805271](https://doi.org/10.1017/CBO9780511805271)
. More details can be found in Helveston (2023)
[doi:10.18637/jss.v105.i10](https://doi.org/10.18637/jss.v105.i10) .

## See also

Useful links:

- <https://github.com/jhelvy/logitr>

- <https://jhelvy.github.io/logitr/>

- Report bugs at <https://github.com/jhelvy/logitr/issues>

## Author

**Maintainer**: John Helveston <john.helveston@gmail.com>
([ORCID](https://orcid.org/0000-0002-2657-9191)) \[copyright holder\]

Other contributors:

- Connor Forsythe <cforsyth@andrew.cmu.edu> \[contributor\]
