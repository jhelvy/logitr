# Utility Models in the Preference & WTP Space

In many applications of discrete choice models, modelers are interested
in estimating consumer’s marginal “willingness-to-pay” (WTP) for
different attributes. WTP can be estimated in two ways:

1.  Estimate a discrete choice model in the “preference space” where
    parameters have units of utility and then compute the WTP by
    dividing the parameters by the price parameter.
2.  Estimate a discrete choice model in the “WTP space” where parameters
    have units of WTP.

While the two procedures generally produce the same estimates of WTP for
homogenous models, the same is not true for heterogeneous models where
model parameters are assumed to follow a specific distribution, such as
normal or log-normal (Train and Weeks 2005). For example, in a
preference space specification, a normally distributed attribute
parameter divided by a log-normally distributed price parameter produces
a strange WTP distribution with large tails. In contrast, a WTP space
specification allows the modeler to directly assume WTP is normally
distributed. The package was developed to enable modelers to choose
between these two utility spaces when estimating multinomial logit
models.

## The random utility model in two spaces

The random utility model is a well-established framework in many fields
for estimating consumer preferences from observed consumer choices
(Louviere et al. 2000; Train 2009). Random utility models assume that
consumers choose the alternative $`j`$ a set of alternatives that has
the greatest utility $`u_{j}`$. Utility is a random variable that is
modeled as $`u_{j} = v_{j} + \varepsilon_{j}`$, where $`v_{j}`$ is the
“observed utility” (a function of the observed attributes such that
$`v_{j} = f(\mathbf{x}_{j})`$) and $`\varepsilon_{j}`$ is a random
variable representing the portion of utility unobservable to the
modeler.

Adopting the same notation as in Helveston et al. (2018), consider the
following utility model:

``` math
\begin{equation}
    u^*_{j} =
        \boldsymbol{\beta}^{*'} \mathbf{x}_{j} +
        \alpha^{*} p_{j} +
        \varepsilon^{*}_{j},
        \quad\quad
        \varepsilon^{*}_{j} \sim \textrm{Gumbel}\left(0, \sigma^2\frac{\pi^2}{6}\right)
\label{eq:utility}
\end{equation}
```

where $`\boldsymbol{\beta}^{*}`$ is the vector of coefficients for
non-price attributes $`\mathbf{x}_{j}`$, $`\alpha^{*}`$ is the
coefficient for price $`p_{j}`$, and the error term,
$`\varepsilon^{*}_{j}`$, is an IID random variable with a Gumbel extreme
value distribution of mean zero and variance $`\sigma^2(\pi^2/6)`$. This
model is not identified since there exists an infinite set of
combinations of values for $`\boldsymbol{\beta}^{*}`$, $`\alpha^{*}`$,
and $`\sigma`$ that produce the same choice probabilities. In order to
specify an identifiable model, the modeler must normalize equation . One
approach is to normalize the scale of the error term by dividing
equation by $`\sigma`$, producing the “preference space” utility
specification:

``` math
\begin{equation}
    \left(\frac{u^*_{j}}{\sigma}\right) =
        \left( \frac{\boldsymbol{\beta}^{*}}{\sigma} \right)' \mathbf{x}_{j} +
        \left( \frac{\alpha^{*}}{\sigma} \right) p_{j} +
        \left( \frac{\varepsilon^{*}_{j}}{\sigma} \right),
        \quad\quad
        \left( \frac{\varepsilon^{*}_{j}}{\sigma} \right) \sim \textrm{Gumbel}\left(0, \frac{\pi^2}{6}\right)
\label{eq:utilityPreferenceScaled}
\end{equation}
```

The typical preference space parameterization of the multinomial logit
(MNL) model can then be written by rewriting equation () with
$`u_j = (u^*_j / \sigma)`$,
$`\boldsymbol{\beta}= (\boldsymbol{\beta}^{*} / \sigma)`$,
$`\alpha = (\alpha^{*} / \sigma)`$, and
$`\varepsilon_{j} = (\varepsilon^{*}_{j} / \sigma)`$:

``` math
\begin{equation}
    u_{j} =
        \boldsymbol{\beta}' \mathbf{x}_{j} +
        \alpha p_{j} +
        \varepsilon_{j},
        \quad\quad
        \varepsilon_{j} \sim \textrm{Gumbel}\left(0,\frac{\pi^2}{6}\right)
\label{eq:utilityPreference}
\end{equation}
```

The vector $`\boldsymbol{\beta}`$ represents the marginal utility for
changes in each non-price attribute, and $`\alpha`$ represents the
marginal utility obtained from price reductions. In addition, the
coefficients $`\boldsymbol{\beta}`$ and $`\alpha`$ are measured in units
of *utility*, which only has relative rather than absolute meaning.

The alternative normalization approach is to normalize equation () by
$`- \alpha^*`$ instead of $`\sigma`$, producing the “willingness-to-pay
(WTP) space” utility specification:

``` math
\begin{equation}
    \left(\frac{u^*_{j}}{- \alpha^*}\right) =
        \left(\frac{\boldsymbol{\beta}^{*}}{- \alpha^{*}}\right)' \mathbf{x}_{j} +
        \left(\frac{\alpha^{*}}{- \alpha^{*}}\right) p_{j} +
        \left(\frac{\varepsilon^{*}_{j}}{- \alpha^{*}}\right),
        \quad\quad
        \left(\frac{\varepsilon^{*}_{j}}{- \alpha^{*}}\right) \sim \textrm{Gumbel} \left(0, \frac{\sigma^2}{(- \alpha^{*})^2}\frac{\pi^2}{6} \right)
\label{eq:utilityWtpScaled}
\end{equation}
```

Since the error term in equation is scaled by
$`\lambda^2 = \sigma^2/(- \alpha^{*})^2`$, we can rewrite equation () by
multiplying both sides by $`\lambda= (- \alpha^{*} / \sigma`$) and
renaming $`u_j = (\lambda u^*_j /- \alpha^*)`$,
$`\boldsymbol{\omega}= (\boldsymbol{\beta}^{*} /- \alpha^{*}`$), and
$`\varepsilon_j = (\lambda \varepsilon^*_j / - \alpha^*)`$:

``` math
\begin{equation}
    u_{j} =
        \lambda \left(
            \boldsymbol{\omega}' \mathbf{x}_{j} - p_{j}
            \right) +
        \varepsilon_{j},
        \quad\quad
        \varepsilon_{j} \sim \textrm{Gumbel}\left(0, \frac{\pi^2}{6}\right)
\label{eq:utilityWtp}
\end{equation}
```

Here $`\boldsymbol{\omega}`$ represents the marginal WTP for changes in
each non-price attribute, and $`\lambda`$ represents the scale of the
deterministic portion of utility relative to the standardized scale of
the random error term.

The utility models in equations and represent the preference space and
WTP space utility specifications, respectively. In equation , WTP is
estimated as $`\hat{\boldsymbol{\beta}} / \hat{- \alpha}`$; in equation
, WTP is simply $`\hat{\boldsymbol{\omega}}`$.

## References

Helveston, John Paul, Elea McDonnell Feit, and Jeremy J. Michalek. 2018.
“Pooling stated and revealed preference data in the presence of RP
endogeneity.” *Transportation Research Part B: Methodological* 109:
70–89.

Louviere, Jordan J., David A. Hensher, and Joffre Swait. 2000. *Stated
Choice Methods: Analysis and Applications*. Edited by J. J. et al.
Louviere. Cambridge University Press.

Train, Kenneth E. 2009. *Discrete Choice Methods with Simulation*. 2nd
ed. Cambridge University Press.

Train, Kenneth E., and Melvyn Weeks. 2005. “Discrete Choice Models in
Preference and Willingness-to-Pay Space.” Chap. 1 in *Appl. Simul.
Methods Environ. Resour. Econ.*
