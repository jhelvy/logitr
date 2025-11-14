# Package index

## Encoding Data

Functions for encoding data prior to model estimation.

- [`recodeData()`](https://jhelvy.github.io/logitr/reference/recodeData.md)
  :

  Returns a list of the design matrix `X` and updated `pars` and
  `randPars` to include any dummy-coded categorical or interaction
  variables.

- [`validate_data()`](https://jhelvy.github.io/logitr/reference/validate_data.md)
  : Validate data formatting for logitr models

## Estimating Models

Functions for estimating multinomial logit models with preference space
and WTP space utility parameterizations.

- [`logitr()`](https://jhelvy.github.io/logitr/reference/logitr.md) :
  The main function for estimating logit models

## Viewing Results

Functions for viewing the results of estimated models.

- [`logLik(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  [`terms(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  [`coef(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  [`coef(`*`<summary.logitr>`*`)`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  [`summary(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  [`print(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  [`print(`*`<summary.logitr>`*`)`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  [`print(`*`<logitr_wtp>`*`)`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  [`print(`*`<logitr_validation>`*`)`](https://jhelvy.github.io/logitr/reference/miscmethods.logitr.md)
  : Methods for logitr objects
- [`se(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/se.logitr.md)
  : Extract standard errors
- [`se()`](https://jhelvy.github.io/logitr/reference/se.md) : Extract
  standard errors
- [`vcov(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/vcov.logitr.md)
  : Calculate the variance-covariance matrix
- [`fitted(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/fitted.logitr.md)
  : Extract Model Fitted Values
- [`residuals(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/residuals.logitr.md)
  : Extract Model Residuals
- [`statusCodes()`](https://jhelvy.github.io/logitr/reference/statusCodes.md)
  : View a description the nloptr status codes

## Computing and Comparing WTP

Functions for computing and comparing WTP from estimated models.

- [`wtp()`](https://jhelvy.github.io/logitr/reference/wtp.md) : Get WTP
  estimates a preference space model
- [`wtp(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/wtp.logitr.md)
  : Get WTP estimates a preference space model
- [`wtpCompare()`](https://jhelvy.github.io/logitr/reference/wtpCompare.md)
  : Compare WTP from preference and WTP space models

## Predicting Probabilities & Outcomes

Functions for predicting probabilities and outcomes.

- [`predict(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/predict.logitr.md)
  : Predict probabilities and / or outcomes

## Tidy / Broom Methods

Methods for tidying up results from estimate models.

- [`augment(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/augment.logitr.md)
  :

  Glance a `logitr` class object

- [`confint(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/confint.logitr.md)
  : Extract Model Confidence Interval

- [`glance(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/glance.logitr.md)
  :

  Glance a `logitr` class object

- [`model.frame(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/model.frame.logitr.md)
  : Extracting the Model Frame from a Formula or Fit

- [`model.matrix(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/model.matrix.logitr.md)
  : Construct Design Matrices

- [`tidy(`*`<logitr>`*`)`](https://jhelvy.github.io/logitr/reference/tidy.logitr.md)
  :

  Tidy a `logitr` class object

## Example Data Sets

Descriptions of data included with this package

- [`yogurt`](https://jhelvy.github.io/logitr/reference/yogurt.md) :
  Choice observations of yogurt purchases by 100 households
- [`cars_us`](https://jhelvy.github.io/logitr/reference/cars_us.md) :
  Stated car choice observations by US car buyers
- [`cars_china`](https://jhelvy.github.io/logitr/reference/cars_china.md)
  : Stated car choice observations by Chinese car buyers
- [`apolloModeChoiceData`](https://jhelvy.github.io/logitr/reference/apolloModeChoiceData.md)
  : Simulated SP dataset of mode choice (from the apollo package).
- [`electricity`](https://jhelvy.github.io/logitr/reference/electricity.md)
  : Stated preference data for the choice of electricity suppliers (from
  mlogit package)
- [`runtimes`](https://jhelvy.github.io/logitr/reference/runtimes.md) :
  Data frame of run times for logitr benchmark

## Other Helpers

Other helper functions.

- [`fquantile()`](https://jhelvy.github.io/logitr/reference/fquantile.md)
  : Predict probabilities and / or outcomes
- [`ci()`](https://jhelvy.github.io/logitr/reference/ci.md) : Obtain a
  confidence interval from coefficient draws
- [`logit_probs()`](https://jhelvy.github.io/logitr/reference/logit_probs.md)
  : Compute logit fraction for sets of alternatives given coefficient
  draws
