// ============================================================================
// Compiled MXL negative log-likelihood + analytic gradient (preference space).
//
// Fused, streaming-over-draws implementation of the same math as logitr's R
// path (getMxlNegLL / computeMxlNegGradLL). Handles fixed + random parameters,
// normal / log-normal / censored-normal mixing distributions, panel and
// non-panel data, and observation weights. It never materializes the 2K
// partial matrices: each draw's contribution is accumulated on the fly, so
// peak memory is bounded by a few length-rowX / length-nObs vectors.
//
// Parameter layout (matching logitr): the gradient has one "mean" slot per
// variable (index 0 .. nVars-1) followed by one "sd" slot per random variable.
// `sdPos[k]` gives the gradient index of variable k's sd slot, or -1 if fixed.
//
// dist codes per variable: 0 = normal (or fixed), 1 = log-normal, 2 = censored
// ============================================================================
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List mxl_negll_grad_pref_cpp(
    const NumericMatrix& X,        // rowX x nVars (differenced design)
    const NumericMatrix& draws,    // R x nVars (standard draws; fixed cols = 0)
    const NumericVector& mean,     // nVars parameter means
    const NumericMatrix& chol,     // nVars x nVars covariance factor (diag or Cholesky)
    const IntegerVector& dist,     // nVars distribution codes
    const IntegerVector& xcol,     // nPars: X column (0-based) per gradient slot
    const IntegerVector& dcol,     // nPars: draw column (0-based), -1 = ones slot
    const IntegerVector& obsID,    // rowX, 1-based sequential
    const IntegerVector& panelID,  // nObs, 1-based sequential (length 0 if none)
    const NumericVector& weights,  // nUnit (nObs, or nPanel if panel)
    const int nObs,
    const int nPanel,              // 0 if non-panel
    const int nPars                // length of the returned gradient
) {
  const int rowX = X.nrow();
  const int nVars = X.ncol();
  const int R = draws.nrow();
  const bool panel = nPanel > 0;
  const int nUnit = panel ? nPanel : nObs;

  std::vector<double> pHat(nUnit, 0.0);
  std::vector<double> G(static_cast<size_t>(nUnit) * nPars, 0.0);

  std::vector<double> beta(nVars), fac(nVars);
  std::vector<double> ev(rowX);
  std::vector<double> sumExp(nObs), logit(nObs);
  std::vector<double> seg(static_cast<size_t>(nObs) * nVars);
  std::vector<double> logitPanel(panel ? nPanel : 0);

  for (int r = 0; r < R; ++r) {
    // Draw-specific betas: beta = mean + draws %*% chol (chol handles both the
    // uncorrelated diagonal and correlated lower-triangular cases), then the
    // mixing distribution and its ln/cn gradient adjustment factor.
    for (int k = 0; k < nVars; ++k) {
      double raw = mean[k];
      for (int m = 0; m < nVars; ++m) raw += draws(r, m) * chol(m, k);
      if (dist[k] == 1) {            // log-normal
        beta[k] = std::exp(raw);
        fac[k] = beta[k];
      } else if (dist[k] == 2) {     // censored-normal
        beta[k] = raw > 0.0 ? raw : 0.0;
        fac[k] = raw > 0.0 ? 1.0 : 0.0;
      } else {                       // normal / fixed
        beta[k] = raw;
        fac[k] = 1.0;
      }
    }

    // Utility, exp(V), and per-obs denominators
    std::fill(sumExp.begin(), sumExp.end(), 0.0);
    for (int i = 0; i < rowX; ++i) {
      double v = 0.0;
      for (int k = 0; k < nVars; ++k) v += X(i, k) * beta[k];
      double e = std::exp(v);
      ev[i] = e;
      sumExp[obsID[i] - 1] += e;
    }
    for (int o = 0; o < nObs; ++o) logit[o] = 1.0 / (1.0 + sumExp[o]);

    // seg[o,k] = sum over rows in obs o of X[row,k] * ev[row]
    std::fill(seg.begin(), seg.end(), 0.0);
    for (int i = 0; i < rowX; ++i) {
      int o = obsID[i] - 1;
      double e = ev[i];
      double* s = &seg[static_cast<size_t>(o) * nVars];
      for (int k = 0; k < nVars; ++k) s[k] += X(i, k) * e;
    }

    if (!panel) {
      for (int o = 0; o < nObs; ++o) {
        pHat[o] += logit[o];
        double lg2 = logit[o] * logit[o];
        const double* s = &seg[static_cast<size_t>(o) * nVars];
        double* g = &G[static_cast<size_t>(o) * nPars];
        // Every partial i contributes fac[xcol] * drawmult * seg[xcol];
        // drawmult is 1 for mean slots (dcol < 0) or the draw value for the
        // sd diagonal and off-diagonal (correlation) slots.
        for (int i = 0; i < nPars; ++i) {
          double dm = dcol[i] < 0 ? 1.0 : draws(r, dcol[i]);
          g[i] += lg2 * fac[xcol[i]] * dm * s[xcol[i]];
        }
      }
    } else {
      // Panel product of logit across obs within each panel
      std::fill(logitPanel.begin(), logitPanel.end(), 1.0);
      for (int o = 0; o < nObs; ++o) logitPanel[panelID[o] - 1] *= logit[o];
      for (int p = 0; p < nPanel; ++p) pHat[p] += logitPanel[p];
      // Accumulate gradient at the panel level
      for (int o = 0; o < nObs; ++o) {
        int p = panelID[o] - 1;
        double lp = logitPanel[p];
        double w = lp * logit[o];
        const double* s = &seg[static_cast<size_t>(o) * nVars];
        double* g = &G[static_cast<size_t>(p) * nPars];
        for (int i = 0; i < nPars; ++i) {
          double dm = dcol[i] < 0 ? 1.0 : draws(r, dcol[i]);
          g[i] += w * fac[xcol[i]] * dm * s[xcol[i]];
        }
      }
    }
  }

  double negLL = 0.0;
  for (int u = 0; u < nUnit; ++u) {
    pHat[u] /= R;
    negLL -= weights[u] * std::log(pHat[u]);
  }
  NumericVector grad(nPars);
  for (int u = 0; u < nUnit; ++u) {
    double f = weights[u] / (pHat[u] * R);
    const double* g = &G[static_cast<size_t>(u) * nPars];
    for (int i = 0; i < nPars; ++i) grad[i] += g[i] * f;
  }

  return List::create(_["objective"] = negLL, _["gradient"] = grad);
}

// ============================================================================
// WTP-space MXL negLL + gradient. Utility is V = lambda * (X %*% gamma - price),
// where parameter 0 is the scale (lambda) and 1..nVars-1 are the WTP
// coefficients (gamma / omega). Spec-driven so it handles uncorrelated and
// correlated heterogeneity uniformly (chol = diagonal of sds or lower-triangular
// Cholesky). Each gradient slot is classified by useQ/facIdx/mulLambda:
//   * lambda mean       -> useQ, facIdx = -1 (uses lamMeanFac), no lambda
//   * lambda sd / off-d -> useQ, facIdx = 0  (fac of lambda),   no lambda
//   * omega (gamma)     -> seg,  facIdx = beta index,           * lambda
// This mirrors getMxlV_wtp / mxlNegGradLL_wtp (lambda-mean partial = V/lambda
// via segQ; omega partials * lambda; lambda partials scaled by the lambda mean).
// ============================================================================

// [[Rcpp::export]]
List mxl_negll_grad_wtp_cpp(
    const NumericMatrix& X,        // rowX x nAttr (nAttr = nVars - 1)
    const NumericVector& price,    // rowX (differenced scalePar)
    const NumericMatrix& draws,    // R x nVars (col 0 = lambda draws)
    const NumericVector& mean,     // nVars parameter means (0 = lambda)
    const NumericMatrix& chol,     // nVars x nVars covariance factor (diag or Cholesky)
    const IntegerVector& dist,     // nVars distribution codes
    const IntegerVector& useQ,     // nPars: 1 = lambda slot (uses segQ), 0 = omega
    const IntegerVector& facIdx,   // nPars: beta index for fac, or -1 = lamMeanFac
    const IntegerVector& mulLambda,// nPars: 1 = multiply by lambda (omega slots)
    const IntegerVector& xcolX,    // nPars: X column (0-based) for omega slots
    const IntegerVector& dcol,     // nPars: draw column (0-based), -1 = ones slot
    const int lambdaRandom,        // 1 if the scale parameter is random
    const IntegerVector& obsID,
    const IntegerVector& panelID,
    const NumericVector& weights,
    const int nObs,
    const int nPanel,
    const int nPars
) {
  const int rowX = X.nrow();
  const int nAttr = X.ncol();
  const int nVars = nAttr + 1;
  const int R = draws.nrow();
  const bool panel = nPanel > 0;
  const int nUnit = panel ? nPanel : nObs;
  const bool lambdaLn = dist[0] == 1;

  std::vector<double> pHat(nUnit, 0.0);
  std::vector<double> G(static_cast<size_t>(nUnit) * nPars, 0.0);

  std::vector<double> beta(nVars), fac(nVars);
  std::vector<double> ev(rowX), q(rowX);
  std::vector<double> sumExp(nObs), logit(nObs);
  std::vector<double> seg(static_cast<size_t>(nObs) * nAttr), segQ(nObs);
  std::vector<double> logitPanel(panel ? nPanel : 0);

  for (int r = 0; r < R; ++r) {
    // beta = mean + draws %*% chol, then mixing distribution + ln/cn factor
    for (int k = 0; k < nVars; ++k) {
      double raw = mean[k];
      for (int m = 0; m < nVars; ++m) raw += draws(r, m) * chol(m, k);
      if (dist[k] == 1) { beta[k] = std::exp(raw); fac[k] = beta[k]; }
      else if (dist[k] == 2) { beta[k] = raw > 0.0 ? raw : 0.0; fac[k] = raw > 0.0 ? 1.0 : 0.0; }
      else { beta[k] = raw; fac[k] = 1.0; }
    }
    double lambda = beta[0];

    std::fill(sumExp.begin(), sumExp.end(), 0.0);
    for (int i = 0; i < rowX; ++i) {
      double xg = 0.0;
      for (int k = 0; k < nAttr; ++k) xg += X(i, k) * beta[k + 1];
      double qq = xg - price[i];
      q[i] = qq;
      double e = std::exp(lambda * qq);
      ev[i] = e;
      sumExp[obsID[i] - 1] += e;
    }
    for (int o = 0; o < nObs; ++o) logit[o] = 1.0 / (1.0 + sumExp[o]);

    // seg[o,k] = sum over rows in o of X[row,k]*ev; segQ[o] = sum of q*ev
    std::fill(seg.begin(), seg.end(), 0.0);
    std::fill(segQ.begin(), segQ.end(), 0.0);
    for (int i = 0; i < rowX; ++i) {
      int o = obsID[i] - 1;
      double e = ev[i];
      double* s = &seg[static_cast<size_t>(o) * nAttr];
      for (int k = 0; k < nAttr; ++k) s[k] += X(i, k) * e;
      segQ[o] += q[i] * e;
    }
    double lamMeanFac = (lambdaRandom && lambdaLn) ? lambda : 1.0;

    // Reduce every obs/panel unit against every gradient slot
    if (!panel) {
      for (int o = 0; o < nObs; ++o) {
        pHat[o] += logit[o];
        double wt = logit[o] * logit[o];
        const double* s = &seg[static_cast<size_t>(o) * nAttr];
        double* g = &G[static_cast<size_t>(o) * nPars];
        for (int i = 0; i < nPars; ++i) {
          double m = facIdx[i] < 0 ? lamMeanFac : fac[facIdx[i]];
          if (mulLambda[i]) m *= lambda;
          double dm = dcol[i] < 0 ? 1.0 : draws(r, dcol[i]);
          double sv = useQ[i] ? segQ[o] : s[xcolX[i]];
          g[i] += wt * m * dm * sv;
        }
      }
    } else {
      std::fill(logitPanel.begin(), logitPanel.end(), 1.0);
      for (int o = 0; o < nObs; ++o) logitPanel[panelID[o] - 1] *= logit[o];
      for (int p = 0; p < nPanel; ++p) pHat[p] += logitPanel[p];
      for (int o = 0; o < nObs; ++o) {
        int p = panelID[o] - 1;
        double wt = logitPanel[p] * logit[o];
        const double* s = &seg[static_cast<size_t>(o) * nAttr];
        double* g = &G[static_cast<size_t>(p) * nPars];
        for (int i = 0; i < nPars; ++i) {
          double m = facIdx[i] < 0 ? lamMeanFac : fac[facIdx[i]];
          if (mulLambda[i]) m *= lambda;
          double dm = dcol[i] < 0 ? 1.0 : draws(r, dcol[i]);
          double sv = useQ[i] ? segQ[o] : s[xcolX[i]];
          g[i] += wt * m * dm * sv;
        }
      }
    }
  }

  double negLL = 0.0;
  for (int u = 0; u < nUnit; ++u) {
    pHat[u] /= R;
    negLL -= weights[u] * std::log(pHat[u]);
  }
  NumericVector grad(nPars);
  for (int u = 0; u < nUnit; ++u) {
    double f = weights[u] / (pHat[u] * R);
    const double* g = &G[static_cast<size_t>(u) * nPars];
    for (int i = 0; i < nPars; ++i) grad[i] += g[i] * f;
  }

  return List::create(_["objective"] = negLL, _["gradient"] = grad);
}
