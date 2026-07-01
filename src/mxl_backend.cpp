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
    const NumericVector& sdFull,   // nVars sd values (0 for fixed vars)
    const IntegerVector& dist,     // nVars distribution codes
    const IntegerVector& sdPos,    // nVars gradient index of sd slot (-1 fixed)
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
    // Draw-specific betas and the ln/cn gradient adjustment factor
    for (int k = 0; k < nVars; ++k) {
      double raw = mean[k] + sdFull[k] * draws(r, k);
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
        for (int k = 0; k < nVars; ++k) {
          double base = lg2 * fac[k] * s[k];
          g[k] += base;                                 // mean slot
          if (sdPos[k] >= 0) g[sdPos[k]] += base * draws(r, k);
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
        for (int k = 0; k < nVars; ++k) {
          double base = w * fac[k] * s[k];
          g[k] += base;
          if (sdPos[k] >= 0) g[sdPos[k]] += base * draws(r, k);
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
