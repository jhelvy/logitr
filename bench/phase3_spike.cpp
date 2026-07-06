// Phase 3 spike: fused C++ negLL + analytic gradient for a preference-space,
// all-random-normal, non-panel MXL model. This is the worst case for the
// gradient loop (every parameter random => 2K partials), i.e. the 80-92% hot
// spot the profiling identified. It streams over draws (no 2K x rowX x R
// allocation) and fuses the segment-sums, mirroring logitr's exact math.
#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
List mxl_negll_grad_cpp(
    const NumericMatrix& X,       // rowX x K  (differenced, scaled design)
    const NumericMatrix& draws,   // R x K     (standard normal draws)
    const NumericVector& pars,    // length 2K (K means, then K sds)
    const IntegerVector& obsID,   // length rowX, 1-based, sequential
    const NumericVector& weights, // length nObs
    const int nObs
) {
  const int rowX = X.nrow();
  const int K = X.ncol();
  const int R = draws.nrow();

  std::vector<double> pHat(nObs, 0.0);
  std::vector<double> G(static_cast<size_t>(nObs) * 2 * K, 0.0); // nObs x 2K

  std::vector<double> beta(K);
  std::vector<double> ev(rowX);
  std::vector<double> sumExp(nObs);
  std::vector<double> logit(nObs);
  std::vector<double> segMean(static_cast<size_t>(nObs) * K);

  for (int r = 0; r < R; ++r) {
    for (int k = 0; k < K; ++k)
      beta[k] = pars[k] + pars[K + k] * draws(r, k);

    std::fill(sumExp.begin(), sumExp.end(), 0.0);
    for (int i = 0; i < rowX; ++i) {
      double v = 0.0;
      for (int k = 0; k < K; ++k) v += X(i, k) * beta[k];
      double e = std::exp(v);
      ev[i] = e;
      sumExp[obsID[i] - 1] += e;
    }
    for (int o = 0; o < nObs; ++o) {
      double lg = 1.0 / (1.0 + sumExp[o]);
      logit[o] = lg;
      pHat[o] += lg;
    }

    // segMean[o,k] = sum over rows in o of X[row,k]*ev[row]
    std::fill(segMean.begin(), segMean.end(), 0.0);
    for (int i = 0; i < rowX; ++i) {
      int o = obsID[i] - 1;
      double e = ev[i];
      double* seg = &segMean[static_cast<size_t>(o) * K];
      for (int k = 0; k < K; ++k) seg[k] += X(i, k) * e;
    }
    for (int o = 0; o < nObs; ++o) {
      double lg2 = logit[o] * logit[o];
      const double* seg = &segMean[static_cast<size_t>(o) * K];
      double* g = &G[static_cast<size_t>(o) * 2 * K];
      for (int k = 0; k < K; ++k) {
        double m = lg2 * seg[k];
        g[k]     += m;                 // mean partial
        g[K + k] += m * draws(r, k);   // sd partial (draw factor pulls out)
      }
    }
  }

  double negLL = 0.0;
  for (int o = 0; o < nObs; ++o) {
    pHat[o] /= R;
    negLL -= weights[o] * std::log(pHat[o]);
  }
  NumericVector grad(2 * K);
  for (int o = 0; o < nObs; ++o) {
    double f = weights[o] / (pHat[o] * R);
    const double* g = &G[static_cast<size_t>(o) * 2 * K];
    for (int i = 0; i < 2 * K; ++i) grad[i] += g[i] * f;
  }

  return List::create(_["objective"] = negLL, _["gradient"] = grad);
}
