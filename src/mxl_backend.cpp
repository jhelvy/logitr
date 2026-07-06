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
#include <RcppParallel.h>
using namespace Rcpp;
using namespace RcppParallel;
// [[Rcpp::depends(RcppParallel)]]

// Worker that accumulates the preference-space negLL sufficient statistics
// (pHat) and gradient numerator (G) over a range of draws. Draws are
// independent, so parallelReduce splits the range across threads and join()
// sums the per-thread accumulators. Uses thread-safe RMatrix/RVector accessors.
struct PrefWorker : public RcppParallel::Worker {
  const RMatrix<double> X;
  const RMatrix<double> draws;
  const RVector<double> mean;
  const RMatrix<double> chol;
  const RVector<int> dist;
  const RVector<int> xcol;
  const RVector<int> dcol;
  const RVector<int> obsID;
  const RVector<int> panelID;
  const int nObs, nPanel, nPars, nVars, rowX, nUnit;
  const bool panel;
  std::vector<double> pHat;
  std::vector<double> G;

  PrefWorker(const NumericMatrix& X_, const NumericMatrix& draws_,
             const NumericVector& mean_, const NumericMatrix& chol_,
             const IntegerVector& dist_, const IntegerVector& xcol_,
             const IntegerVector& dcol_, const IntegerVector& obsID_,
             const IntegerVector& panelID_, int nObs_, int nPanel_, int nPars_)
    : X(X_), draws(draws_), mean(mean_), chol(chol_), dist(dist_), xcol(xcol_),
      dcol(dcol_), obsID(obsID_), panelID(panelID_), nObs(nObs_), nPanel(nPanel_),
      nPars(nPars_), nVars(X_.ncol()), rowX(X_.nrow()),
      nUnit(nPanel_ > 0 ? nPanel_ : nObs_), panel(nPanel_ > 0),
      pHat(nUnit, 0.0), G(static_cast<size_t>(nUnit) * nPars_, 0.0) {}

  PrefWorker(const PrefWorker& o, RcppParallel::Split)
    : X(o.X), draws(o.draws), mean(o.mean), chol(o.chol), dist(o.dist),
      xcol(o.xcol), dcol(o.dcol), obsID(o.obsID), panelID(o.panelID),
      nObs(o.nObs), nPanel(o.nPanel), nPars(o.nPars), nVars(o.nVars),
      rowX(o.rowX), nUnit(o.nUnit), panel(o.panel),
      pHat(o.nUnit, 0.0), G(static_cast<size_t>(o.nUnit) * o.nPars, 0.0) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::vector<double> beta(nVars), fac(nVars), ev(rowX), sumExp(nObs),
      logit(nObs), seg(static_cast<size_t>(nObs) * nVars),
      logitPanel(panel ? nPanel : 0);
    for (std::size_t r = begin; r < end; ++r) {
      for (int k = 0; k < nVars; ++k) {
        double raw = mean[k];
        for (int m = 0; m < nVars; ++m) raw += draws(r, m) * chol(m, k);
        if (dist[k] == 1) { beta[k] = std::exp(raw); fac[k] = beta[k]; }
        else if (dist[k] == 2) { beta[k] = raw > 0.0 ? raw : 0.0; fac[k] = raw > 0.0 ? 1.0 : 0.0; }
        else { beta[k] = raw; fac[k] = 1.0; }
      }
      std::fill(sumExp.begin(), sumExp.end(), 0.0);
      for (int i = 0; i < rowX; ++i) {
        double v = 0.0;
        for (int k = 0; k < nVars; ++k) v += X(i, k) * beta[k];
        double e = std::exp(v);
        ev[i] = e;
        sumExp[obsID[i] - 1] += e;
      }
      for (int o = 0; o < nObs; ++o) logit[o] = 1.0 / (1.0 + sumExp[o]);
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
          for (int i = 0; i < nPars; ++i) {
            double dm = dcol[i] < 0 ? 1.0 : draws(r, dcol[i]);
            g[i] += lg2 * fac[xcol[i]] * dm * s[xcol[i]];
          }
        }
      } else {
        std::fill(logitPanel.begin(), logitPanel.end(), 1.0);
        for (int o = 0; o < nObs; ++o) logitPanel[panelID[o] - 1] *= logit[o];
        for (int p = 0; p < nPanel; ++p) pHat[p] += logitPanel[p];
        for (int o = 0; o < nObs; ++o) {
          int p = panelID[o] - 1;
          double w = logitPanel[p] * logit[o];
          const double* s = &seg[static_cast<size_t>(o) * nVars];
          double* g = &G[static_cast<size_t>(p) * nPars];
          for (int i = 0; i < nPars; ++i) {
            double dm = dcol[i] < 0 ? 1.0 : draws(r, dcol[i]);
            g[i] += w * fac[xcol[i]] * dm * s[xcol[i]];
          }
        }
      }
    }
  }

  void join(const PrefWorker& rhs) {
    for (int u = 0; u < nUnit; ++u) pHat[u] += rhs.pHat[u];
    size_t n = static_cast<size_t>(nUnit) * nPars;
    for (size_t i = 0; i < n; ++i) G[i] += rhs.G[i];
  }
};

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
    const int nPars,               // length of the returned gradient
    const int numThreads           // draws are processed in parallel if > 1
) {
  const int R = draws.nrow();
  PrefWorker worker(X, draws, mean, chol, dist, xcol, dcol, obsID, panelID,
                    nObs, nPanel, nPars);
  if (numThreads > 1) {
    // The active thread count is set from R via RcppParallel::setThreadOptions()
    RcppParallel::parallelReduce(0, R, worker);
  } else {
    worker(0, R);   // serial: accumulate directly into the worker
  }

  const int nUnit = worker.nUnit;
  double negLL = 0.0;
  for (int u = 0; u < nUnit; ++u) {
    worker.pHat[u] /= R;
    negLL -= weights[u] * std::log(worker.pHat[u]);
  }
  NumericVector grad(nPars);
  for (int u = 0; u < nUnit; ++u) {
    double f = weights[u] / (worker.pHat[u] * R);
    const double* g = &worker.G[static_cast<size_t>(u) * nPars];
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
//   * lambda (mean, sd, off-d) -> useQ, facIdx = 0 (fac of lambda), no lambda
//   * omega (gamma)            -> seg,  facIdx = beta index,        * lambda
// fac of lambda is d(lambda)/d(raw) per draw: 1 for normal/fixed, lambda for
// log-normal, and the censoring indicator 1{raw > 0} for censored-normal.
// This mirrors getMxlV_wtp / mxlNegGradLL_wtp (lambda partials use q via segQ;
// omega partials are multiplied by the per-draw lambda).
// ============================================================================

// WTP-space analogue of PrefWorker: accumulates pHat and G over a range of
// draws, using the useQ/facIdx/mulLambda slot classification.
struct WtpWorker : public RcppParallel::Worker {
  const RMatrix<double> X;
  const RVector<double> price;
  const RMatrix<double> draws;
  const RVector<double> mean;
  const RMatrix<double> chol;
  const RVector<int> dist;
  const RVector<int> useQ;
  const RVector<int> facIdx;
  const RVector<int> mulLambda;
  const RVector<int> xcolX;
  const RVector<int> dcol;
  const RVector<int> obsID;
  const RVector<int> panelID;
  const int lambdaRandom;
  const int nObs, nPanel, nPars, nVars, nAttr, rowX, nUnit;
  const bool panel, lambdaLn;
  std::vector<double> pHat;
  std::vector<double> G;

  WtpWorker(const NumericMatrix& X_, const NumericVector& price_,
            const NumericMatrix& draws_, const NumericVector& mean_,
            const NumericMatrix& chol_, const IntegerVector& dist_,
            const IntegerVector& useQ_, const IntegerVector& facIdx_,
            const IntegerVector& mulLambda_, const IntegerVector& xcolX_,
            const IntegerVector& dcol_, int lambdaRandom_,
            const IntegerVector& obsID_, const IntegerVector& panelID_,
            int nObs_, int nPanel_, int nPars_)
    : X(X_), price(price_), draws(draws_), mean(mean_), chol(chol_), dist(dist_),
      useQ(useQ_), facIdx(facIdx_), mulLambda(mulLambda_), xcolX(xcolX_),
      dcol(dcol_), obsID(obsID_), panelID(panelID_), lambdaRandom(lambdaRandom_),
      nObs(nObs_), nPanel(nPanel_), nPars(nPars_), nVars(X_.ncol() + 1),
      nAttr(X_.ncol()), rowX(X_.nrow()), nUnit(nPanel_ > 0 ? nPanel_ : nObs_),
      panel(nPanel_ > 0), lambdaLn(dist_[0] == 1),
      pHat(nUnit, 0.0), G(static_cast<size_t>(nUnit) * nPars_, 0.0) {}

  WtpWorker(const WtpWorker& o, RcppParallel::Split)
    : X(o.X), price(o.price), draws(o.draws), mean(o.mean), chol(o.chol),
      dist(o.dist), useQ(o.useQ), facIdx(o.facIdx), mulLambda(o.mulLambda),
      xcolX(o.xcolX), dcol(o.dcol), obsID(o.obsID), panelID(o.panelID),
      lambdaRandom(o.lambdaRandom), nObs(o.nObs), nPanel(o.nPanel),
      nPars(o.nPars), nVars(o.nVars), nAttr(o.nAttr), rowX(o.rowX),
      nUnit(o.nUnit), panel(o.panel), lambdaLn(o.lambdaLn),
      pHat(o.nUnit, 0.0), G(static_cast<size_t>(o.nUnit) * o.nPars, 0.0) {}

  void operator()(std::size_t begin, std::size_t end) {
    std::vector<double> beta(nVars), fac(nVars), ev(rowX), q(rowX),
      sumExp(nObs), logit(nObs), seg(static_cast<size_t>(nObs) * nAttr),
      segQ(nObs), logitPanel(panel ? nPanel : 0);
    for (std::size_t r = begin; r < end; ++r) {
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
      std::fill(seg.begin(), seg.end(), 0.0);
      std::fill(segQ.begin(), segQ.end(), 0.0);
      for (int i = 0; i < rowX; ++i) {
        int o = obsID[i] - 1;
        double e = ev[i];
        double* s = &seg[static_cast<size_t>(o) * nAttr];
        for (int k = 0; k < nAttr; ++k) s[k] += X(i, k) * e;
        segQ[o] += q[i] * e;
      }
      if (!panel) {
        for (int o = 0; o < nObs; ++o) {
          pHat[o] += logit[o];
          double wt = logit[o] * logit[o];
          const double* s = &seg[static_cast<size_t>(o) * nAttr];
          double* g = &G[static_cast<size_t>(o) * nPars];
          for (int i = 0; i < nPars; ++i) {
            double m = fac[facIdx[i]];
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
            double m = fac[facIdx[i]];
            if (mulLambda[i]) m *= lambda;
            double dm = dcol[i] < 0 ? 1.0 : draws(r, dcol[i]);
            double sv = useQ[i] ? segQ[o] : s[xcolX[i]];
            g[i] += wt * m * dm * sv;
          }
        }
      }
    }
  }

  void join(const WtpWorker& rhs) {
    for (int u = 0; u < nUnit; ++u) pHat[u] += rhs.pHat[u];
    size_t n = static_cast<size_t>(nUnit) * nPars;
    for (size_t i = 0; i < n; ++i) G[i] += rhs.G[i];
  }
};

// [[Rcpp::export]]
List mxl_negll_grad_wtp_cpp(
    const NumericMatrix& X, const NumericVector& price, const NumericMatrix& draws,
    const NumericVector& mean, const NumericMatrix& chol, const IntegerVector& dist,
    const IntegerVector& useQ, const IntegerVector& facIdx,
    const IntegerVector& mulLambda, const IntegerVector& xcolX,
    const IntegerVector& dcol, const int lambdaRandom, const IntegerVector& obsID,
    const IntegerVector& panelID, const NumericVector& weights, const int nObs,
    const int nPanel, const int nPars, const int numThreads
) {
  const int R = draws.nrow();
  WtpWorker worker(X, price, draws, mean, chol, dist, useQ, facIdx, mulLambda,
                   xcolX, dcol, lambdaRandom, obsID, panelID, nObs, nPanel, nPars);
  if (numThreads > 1) {
    RcppParallel::parallelReduce(0, R, worker);
  } else {
    worker(0, R);
  }

  const int nUnit = worker.nUnit;
  double negLL = 0.0;
  for (int u = 0; u < nUnit; ++u) {
    worker.pHat[u] /= R;
    negLL -= weights[u] * std::log(worker.pHat[u]);
  }
  NumericVector grad(nPars);
  for (int u = 0; u < nUnit; ++u) {
    double f = weights[u] / (worker.pHat[u] * R);
    const double* g = &worker.G[static_cast<size_t>(u) * nPars];
    for (int i = 0; i < nPars; ++i) grad[i] += g[i] * f;
  }

  return List::create(_["objective"] = negLL, _["gradient"] = grad);
}
