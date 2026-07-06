# ============================================================================
# logitr performance comparison (run by hand)
#
# Run this SAME script against two different installs of logitr:
#
#   1) The CRAN version:
#        install.packages("logitr")
#        Rscript bench/perf_compare.R
#      -> prints the "CRAN default (R backend)" time.
#
#   2) This development version (install a real -O2 build, NOT load_all):
#        # from the package directory:
#        devtools::install()          # or: R CMD INSTALL .
#        Rscript bench/perf_compare.R
#      -> prints the dev "default (cpp backend + threads)" time AND the
#         "backend = 'cpu'" time, plus the speedup between them.
#
# The script auto-detects which version is installed (via whether logitr() has
# the `backend` argument), so you don't edit anything between runs. Comparing
# the reported logLik across all three runs confirms they agree.
# ============================================================================

library(logitr)

ver <- as.character(utils::packageVersion("logitr"))
has_backend <- "backend" %in% names(formals(logitr))
cat("\n=====================================================\n")
cat(
  " logitr version:",
  ver,
  if (has_backend) "(dev: has backend arg)" else "(CRAN: no backend arg)",
  "\n"
)
cat(" cores detected: ", parallel::detectCores(), "\n")
cat("=====================================================\n\n")

# ---- Model settings (identical across every run) --------------------------
# Bump numDraws up (e.g. 2000, 5000) to make the speed gap more dramatic.
numDraws <- 10000

fit_args <- list(
  data = yogurt,
  outcome = "choice",
  obsID = "obsID",
  panelID = "id",
  pars = c("price", "feat", "brand"),
  randPars = c(price = "n", feat = "n", brand = "n"),
  numDraws = numDraws,
  numMultiStarts = 1,
  numCores = 1,
  drawType = "sobol"
)

time_fit <- function(extra = list()) {
  args <- c(fit_args, extra)
  invisible(gc())
  t <- system.time(model <- suppressMessages(do.call(logitr, args)))[[
    "elapsed"
  ]]
  list(time = t, logLik = as.numeric(logLik(model)), coefs = coef(model))
}

cat(sprintf(
  "Model: panel MXL, 3 random normal parameters, numDraws = %d\n\n",
  numDraws
))

# ---- Default backend (works on both CRAN and dev) -------------------------
label_def <- if (has_backend) {
  "dev default (cpp + threads)"
} else {
  "CRAN default (R backend)"
}
cat("Fitting with DEFAULT settings ...\n")
def <- time_fit()
cat(sprintf(
  "  %-30s  time = %7.2f s   logLik = %.4f\n",
  label_def,
  def$time,
  def$logLik
))
cat("  Coefficients:\n")
print(round(def$coefs, 5))
cat("\n")

# ---- cpu backend (dev version only) ---------------------------------------
if (has_backend) {
  cat("Fitting with backend = 'cpu' (native R) ...\n")
  cpu <- time_fit(list(backend = "cpu"))
  cat(sprintf(
    "  %-30s  time = %7.2f s   logLik = %.4f\n",
    "dev cpu backend (R)",
    cpu$time,
    cpu$logLik
  ))
  cat("  Coefficients:\n")
  print(round(cpu$coefs, 5))
  cat("\n")
  cat(sprintf(
    "Max abs. difference in coefficients (cpp vs cpu): %.2e\n",
    max(abs(def$coefs - cpu$coefs))
  ))
  cat(sprintf(
    "Speedup, default (cpp+threads) vs cpu backend: %.1fx\n\n",
    cpu$time / def$time
  ))
  cat("Compare the 'dev cpu backend' time above to the CRAN time from your\n")
  cat("other run: they use the same R implementation and should be similar.\n")
}
