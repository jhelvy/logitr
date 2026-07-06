# ============================================================================
# Cross-version parity harness for logitr
#
# Fits a battery of model specifications under two versions of the package in
# isolated subprocesses (via callr + pkgload) and reports the max abs
# difference in coefficients, log-likelihood, and standard errors.
#
#   DEV = current working tree (this checkout, incl. uncommitted changes)
#   REF = a reference checkout (default: a worktree of `master`, which matches
#         the CRAN release)
#
# Usage:
#   Rscript bench/compare_versions.R                       # ref = /tmp/logitr-master
#   Rscript bench/compare_versions.R /path/to/ref/checkout
#
# The default path uses numMultiStarts = 1 (deterministic zero start) so any
# difference reflects the estimation code, not RNG in the multistart.
# ============================================================================

args    <- commandArgs(trailingOnly = TRUE)
DEV_DIR <- normalizePath(".")
REF_DIR <- if (length(args) >= 1) args[1] else "/tmp/logitr-master"

# ---- Battery of model specifications (data referenced by name) -------------
specs <- list(
  "MNL pref" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand")),
  "MNL pref weighted" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand"), weights = "wt_placeholder"),
  "MNL wtp" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID",
    pars = c("feat", "brand"), scalePar = "price", numMultiStarts = 1),
  "MNL pref robust" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand"), robust = TRUE),
  "MXL pref n (panel)" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 50, numCores = 1),
  "MXL pref ln (panel)" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "ln"),
    numDraws = 50, numCores = 1),
  "MXL pref cn (panel)" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "cn"),
    numDraws = 50, numCores = 1),
  "MXL pref correlated" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n", price = "n"),
    correlation = TRUE, numDraws = 50, numCores = 1),
  "MXL wtp n (panel)" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("feat", "brand"), scalePar = "price", randPars = c(feat = "n"),
    numDraws = 50, numCores = 1, numMultiStarts = 1),
  "MXL pref no-panel" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 50, numCores = 1),
  # Large-draw spec: DEV streams (batchSize via .dev_extra, applied to DEV
  # only since REF/master has no numDrawsBatch arg); REF runs the same
  # numDraws with the stored-partials path. Confirms parity + compares speed
  # at a draw count where streaming's memory bound matters.
  "MXL pref n LARGE draws" = list(
    data = "yogurt", outcome = "choice", obsID = "obsID", panelID = "id",
    pars = c("price", "feat", "brand"), randPars = c(feat = "n"),
    numDraws = 1000, numCores = 1, vcov = FALSE, .reps = 1L,
    .dev_extra = list(numDrawsBatch = 100))
)

# Default vcov = TRUE (to also compare standard errors) unless a spec sets it
specs <- lapply(specs, function(s) { if (is.null(s$vcov)) s$vcov <- TRUE; s })

# ---- Install a package dir into its own stable library (pure R, so fast) ---
install_to_templib <- function(pkg_dir, label) {
  templib <- file.path("/tmp", paste0("logitr_parity_", Sys.getpid()), label)
  unlink(templib, recursive = TRUE)
  dir.create(templib, showWarnings = FALSE, recursive = TRUE)
  cat("Installing", label, "from", pkg_dir, "...\n")
  st <- system2(
    file.path(R.home("bin"), "R"),
    c("CMD", "INSTALL", "--no-docs", "--no-multiarch", "--no-byte-compile",
      paste0("--library=", templib), shQuote(pkg_dir)),
    stdout = FALSE, stderr = FALSE
  )
  if (st != 0) stop("Install failed for ", label, " (", pkg_dir, ")")
  templib
}

# ---- Runner: library() a given version in a fresh process and fit all specs -
# Each fit is timed (median of `reps` runs) so we can compare speed as well as
# numerical agreement.
run_battery <- function(templib, specs, reps = 3L, is_dev = FALSE) {
  callr::r(
    function(templib, specs, reps, is_dev) {
      library(logitr, lib.loc = templib)
      lapply(specs, function(s) {
        # Control keys (dot-prefixed) are not passed to logitr()
        spec_reps <- if (!is.null(s$.reps)) s$.reps else reps
        dev_extra <- s$.dev_extra
        s$.reps <- NULL; s$.dev_extra <- NULL
        if (is_dev && !is.null(dev_extra)) s <- utils::modifyList(s, dev_extra)
        s$data <- get(s$data)  # resolve dataset name to the object
        if (!is.null(s$weights) && s$weights == "wt_placeholder") {
          s$data$wt_placeholder <- rep(1, nrow(s$data))
        }
        run <- function() suppressMessages(suppressWarnings(do.call(logitr, s)))
        m <- tryCatch(run(), error = function(e) e)
        if (inherits(m, "error")) return(list(error = conditionMessage(m)))
        times <- vapply(seq_len(spec_reps),
                        function(i) system.time(run())[["elapsed"]], numeric(1))
        list(coef = coef(m), ll = as.numeric(logLik(m)),
             se = tryCatch(m$se, error = function(e) NULL),
             time = stats::median(times))
      })
    },
    args = list(templib, specs, reps, is_dev),
    libpath = c(templib, .libPaths())
  )
}

cat("DEV:", DEV_DIR, "\nREF:", REF_DIR, "\n\n")
dev_lib <- install_to_templib(DEV_DIR, "dev")
ref_lib <- install_to_templib(REF_DIR, "ref")
dev <- run_battery(dev_lib, specs, is_dev = TRUE)
ref <- run_battery(ref_lib, specs, is_dev = FALSE)

# ---- Compare ---------------------------------------------------------------
maxabs <- function(a, b) {
  if (is.null(a) || is.null(b)) return(NA_real_)
  diff <- abs(as.numeric(a) - as.numeric(b))
  # Ignore positions that are NA/NaN in both versions (e.g. boundary SEs)
  both_na <- is.na(as.numeric(a)) & is.na(as.numeric(b))
  diff[both_na] <- 0
  if (all(is.na(diff))) return(0)
  max(diff, na.rm = TRUE)
}

cat(sprintf("%-24s %10s %10s %10s | %8s %8s %7s   %s\n",
            "spec", "d_coef", "d_logLik", "d_se",
            "ref(s)", "dev(s)", "speedup", "status"))
worst <- 0
for (nm in names(specs)) {
  d <- dev[[nm]]; r <- ref[[nm]]
  if (!is.null(d$error) || !is.null(r$error)) {
    cat(sprintf("%-24s %10s %10s %10s | %8s %8s %7s   %s\n", nm, "-", "-", "-",
                "-", "-", "-", paste0("ERROR: ", c(d$error, r$error)[1])))
    next
  }
  dc <- maxabs(d$coef, r$coef)
  dl <- abs(d$ll - r$ll)
  ds <- maxabs(d$se, r$se)
  worst <- max(worst, dc, dl, ds, na.rm = TRUE)
  status <- if (max(dc, dl, ds, na.rm = TRUE) < 1e-6) "OK" else "** CHECK **"
  cat(sprintf("%-24s %10.2e %10.2e %10.2e | %8.3f %8.3f %6.2fx   %s\n",
              nm, dc, dl, ds, r$time, d$time, r$time / d$time, status))
}
cat(sprintf("\nWorst difference across all specs: %.2e\n", worst))
cat(if (worst < 1e-6) "PASS: versions agree to < 1e-6\n" else
    "REVIEW: some specs differ by >= 1e-6\n")
cat("(speedup > 1 means DEV is faster; timings are median of a few runs)\n")
