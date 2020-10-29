# ============================================================================
# Other functions
# ============================================================================

# R equivalent of matlab's repmat function
repmat <- function(X, m, n) {
  mx <- dim(X)[1]
  nx <- dim(X)[2]
  return(matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = T))
}

# Replicates matrix mat n times by row
repmatRow <- function(mat, n) {
  return(mat[rep(seq(nrow(mat)), n), ])
}

# Replicates each row of matrix mat n times
repmatRowEach <- function(mat, n) {
  return(mat[rep(seq(nrow(mat)), each = n), ])
}

# Replicates matrix mat n times by column
repmatCol <- function(mat, n) {
  return(mat[, rep(seq(ncol(mat)), n)])
}

# Replicates each column of matrix mat n times
repmatColEach <- function(mat, n) {
  return(mat[, rep(seq(ncol(mat)), each = n)])
}

# Converts seconds into hours, minutes, and seconds
# (modified from the gmnl package)
convertTime <- function(time) {
  et <- time["elapsed"]
  if (et < 1) {
    s <- round(et, 2)
  } else {
    s <- round(et, 0)
  }
  h <- s %/% 3600
  s <- s - 3600 * h
  m <- s %/% 60
  s <- s - 60 * m
  return(paste(h, "h:", m, "m:", s, "s", sep = ""))
}

# Returns a confidence interval from a vector of data
ci <- function(data, alpha = 0.025) {
  B <- mean(data, na.rm = T)
  L <- stats::quantile(data, alpha, na.rm = T)
  U <- stats::quantile(data, 1 - alpha, na.rm = T)
  ests <- c(B, L, U)
  names(ests) <- c("mean", "low", "high")
  return(ests)
}

# Class check functions
is_logitr <- function(x) {
  inherits(x, "logitr")
}
is_logitr_multistart <- function(x) {
  inherits(x, "logitr.multistart")
}
is_logitr_allRuns <- function(x) {
  inherits(x, "logitr.allRuns")
}

allRunsCheck <- function(model) {
  if (is_logitr_allRuns(model)) {
    model <- useBestModel(model)
  }
  return(model)
}

# Return the best model, and print a warning statement
useBestModel <- function(model) {
  model <- model$bestModel
  modelRun <- paste(model$multistartNumber, "of",
    model$options$numMultiStarts,
    sep = " "
  )
  cat(paste(
    "**Using results for model ", modelRun, ",\n",
    "the best model (largest log-likelihood) from the multistart**",
    "\n",
    sep = ""
  ))
  return(model)
}

# Functions for getting specific parameter indexes
getFixedParIDs <- function(parSetup) {
  return(which(parSetup == "f"))
}

getRandParIDs <- function(parSetup) {
  return(which(parSetup != "f"))
}

getNormParIDs <- function(parSetup) {
  return(which(parSetup == "n"))
}

getLogNormParIDs <- function(parSetup) {
  return(which(parSetup == "ln"))
}

isMxlModel <- function(parSetup) {
  return(("n" %in% parSetup) | ("ln" %in% parSetup))
}
