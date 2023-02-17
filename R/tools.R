# ============================================================================
# Other functions
# ============================================================================

#' View a description the nloptr status codes
#'
#' Prints a description of the status codes from the nloptr optimization routine.
#' @keywords logitr nloptr status codes
#' @return No return value; prints a summary of the `nloptr` status codes to
#' the console.
#' @export
#' @examples
#' statusCodes()
statusCodes <- function() {
  codes <- getStatusCodes()
  cat("Status codes:", "\n", sep = "")
  for (i in seq_len(nrow(codes))) {
    row <- codes[i,]
    cat(row$code, ": ", row$message, "\n", sep = "")
  }
}

getStatusCodes <- function() {
  result <- data.frame(
    code = c(1, 2, 3, 4, 5, 6, -1, -2, -3, -4, -5),
    message = c(
      "Generic success return value.",
      "Optimization stopped because stopval was reached.",
      "Optimization stopped because ftol_rel or ftol_abs was reached.",
      "Optimization stopped because xtol_rel or xtol_abs was reached.",
      "Optimization stopped because maxeval was reached.",
      "Optimization stopped because maxtime was reached.",
      "Generic failure code.",
      paste(
        "Invalid arguments (e.g. lower bounds are bigger than upper",
        "bounds, an unknown algorithm was specified, etc.)."
      ),
      "Ran out of memory.",
      paste(
        "Halted because roundoff errors limited progress. (In this case,",
        "the optimization still typically returns a useful result.)"
      ),
      paste(
        "Halted because of a forced termination: the user called",
        "nlopt_force_stop(opt) on the optimization's nlopt_opt object opt",
        "from the user's objective function or constraints."
      )
    )
  )
  return(result)
}

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

# Class check functions
is_logitr <- function(x) {
  inherits(x, "logitr")
}

isMnlModel <- function(parSetup) {
  return(all(parSetup == "f"))
}

isMxlModel <- function(parSetup) {
  return(!isMnlModel(parSetup))
}

checkMatrix <- function(x) {
  if (!is.matrix(x)) { return(as.matrix(x)) }
  return(x)
}

#' Display version number and date when the package is loaded.
#' @importFrom utils packageDescription
#' @noRd
.onAttach <- function(libname, pkgname) {
  desc  <- utils::packageDescription(pkgname, libname)
  packageStartupMessage(
    "Version:  ", desc$Version, "\n",
    "Author:   ", "John Paul Helveston (George Washington University)", "\n\n",
    "Consider submitting praise at\n",
    "https://github.com/jhelvy/logitr/issues/8.\n\n",
    "Please cite the JSS article in your publications, see:\ncitation(\"logitr\")"
  )
}
