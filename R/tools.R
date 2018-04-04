# ============================================================================
# Other functions
# ============================================================================

# R equivalent of matlab's repmat function
repmat <- function(X, m, n) {
    mx <- dim(X)[1]
    nx <- dim(X)[2]
    return(matrix(t(matrix(X,mx,nx*n)), mx*m, nx*n, byrow=T))
}

# Replicates matrix mat n times by row
repmatRow <- function(mat, n) {
    return(mat[rep(seq(nrow(mat)), n), ])
}

# Replicates each row of matrix mat n times
repmatRowEach <- function(mat, n) {
    return(mat[rep(seq(nrow(mat)), each=n), ])
}

# Replicates matrix mat n times by column
repmatCol <- function(mat, n) {
    return(mat[ ,rep(seq(ncol(mat)), n)])
}

# Replicates each column of matrix mat n times
repmatColEach <- function(mat, n) {
    return(mat[ ,rep(seq(ncol(mat)), each=n)])
}

# Converts seconds into hours, minutes, and seconds
# (modified from the gmnl package)
convertTime <- function(time){
    et <- time['elapsed']
    if (et < 1) {
        s <- round(et, 2)
    } else {
        s <- round(et, 0)
    }
    h <- s %/% 3600
    s <- s - 3600*h
    m <- s %/% 60
    s <- s - 60*m
    return(paste(h, "h:", m, "m:", s, "s", sep = ""))
}

# Returns a confidence interval from a vector of data
ci = function(data, alpha=0.025) {
    B = mean(data)
    L = quantile(data, alpha)
    U = quantile(data, 1-alpha)
    ests = c(B,L,U)
    names(ests) = Cs(mean, low, high)
    return(ests)
}
