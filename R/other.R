# ============================================================================
# Other functions
# ============================================================================

# R equivalent of matlab's repmat function
repmat = function(X, m, n) {
    mx = dim(X)[1]
    nx = dim(X)[2]
    return(matrix(t(matrix(X,mx,nx*n)), mx*m, nx*n, byrow=T))
}

# Replicates matrix mat n times by row
repmatRow = function(mat, n) {
    return(mat[rep(seq(nrow(mat)), n), ])
}

# Replicates each row of matrix mat n times
repmatRowEach = function(mat, n) {
    return(mat[rep(seq(nrow(mat)), each=n), ])
}

# Replicates matrix mat n times by column
repmatCol = function(mat, n) {
    return(mat[ ,rep(seq(ncol(mat)), n)])
}

# Replicates each column of matrix mat n times
repmatColEach = function(mat, n) {
    return(mat[ ,rep(seq(ncol(mat)), each=n)])
}
