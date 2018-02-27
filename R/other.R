# ============================================================================
# Other functions
# ============================================================================

# R equivalent of matlab's repmat function
repmat = function(X, m, n) {
    mx = dim(X)[1]
    nx = dim(X)[2]
    return(matrix(t(matrix(X,mx,nx*n)), mx*m, nx*n, byrow=T))
}
