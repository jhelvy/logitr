# Create all starting points for the optimization loop
d$startPoints = matrix(0, ncol=d$numMultiStarts, nrow=d$numParams)
row.names(d$startPoints) = d$allParNames
for (i in 1:d$numMultiStarts) {
    # Check if the user has specified custom bounds
    if (!is.null(d$customLowerBound)) {
        d$startPoints[,i] = getCustomStartPoint(d)
    } else {
        d$startPoints[,i] = getRegularStartPoint(d)
    }
}

# If the user has specified a custom start point, use it for the first run
if (!is.null(d$customStartPoints)) {
    d$startPoints[,1] = d$customStartPoints
}
