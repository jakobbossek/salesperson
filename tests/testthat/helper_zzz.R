set.seed(1)

# define some helpers

# Generate simple network with three nodes.
#
# Test network geography:
# (0,1)   (1,1)
# o       o
#
#
#
# o       o
# (0,0)   (1,0)
# @return [Network]
generateSimpleTestNetwork = function() {
    x = list()
    x$coordinates = matrix(c(0, 0, 1, 1, 0, 1, 1, 0), ncol = 2, byrow = TRUE)
    x$distance.matrix = dist(x$coordinates)
    class(x) = "Network"
    return(x)
}


# Check that object is feature list
# @param x [any]
#   Object to check.
# @param feature.set [character(1)]
#   Name of the feature set (for debugging).
expect_feature_list = function(x, feature.set) {
    expect_true(is.list(x))
    expect_true(!is.null(attr(x, "time.elapsed")))
    expect_true(is.numeric(attr(x, "time.elapsed")))
    for (i in 1:length(x)) {
        expect_true(is.numeric(x[[i]]),
            info = sprintf("Feature '%s' of feature set '%s' is NOT numeric.",
            names(x)[i], feature.set)
        )
    }
}