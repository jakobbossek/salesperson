context("test feature-set: HULL DISTANCE")

test_that("getHullDistanceFeatureSet does produce reasonable results", {
    # build this simple network object by hand
    x = generateSimpleTestNetwork()

    # build feature set and check structure
    # (as all points of this network are on the hull, return a vector of 0s and NaNs)
    feature.set = getHullDistanceFeatureSet(x)
    expect_list(feature.set, len = 9L)
    expect_equal(unlist(feature.set[!grepl("varcoeff|skew", names(feature.set))], use.names = FALSE), rep(0, 7L))
    expect_equal(unlist(feature.set[grepl("varcoeff|skew", names(feature.set))], use.names = FALSE), rep(NaN, 2L))


    # build a simple well clustered network
    x = netgen::generateClusteredNetwork(n.points = 100L, n.cluster = 4L)

    # build feature set and check structure
    # (all points are located within [0, 1]^2 and therefore the statistics should be at most 1)
    feature.set = getHullDistanceFeatureSet(x)
    expect_list(feature.set, len = 9L)
    expect_numeric(unlist(feature.set, use.names = FALSE), lower = 0, upper = 1, len = 9L)
    
    # given that at least 1 point has to be on the hull the minimum distance should be zero
    expect_identical(feature.set$hulldist_min, 0)
})
