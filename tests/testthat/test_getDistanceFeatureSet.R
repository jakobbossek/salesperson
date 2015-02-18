context("test feature-set: DISTANCE")

test_that("getDistanceFeatureSet does produce reasonable results", {
    # build this simple network object by hand
    x = generateSimpleTestNetwork()

    # "should be" values
    mst_dists_sum.expected = (getNumberOfNodes(x) - 1L) / sum(x$distance.matrix)

    # build feature set and check structure
    feature.set = getDistanceFeatureSet(x)
    expect_feature_list(feature.set, feature.set = "DISTANCE")

    # check if values are correct

    # we have 4 times distance 1 and 2 times distance sqrt(2)
    expect_true((feature.set$fraction_of_distinct_distances - 2 / 6) < 0.001)
    expect_true((feature.set$fraction_shorter_mean_distance - 4 / 6) < 0.001)

    # see comment above. Distance 1 is the mode.
    expect_equal(feature.set$mode_quantity, 1L)

    # ... and there are 4 edges with this edge costs
    expect_equal(feature.set$mode_frequency, 4L)

    # Since there is one mode, the value of the mode corresponds to the mode mean
    expect_equal(feature.set$mode_mean, 1)

    # the 4 lowest edge values correspond to the mode
    expect_equal(feature.set$sum_of_lowest_edge_values, 4)
})