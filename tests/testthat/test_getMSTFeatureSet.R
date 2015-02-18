context("test feature-set: MINIMUM SPANNING TREE")

test_that("getMSTFeatureSet does produce reasonable results", {
    # build this simple network object by hand
    x = generateSimpleTestNetwork()

    # "should be" values
    mst_dists_sum.expected = (getNumberOfNodes(x) - 1L) / sum(x$distance.matrix)

    # build feature set and check structure
    feature.set = getMSTFeatureSet(x)
    expect_feature_list(feature.set, feature.set = "MST")

    # check if values are correct
    expect_equal(feature.set$mst_dists_min, 1)
    expect_equal(feature.set$mst_dists_mean, 1)
    expect_equal(feature.set$mst_dists_median, 1)
    expect_equal(feature.set$mst_dists_max, 1)
    expect_equal(feature.set$mst_dists_span, 0)
    expect_equal(feature.set$mst_dists_sd, 0)
    expect_equal(feature.set$mst_dists_coef_of_var, 0)
    expect_equal(feature.set$mst_dists_sum, mst_dists_sum.expected)
})