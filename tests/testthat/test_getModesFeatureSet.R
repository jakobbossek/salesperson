context("test feature-set: MODES")

test_that("getModesFeatureSet does produce reasonable results", {
    # "should be" values
    n.cluster.expected = 2L

    # build our simple test network
    x = generateSimpleTestNetwork()

    # build feature set and check structure
    feature.set = getModesFeatureSet(x)
    expect_feature_list(feature.set, feature.set = "Modes")

    expect_true(is.numeric(feature.set[[1]]))
    #FIXME: write a more sophisticated test
})