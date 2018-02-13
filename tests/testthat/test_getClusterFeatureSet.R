context("test feature-set: CLUSTER")

test_that("getClusterFeatureSet does produce reasonable results", {
    # "should be" values
    n.cluster.expected = 2L

    # build a simple well clustered network
    x = generateClusteredNetwork(n.points = 100L, n.cluster = n.cluster.expected, upper = 300)

    # build feature set and check structure
    feature.set = getClusterFeatureSet(x, epsilon = 0.1)
    expect_feature_list(feature.set, feature.set = "Cluster")

    # check if values are correct
    expect_equal(feature.set[[1]], n.cluster.expected)

    # now underlying dbscan algorithm should identify only one single cluster
    feature.set = getClusterFeatureSet(x, epsilon = 0.999)
    expect_feature_list(feature.set, feature.set = "Cluster")

    expect_equal(feature.set[[1]], 1L)
})
