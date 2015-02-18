context("test feature-set: ALL")

test_that("getFeatureSet does produce reasonable results", {
    # build a simple well clustered network
    x = netgen::generateClusteredNetwork(n.points = 100L, n.cluster = 2L, upper = 300)

    feature.set = getFeatureSet(x)
    expect_feature_list(feature.set, feature.set = "ALL")

    feature.set = getFeatureSet(x, include.costs = TRUE)
    expect_feature_list(feature.set, feature.set = "ALL")

    feature.set = getFeatureSet(x, black.list = c("MST", "Distance"))
    expect_feature_list(feature.set, feature.set = "ALL")

    args = list("Cluster" = list("epsilon" = seq(0.01, 0.1, by = 0.1)))
    feature.set = getFeatureSet(x, feature.fun.args = args)
})