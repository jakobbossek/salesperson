context("test feature-set: CONVEX HULL")

test_that("getConvexHullFeatureSet does produce reasonable results", {
    # build this simple network object by hand
    x = generateSimpleTestNetwork()

    # "should be" values
    chull_area.expected = 1
    chull_points_on_hull.expected = 1

    # build feature set and check structure
    feature.set = getConvexHullFeatureSet(x)
    expect_feature_list(feature.set, feature.set = "ConvexHull")

    # check if values are correct
    expect_true((feature.set$chull_area - chull_area.expected) < 0.001)
    expect_true((feature.set$chull_points_on_hull - chull_points_on_hull.expected) < 0.001)
})