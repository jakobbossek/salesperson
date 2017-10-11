context("test feature-set: CONVEX HULL")

test_that("getConvexHullFeatureSet does produce reasonable results", {
    # build this simple network object by hand
    x = generateSimpleTestNetwork()

    # build feature set and check structure
    feature.set = getConvexHullFeatureSet(x)
    expect_feature_list(feature.set, feature.set = "ConvexHull")

    # all points define the hull
    expect_equal(feature.set$hull_points_ratio, 1)
    expect_equal(feature.set$hull_dists_point_ratio, 1)
    expect_equal(feature.set$hull_dists_min, 0)
    expect_equal(feature.set$hull_dists_mean, 0)
    expect_equal(feature.set$hull_dists_median, 0)
    expect_equal(feature.set$hull_dists_max, 0)
    expect_equal(feature.set$hull_dists_span, 0)
    expect_equal(feature.set$hull_dists_sd, 0)
    expect_equal(feature.set$hull_dists_varcoeff, 0)
    expect_scalar_na(feature.set$hull_dists_skew)

    # hull is a 1 by 1 square
    expect_equal(feature.set$hull_area, 1)
    expect_equal(feature.set$hull_edges_min, 1)
    expect_equal(feature.set$hull_edges_mean, 1)
    expect_equal(feature.set$hull_edges_median, 1)
    expect_equal(feature.set$hull_edges_max, 1)
    expect_equal(feature.set$hull_edges_span, 0)
    expect_equal(feature.set$hull_edges_sd, 0)
    expect_equal(feature.set$hull_edges_varcoeff, 0)
    expect_scalar_na(feature.set$hull_edges_skew)
})