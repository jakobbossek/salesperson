context("test feature-set: BOUNDING-BOX")

test_that("getBoundingBoxFeatureSet does produce reasonable results", {
    # build this simple network object by hand
    x = generateSimpleTestNetwork()
    distance.fraction = 0.1

    # build feature set and check structure
    feature.set = getBoundingBoxFeatureSet(x, distance_fraction = distance.fraction)
    expect_feature_list(feature.set, feature.set = "BoundingBox")

    # fraction of points outside bounding box is 1 for the test network
    expect_equal(feature.set[[1]], 1)
})