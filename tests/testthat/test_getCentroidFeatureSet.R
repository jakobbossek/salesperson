context("test feature-set: CENTROID")

test_that("getCentroidFeatureSet does produce reasonable results", {
    # build this simple network object by hand
    x = generateSimpleTestNetwork()

    feature.set = getCentroidFeatureSet(x)
    expect_feature_list(feature.set, feature.set = "Centroid")

    # check if centroid coordinates are correct
    expect_true((feature.set[["centroid_x"]] - 0.5) < 0.001)
    expect_true((feature.set[["centroid_y"]] - 0.5) < 0.001)
})