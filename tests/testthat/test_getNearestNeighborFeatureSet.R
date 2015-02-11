context("test Feature-set: NEAREST NEIGHBOR")

test_that("getNearestNeighborFeatureSet does produce reasonable results", {
    x = generateSimpleTestNetwork()

    # check the unexposed method which simply returns a vector of distances
    nn.distances = salesperson:::getNearestNeighbourDistancesCPP(as.matrix(x$distance.matrix))
    expect_equal(length(nn.distances), getNumberOfNodes(x))
    expect_true(all(nn.distances == 1))

    # check the exposed method
    nn.feats = getNearestNeighbourFeatureSet(x)
    expect_feature_list(nn.feats, feature.set = "NearestNeighbor")
})