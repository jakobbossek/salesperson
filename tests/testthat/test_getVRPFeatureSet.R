context("test feature-set: VRP")

test_that("getVRPFeatureSet does produce reasonable results", {
  # build a simple well clustered network
  x = netgen::generateClusteredNetwork(n.points = 100L, n.cluster = 2L, n.depots = 2L, upper = 100)
  x = dynamise(x, n.dynamic = 25L, arrival.limit = 400)

  feature_set = getVRPFeatureSet(x)
  expect_list(feature_set, types = "numeric", names = "named", any.missing = FALSE)
  expect_true(all(feature_set > 0))
})
