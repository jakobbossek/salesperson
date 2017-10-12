context("test feature-set: MONITORING")

test_that("getMonitoringFeatureSet does produce reasonable results", {
  # generate simple trajectory
  n = 10
  iters = 1:n
  times = cumsum(runif(n))
  incumbents = c(10.0, 9.0, 9.0, 9.0, 9.0, 8.0, 7.5, 7.5, 6.4, 5.0)

  # get raw data first
  raw = getMonitoringFeatureSet(times, incumbents, raw.data = TRUE)

  # check for reasonable values
  expect_true(length(raw$slopes_consecutive) > length(na.omit(raw$slopes_improvement)))
  expect_length(raw$slopes_consecutive, n - 1L)
  expect_length(raw$vertical_gaps, n - 1L)
  expect_equal(raw$n_plateaus, 2L)
  expect_equal(length(na.omit(raw$plateau_lengths)), 2L)
  expect_equal(raw$total_improvement, 5.0)

  # get actual feature set
  feature.set = getMonitoringFeatureSet(times, incumbents)
  expect_feature_list(feature.set, feature.set = "Monitoring")
})
