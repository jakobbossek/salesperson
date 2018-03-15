context("test feature-set: MONITORING")

test_that("getMonitoringFeatureSet does produce reasonable results [detail]", {
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

  # detailed check of plateus features
  expect_true(all(na.omit(raw$plateau_lengths) %in% c(4, 2)))
  expect_equal(length(na.omit(raw$plateau_lengths)), 2L)
  expect_equal(raw$max_plateau_length, 4) # 9.0 block
  expect_equal(raw$n_plateaus, 2L) # 9.0 block and 7.5 block

  # detailed check of gap features
  expect_length(raw$vertical_gaps, n - 1L)

  # detailed check of slope stuff
  expect_equal(raw$total_improvement, 5.0)

  # detailed check of success ratio
  expect_equal(raw$success_ratio, 0.5)

  # get actual feature set
  feature.set = getMonitoringFeatureSet(times, incumbents)
  expect_feature_list(feature.set, feature.set = "Monitoring")
})

test_that("getMonitoringFeatureSet does produce reasonable results [batch]", {
  # more superficial test -> generate bunch of trajectories and check for consistency
  n.tests = 50L
  max.traj.length = 100L
  max.threshold = 0.7

  for (i in seq_len(n.tests)) {
    # generate simple trajectory
    n = sample(2:max.traj.length, 1L)
    iters = 1:n
    times = cumsum(runif(n))

    incumbents = runif(n)
    # we simmulate stagnation/plateus in the following
    threshold = runif(1, max = max.threshold)
    # now assert that some values are lower or equal to 0 and we get plateaus
    lower.th = which(incumbents < threshold)
    if (length(lower.th) > 0L)
      incumbents[lower.th] = 0
    incumbents = rev(cumsum(incumbents))

    # get raw data first
    raw = getMonitoringFeatureSet(times, incumbents, raw.data = TRUE)

#    catf("%i, %i", n, max(na.omit(raw$plateau_lengths)))

    # debug
    # plot(iters, incumbents, type = "s")
    # print(raw$n_plateaus)
    # Sys.sleep(0.3)

    # get raw data first
    raw = getMonitoringFeatureSet(times, incumbents, raw.data = TRUE)

    # check for reasonable values
    expect_true(length(raw$slopes_consecutive) >= length(na.omit(raw$slopes_improvement)))
    expect_length(raw$slopes_consecutive, n - 1L)

    # detailed check of plateus features
    expect_integer(raw$plateau_lengths, any.missing = TRUE, all.missing = TRUE, lower = 1L, upper = n)
    expect_int(raw$max_plateau_length, lower = 1L)
    expect_int(raw$n_plateaus, lower = 0, upper = n - 1L)
    expect_length(raw$vertical_gaps, n - 1L)
    expect_number(raw$total_improvement, lower = 0)
    expect_number(raw$success_ratio, lower = 0, upper = 1)
  }
})
