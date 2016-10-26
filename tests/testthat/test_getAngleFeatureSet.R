context("test Feature-set: ANGLE")

test_that("getAngleFeatureSet does produce reasonable results", {
    # build this simple network object by hand
    x = generateSimpleTestNetwork()

    PI = 3.1415

    angles = salesperson:::getAnglesToNearestNeighborsCPP(x$coordinates, as.matrix(x$distance.matrix))
    # no NaNs
    expect_true(all(!is.nan(angles)))
    # all angles <= PI (since we always return the smaller angle)
    expect_true(all(angles <= PI))

    #FIXME: comparisson of doubles
    angles = round(angles, digits = 2)

    # there is exactly one 90 degrees angle ...
    idx = which(angles == round(PI / 2, digits = 2))

    # and the remaining angles are 45 degree angles
    expect_true(all(angles[-idx] == round(PI / 4, digits = 2)))

    # check the exposed method
    angle.feats = getAngleFeatureSet(x)
    expect_feature_list(angle.feats, feature.set = "Angle")
})

test_that("getAngleFeatureSet does not produce NaN/NA if drop.duplicates = TRUE", {
  coordinates = matrix(c(1, 2, 1, 2, 4, 3, 8, 9), byrow = TRUE, ncol = 2L)

  # first check that NaN/NA is produced if duplicates occur
  x = suppressWarnings(makeNetwork(coordinates))
  feats = getAngleFeatureSet(x)
  expect_true(any(is.na(feats)))

  # now check that this is not the case if duplicated are removed
  feats = suppressWarnings(getAngleFeatureSet(x, drop.duplicates = TRUE))
  expect_true(all(!is.na(feats)))
})
