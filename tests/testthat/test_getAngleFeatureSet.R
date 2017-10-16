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

    # variation statistics of angle feats should be exactly zero
    fns = !grepl("_cos_", names(angle.feats)) & !grepl("mean|median|min|max|skew", names(angle.feats))
    expect_identical(as.numeric(unlist(angle.feats[fns])), rep(0, 4))

    # location statistics of angle feats should be identical to pi/2
    fns = !grepl("_cos_", names(angle.feats)) & grepl("mean|median|min|max", names(angle.feats))
    expect_identical(as.numeric(unlist(angle.feats[fns])), rep(pi/2, 4))

    # skewness of a problem with identical angles is not computable
    expect_scalar_na(angle.feats$angle_skew)

    # variation statistics of angle cosine feats should be exactly zero
    fns = grepl("_cos_", names(angle.feats)) & !grepl("mean|median|min|max|skew", names(angle.feats))
    expect_identical(as.numeric(unlist(angle.feats[fns])), rep(0, 4))
    
    # location statistics of angle cosine feats should be roughly zero
    fns = grepl("_cos_", names(angle.feats)) & grepl("mean|median|min|max", names(angle.feats))
    expect_equal(as.numeric(unlist(angle.feats[fns])), rep(0, 4))

    # skewness of a problem with identical angles is not computable
    expect_scalar_na(angle.feats$angle_cos_skew)
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

test_that("getAngleFeatureSet generates different outputs for the different feature sets", {
  # build this simple network object by hand
  x = generateSimpleTestNetwork()

  feats = getAngleFeatureSet(x)
  feats1 = getAngleFeatureSet(x, feature.set = "angle")
  feats2 = getAngleFeatureSet(x, feature.set = "cos")
  feats3 = getAngleFeatureSet(x, feature.set = c("cos", "angle"))

  ## compare the feature names
  expect_identical(names(feats), c(names(feats1), names(feats2)))
  expect_true(!all(names(feats1) == names(feats2)))
  expect_identical(sort(names(feats)), sort(names(feats3)))
  
  # now check whether we catch an error if we throw in a wrong feature set name
  expect_error(getAngleFeatureSet(x, feature.set = "hull"))
})
