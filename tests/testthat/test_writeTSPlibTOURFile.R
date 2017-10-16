context("{write,read}TSPlibTOURFile")

test_that("writting and reading TOUR files works", {
  instance = generateSimpleTestNetwork()
  n = getNumberOfNodes(instance)
  rndtour = sample(1:n)

  tour.file = "test.TOUR"
  res = writeTSPlibTOURFile(tour.file, name = "testthat tour",
    tour = rndtour, instance = instance)
  expect_logical(res)

  res = readTSPlibTOURFile(tour.file)
  expect_equal(rndtour, res$tour)
  #expect_true(computeTourLength(instance, rndtour) == res$tour.length)
})
