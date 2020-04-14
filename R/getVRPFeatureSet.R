#' Feature: Statistics of distances between obligatory/optional customers and
#' depots.
#'
#' @note Only orienteering instances with two depots are supported at the moment.
#'
#' @template arg_network
#' @template arg_include_costs
#' @template arg_dots
#' @return [\code{list}]
#' @export
getVRPFeatureSet = function(x, include.costs = FALSE, ...) {
  assertClass(x, "Network")
  measureTime(expression({
    dist.mat = as.matrix(x$distance.matrix)
    coordinates = x$coordinates
    depot.coordinates = x$depot.coordinates
    arrival.times = x$arrival.times

    if (is.null(depot.coordinates) || getNumberOfDepots(x) != 2L) {
      stopf("Only orienteering instances with exactly 2 depots are supported at the moment.")
    }

    if (is.null(x$arrival.times)) {
      stopf("There must be dynamic and static customers, but only static customers found.")
    }

    # distance of all custimors to each other
    feat.set = computeStatisticsOnNumericVector(as.numeric(as.dist(x$distance.matrix)), "VRP_customers")

    # get IDs of customers (dynamic and static)
    idx.dynamic = which(arrival.times > 0)
    idx.static = which(arrival.times == 0)

    # compute distances of dynamic/static customers to each other
    coordinates.dynamic = coordinates[idx.dynamic, , drop = FALSE]
    coordinates.static = coordinates[idx.static, , drop = FALSE]
    dists.dynamic = as.numeric(dist(coordinates.dynamic))
    dists.static = as.numeric(dist(coordinates.static))

    # distances of customers to start/end depot
    # ... where start depot is the first depot by convention
    dists.to.start.dynamic = apply(coordinates.dynamic, 1L, function(coord) {
      sqrt(sum(depot.coordinates[1L, ] - coord)^2)
    })

    dists.to.start.static = apply(coordinates.static, 1L, function(coord) {
      sqrt(sum(depot.coordinates[1L, ] - coord)^2)
    })

    # ... and the second depot is the end depot
    dists.to.end.dynamic = apply(coordinates.dynamic, 1L, function(coord) {
      sqrt(sum(depot.coordinates[2L, ] - coord)^2)
    })

    # ... and the second depot is the end depot
    dists.to.end.static = apply(coordinates.static, 1L, function(coord) {
      sqrt(sum(depot.coordinates[2L, ] - coord)^2)
    })

    # distances between optional and dyanmic customers
    dists.dynamic.to.static = as.numeric(dist.mat[idx.dynamic, idx.static])

    distance.between.depots = sqrt(sum(depot.coordinates[1L, ] - depot.coordinates[2L, ])^2)

    feat.set = list(
      "VRP_start_depot_x" = depot.coordinates[1L, 1L],
      "VRP_start_depot_y" = depot.coordinates[1L, 2L],
      "VRP_end_depot_x" = depot.coordinates[2L, 1L],
      "VRP_end_depot_y" = depot.coordinates[2L, 2L],
      "VRP_dynamic_static_ratio" = length(idx.dynamic) / length(idx.static),
      "VRP_distance_between_depots" = distance.between.depots
    )

    feat.set = c(
      feat.set,
      # statistics on distances between all customers
      computeStatisticsOnNumericVector(as.numeric(as.dist(x$distance.matrix)), "VRP_all_customers_distance"),
      # statistics on distances between dyanmic and static customers
      computeStatisticsOnNumericVector(dists.dynamic.to.static, "VRP_dynamic_static_distances"),
      # statistics on distances between dynamic customers only
      computeStatisticsOnNumericVector(dists.dynamic, "VRP_dynamic_distance"),
      # statistics on distances between static customers only
      computeStatisticsOnNumericVector(dists.static, "VRP_static_distance"),
      # statistics on distances between start depot and dynamic customers
      computeStatisticsOnNumericVector(dists.to.start.dynamic, "VRP_dynamic_distance_to_start_depot"),
      # statistics on distances between start depot and static customers
      computeStatisticsOnNumericVector(dists.to.start.static, "VRP_static_distance_to_start_depot"),
      # statistics on distances between end depot and dynamic customers
      computeStatisticsOnNumericVector(dists.to.end.dynamic, "VRP_dynamic_distance_to_end_depot"),
      # statistics on distances between end depot and static customers
      computeStatisticsOnNumericVector(dists.to.end.static, "VRP_static_distance_to_end_depot")
    )

    return(feat.set)
  }), "VRP", include.costs)
}
