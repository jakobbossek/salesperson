#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// FIXME: add comments
// FIXME: move all the helper methods to separate file(s)
std::vector<double> computeRangeForDimension(const NumericMatrix mat, int dim) {
    std::vector<double> rg(2);
    double min = INFINITY;
    double max = -INFINITY;
    int n_rows = mat.nrow();
    for (int i = 0; i < n_rows; ++i) {
        if (mat(i, dim) < min) {
            min = mat(i, dim);
        }
        if (mat(i, dim) > max) {
            max = mat(i, dim);
        }
    }
    rg[0] = min;
    rg[1] = max;
    return rg;
}

// [[Rcpp::export]]
List getFractionOfPointsNearBoundingBoxCPP(NumericMatrix coords, double distance_fraction) {
    std::vector<double> range_x = computeRangeForDimension(coords, 0);
    std::vector<double> range_y = computeRangeForDimension(coords, 1);

    double distance_x = (range_x[1] - range_x[0]) * distance_fraction;
    double distance_y = (range_y[1] - range_y[0]) * distance_fraction;

    double x_min = range_x[0] + distance_x;
    double y_min = range_y[0] + distance_y;
    double x_max = range_x[1] - distance_x;
    double y_max = range_y[1] - distance_y;

    int n_cities = coords.nrow();

    double n_out_of_bounds = 0;
    for (int i = 0; i < n_cities; ++i) {
        if (coords(i, 0) < x_min || coords(i, 0) > x_max || coords(i, 1) < y_min || coords(i, 1) > y_max) {
            n_out_of_bounds += 1;
        }
    }

    std::string feature_name = "fraction_of_nodes_outside_near_bounding_box_" + std::to_string(distance_fraction);

    return List::create(
        _[feature_name] = NumericVector::create(n_out_of_bounds / n_cities)
    );
}


// feature_bounding_box = function(x, distance_fraction = 0.1) {
//     coords = x$coords
//     ranges = apply(coords, 2, range)
//     distances = diff(ranges)
//     distance_x = distances[1] * distance_fraction
//     distance_y = distances[2] * distance_fraction
//     x_min = ranges[1,1] + distance_x
//     x_max = ranges[2,1] - distance_x
//     y_min = ranges[1,2] + distance_y
//     y_max = ranges[2,2] - distance_y
//     idx = which(coords[, 1] < x_min | coords[, 1] > x_max | coords[, 2] < y_min | coords[, 2] > y_max)
//     if (length(idx) == 0) {
//         idx = 0
//     }
//     res = list(ratio_of_cities_outside_box = length(unique(idx)) / number_of_cities(x))
//     prefix = sprintf("bounding_box_%02i", floor(distance_fraction * 100))
//     names(res) = paste(prefix, names(res), sep = "_")
//     res
// }
