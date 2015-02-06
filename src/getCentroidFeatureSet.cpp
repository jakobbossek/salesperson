#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getCentroidCoordinatesCPP(const NumericMatrix coords, const NumericVector centroid_coords) {
    double sum_x = 0.0, sum_y = 0.0;
    int n_nodes = coords.nrow();
    for (int i = 0; i < n_nodes; ++i) {
        sum_x += coords(0, 1);
        sum_y += coords(0, 2);
    }
    return NumericVector::create(sum_x / n_nodes, sum_y / n_nodes);
}
