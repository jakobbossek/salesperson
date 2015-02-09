#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getCentroidCoordinatesCPP(const NumericMatrix coords) {
    double sum_x = 0.0, sum_y = 0.0;
    int n_nodes = coords.nrow();
    for (int i = 0; i < n_nodes; ++i) {
        sum_x += coords(0, 1);
        sum_y += coords(0, 2);
    }
    return NumericVector::create(sum_x / n_nodes, sum_y / n_nodes);
}

// [[Rcpp::export]]
NumericVector getDistancesToCentroidCPP(const NumericMatrix coords, NumericVector centroid_coords) {
    unsigned int n_nodes = coords.nrow();
    NumericVector dists(n_nodes);
    for (int i = 0; i < n_nodes; ++i) {
        double dist_x = pow(centroid_coords[0] - coords(i, 0), 2);
        double dist_y = pow(centroid_coords[1] - coords(i, 1), 2);
        dists[i] = sqrt(dist_x + dist_y);
    }
    return dists;
}
