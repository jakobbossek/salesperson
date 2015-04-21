#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector getNearestNeighbourDistancesCPP(NumericMatrix dist_mat) {
  int n_nodes = dist_mat.nrow();
  NumericVector nn_dists(n_nodes);

  for (int i = 0; i < n_nodes; ++i) {
    double nn_dist = INFINITY;
    for (int j = 0; j < n_nodes; ++j) {
      if (i == j) {
        continue;
      }
      if (dist_mat(i, j) < nn_dist) {
        nn_dist = dist_mat(i, j);
      }
    }
    nn_dists[i] = nn_dist;
  }
  return nn_dists;
}
