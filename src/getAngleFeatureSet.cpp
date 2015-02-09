#include <Rcpp.h>
using namespace Rcpp;

std::vector<int> getTwoNearestNeighborNodes(const unsigned int node_id, const NumericMatrix dist_mat) {
    std::vector<int> nn_nodes(2);
    // partial ordering possible? Maybe faster?
    //std::partial_sort(x.begin(), x.begin() + 2, x.end());
    unsigned int n_nodes = dist_mat.nrow();
    int nn1 = n_nodes + 1, nn2 = n_nodes + 1;
    double nn1_dist = INFINITY, nn2_dist = INFINITY;
    for (int i = 0; i < n_nodes; ++i) {
        if (node_id == i) {
            continue;
        }
        if (dist_mat(node_id, i) < nn1_dist) {
            nn2_dist = nn1_dist;
            nn2 = nn1;
            nn1_dist = dist_mat(node_id, i);
            nn1 = i;
        } else if (dist_mat(node_id, i) < nn2_dist) {
            nn2_dist = dist_mat(node_id, i);
            nn2 = i;
        }
    }
    nn_nodes[0] = nn1;
    nn_nodes[1] = nn2;
    return nn_nodes;
}

double computeShortestAngleBetweenPoints(NumericVector node, NumericVector nn1, NumericVector nn2) {
    // FIXME: add implementation
    return 1.1;
}

// [[Rcpp::export]]
NumericVector getAnglesToNearestNeighborsCPP(NumericMatrix coords, NumericMatrix dist_mat) {
    unsigned int n_nodes = coords.nrow();
    NumericVector angles(n_nodes);

    for (int i = 0; i < n_nodes; ++i) {
        // get two nearest neighbor nodes as vector of integer values
        std::vector<int> nns = getTwoNearestNeighborNodes(i, dist_mat);
        int nn1 = nns[0];
        int nn2 = nns[1];

        // compute angle between them
        // FIXME: Rcpp syntactic sugar: get row of matrix. What type is it? NumericVector?
        angles[i] = computeShortestAngleBetweenPoints(coords(i, _), coords(nn1, _), coords(nn2, _));
    }
    return angles;
}
