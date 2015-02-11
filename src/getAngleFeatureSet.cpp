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

double computeScalarProduct(std::vector<double> x, std::vector<double> y) {
    double sp = 0.0;
    for (int i = 0; i < x.size(); ++i) {
        sp += x[i] * y[i];
    }
    return sp;
}

double getNormOfVector(std::vector<double> x) {
    double sqsum = 0.0;
    for (int i = 0; i < x.size(); ++i) {
        sqsum += pow(x[i], 2);
    }
    return sqrt(sqsum);
}

double computeShortestAngleBetweenPoints(NumericVector node, NumericVector nn1, NumericVector nn2) {
    std::vector<double> r1(2);
    std::vector<double> r2(2);

    // first we get the two direction vectors
    r1[0] = (nn1[0] - node[0]);
    r1[1] = (nn1[1] - node[1]);
    r2[0] = (nn2[0] - node[0]);
    r2[1] = (nn2[1] - node[1]);

    // compute scalar product
    double sp = computeScalarProduct(r1, r2);

    // compute direction vector lengths //
    double length_r1 = getNormOfVector(r1);
    double length_r2 = getNormOfVector(r2);

    double angle = sp / (length_r1 * length_r2);

    // finally compute angle
    // check for special cases (orthogonality, ...)
    if (angle >= 1.0) {
        return 0.0;
    }
    if (angle <= -1.0) {
        return 3.141592;
    }
    return acos(angle);
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

        angles[i] = computeShortestAngleBetweenPoints(coords(i, _), coords(nn1, _), coords(nn2, _));
    }
    return angles;
}
