#include <Rcpp.h>
using namespace Rcpp;


/* NOTES
 * Fraction of distinct distances is always 1 for non-grid aligned nodes
 */

// Helper for determining the highest count in frequency table
double getMaxFrequency(const std::map<double, int>& table) {
    int max_freq = 0;
    for(std::map<double, int>::const_iterator it = table.begin(); it != table.end(); ++it) {
        if (it->second > max_freq) {
            max_freq = it->second;
        }
    }
    return(max_freq);
}

// Get the set of values which are the modes :-)
std::set<double> getModeValues(const std::map<double, int>& table , int mode_freq) {
    std::set<double> mode_values;
    for (std::map<double, int>::const_iterator it = table.begin(); it != table.end(); ++it) {
        if (it->second == mode_freq) {
            mode_values.insert(it->first);
        }
    }
    return mode_values;
}

std::set<double> getUniqueDistances(const NumericVector x) {
    std::set<double> unique_dists;
    for (int i = 0; i < x.size(); ++i) {
        unique_dists.insert(x[i]);
    }
    return unique_dists;
}

// [[Rcpp::export]]
List getDistanceFeatureSetCPP(const NumericMatrix d, const NumericVector dd) {
    // allocate feature vars
    double fraction_of_distinct_distances = 0.0, fraction_shorter_mean_distance = 0.0, mean_tour_length = 0.0, sum_of_lowest_edge_values = 0.0, mode_mean = 0.0;
    int mode_freq = 0, mode_quantity = 0;

    int n_cities = d.nrow();
    int dd_length = dd.size();

    std::map<double, int> table;

    double dist_sum = 0;
    for (int i = 0; i < dd_length; ++i) {
        dist_sum += dd[i];
        // FIXME: hmm, this could be problematic since comparisson of floats is a non-trivial task
        table[dd[i]]++;
    }
    double mean_dist = dist_sum / dd_length;

    // get shortest lengthes
    NumericVector ddd = clone(dd);
    std::partial_sort(ddd.begin(), ddd.begin() + n_cities, ddd.end());
    sum_of_lowest_edge_values = std::accumulate(ddd.begin(), ddd.begin() + n_cities, 0.0);

    // get unique dists
    std::set<double> unique_dists = getUniqueDistances(dd);
    fraction_of_distinct_distances = (double)unique_dists.size() / (double)dd_length;

    // get distances shorter than mean distance
    fraction_shorter_mean_distance = 0;
    for (int i = 0; i < dd_length; ++i) {
        if (dd[i] < mean_dist) {
            fraction_shorter_mean_distance += 1;
        }
    }
    fraction_shorter_mean_distance /= dd_length;

    // mode features
    mode_freq = getMaxFrequency(table);
    std::set<double> mode_values = getModeValues(table, mode_freq);
    mode_quantity = mode_values.size();
    mode_mean = 0;
    for (std::set<double>::const_iterator it = mode_values.begin(); it != mode_values.end(); it++) {
         mode_mean += *it; // dereference to access element
    }
    mode_mean /= mode_quantity;

    // other features
    mean_tour_length = 2 / (n_cities - 1) * dist_sum;

    List out = List::create(
        NumericVector::create(fraction_shorter_mean_distance),
        NumericVector::create(fraction_of_distinct_distances),
        IntegerVector::create(mode_freq),
        IntegerVector::create(mode_quantity),
        NumericVector::create(mode_mean),
        NumericVector::create(mean_tour_length),
        NumericVector::create(sum_of_lowest_edge_values)
    );

    out.names() = CharacterVector::create(
        "fraction_shorter_mean_distance",
        "fraction_of_distinct_distances",
        "mode_frequency",
        "mode_quantity",
        "mode_mean",
        "mean_tour_length",
        "sum_of_lowest_edge_values"
    );

    return out;
}
