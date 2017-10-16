#include <R.h>
#include <Rinternals.h>

#include "macros.h"

/*
 * @param t [numeric]
 *   Vector of times / iterations.
 * @param f [numeric]
 *   Vector of incumbent values.
 * @param lb [numeric(1)]
 *   Lower bound for tour length. Used for the computation of
 *   the area under the curve.
 * @return [list] Named list. See *names var at the bottom.
 */
SEXP getMonitoringFeatureSetC(SEXP t, SEXP f, SEXP lb) {
  EXTRACT_NUMERIC_VECTOR(t, c_t, n);
  EXTRACT_NUMERIC_VECTOR(f, c_f, n2);
  EXTRACT_REAL(lb, c_lb);

  // R result stuff
  SEXP r_slopes_consecutive = PROTECT(allocVector(REALSXP, n - 1));
  SEXP r_slopes_improvement = PROTECT(allocVector(REALSXP, n - 1));
  SEXP r_plateau_lengths = PROTECT(allocVector(INTSXP, n - 1));
  SEXP r_total_improvement = PROTECT(allocVector(REALSXP, 1));
  SEXP r_max_plateau_length = PROTECT(allocVector(INTSXP, 1));
  SEXP r_n_plateaus = PROTECT(allocVector(INTSXP, 1));
  SEXP r_success_ratio = PROTECT(allocVector(REALSXP, 1));
  SEXP r_vertical_gaps = PROTECT(allocVector(REALSXP, n - 1));
  SEXP r_area_under_curve = PROTECT(allocVector(REALSXP, 1));


  // correpsponding C data structures
  double *slopes_consecutive = REAL(r_slopes_consecutive);
  double *slopes_improvement = REAL(r_slopes_improvement);
  int *plateau_lengths = INTEGER(r_plateau_lengths);
  double *vertical_gaps = REAL(r_vertical_gaps);

  double total_improvement = c_f[0] - c_f[n - 1];
  int n_success = 0;
  int max_plateau_length = 1;
  int cur_plateau_length = 1;
  int n_plateaus = 0;
  int idx_last_improvement = 0;
  double area_under_curve = 0.0;

  // counters for slopes_improvement and plateau_lengths
  int s = 0;
  int p = 0;

  for (int i = 1; i < n; ++i) {
    // initialize
    slopes_improvement[i - 1] = NA_REAL;
    slopes_consecutive[i - 1] = NA_REAL;
    plateau_lengths[i - 1] = NA_INTEGER;

    // count vertical gap
    vertical_gaps[i - 1] = c_f[i] - c_f[i - 1];

    // area under the curve
    double area_width = c_t[i] - c_t[i - 1];
    double area_height = c_f[i - 1] - c_lb;
    area_under_curve += area_width * area_height;

    if (c_f[i] < c_f[i - 1]) {
      // count successful iterations
      ++n_success;

      // slope between consecutive improvements
      double cur_slope = (c_f[i] - c_f[idx_last_improvement]) / (c_t[i] - c_t[idx_last_improvement]);
      slopes_improvement[s++] = cur_slope;
      idx_last_improvement = i;
    }

    // slope between (t_i, f_i) and (t_{i+1}, f_{i+1})
    double cur_slope = (c_f[i] - c_f[i - 1]) / (c_t[i] - c_t[i - 1]);
    slopes_consecutive[i - 1] = cur_slope;

    // check if we are in still in a plateau
    if (c_f[i] == c_f[i - 1]) {
      ++cur_plateau_length;
      // do once a new plateau is detected, i.e., at least to consecutive
      // equal incumbant values occur
      if (cur_plateau_length == 2)
        ++n_plateaus;
      if (cur_plateau_length > max_plateau_length)
        max_plateau_length = cur_plateau_length;
    } else {
      // once a plateau ended, save its length
      if (cur_plateau_length > 1)
        plateau_lengths[p++] = cur_plateau_length;
      // ... and reset
      cur_plateau_length = 1;
    }
  }
  if (cur_plateau_length > 1)
    plateau_lengths[p++] = cur_plateau_length;

  REAL(r_total_improvement)[0] = total_improvement;
  INTEGER(r_n_plateaus)[0] = n_plateaus;
  REAL(r_success_ratio)[0] = (double)n_success / n;
  REAL(r_area_under_curve)[0] = area_under_curve;
  INTEGER(r_max_plateau_length)[0] = max_plateau_length;

  // reserve named R list wrapper
  // See http://adv-r.had.co.nz/C-interface.html for details
  const char *names[] = {
    "slopes_consecutive",
    "slopes_improvement",
    "plateau_lengths",
    "total_improvement",
    "n_plateaus",
    "success_ratio",
    "max_plateau_length",
    "vertical_gaps",
    "area_under_curve",
    ""
  };
  SEXP r_out = PROTECT(mkNamed(VECSXP, names));

  SET_VECTOR_ELT(r_out, 0, r_slopes_consecutive);
  SET_VECTOR_ELT(r_out, 1, r_slopes_improvement);
  SET_VECTOR_ELT(r_out, 2, r_plateau_lengths);
  SET_VECTOR_ELT(r_out, 3, r_total_improvement);
  SET_VECTOR_ELT(r_out, 4, r_n_plateaus);
  SET_VECTOR_ELT(r_out, 5, r_success_ratio);
  SET_VECTOR_ELT(r_out, 6, r_max_plateau_length);
  SET_VECTOR_ELT(r_out, 7, r_vertical_gaps);
  SET_VECTOR_ELT(r_out, 8, r_area_under_curve);

  UNPROTECT(10);
  return(r_out);
}
