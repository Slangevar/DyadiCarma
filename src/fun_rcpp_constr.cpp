// fun_rcpp_constr.cpp
// @title Construction of a list of matrices for \code{Dyadic} object
// @description The function constructs a list of matrices for \code{Dyadic}
// object either with random entries (default) or with entries equal to one.
// @param height positive integer, the number of dyadic levels;
// @param breadth positive integer, the breadth of the dyadic structure;
// @param distr string, if it is one the strings 'binom', 'unif', 'norm' it
// indicate the type of the distribution used for obtaining the entries, any
// other string results in non-random 1's in all entries.
// @param par vector of two numeric values, these are parameters for the
// distributions used to generate the entries.
// @return list of matrices for dyadic matrix.
// @details The function constructs a generic list for \code{Dyadic}-object of
// any type.

#include "fun_aux.h"

// using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_constr(int N, int k, String distr, NumericVector param) {
    List EE(N);
    // Building 'entries'
    for (int i = 0; i < N; i++) {
        int rows = ((1 << (i + 1)) - 1) * k;  // (2^(l+1)-1) * breadth
        int cols = (1 << (N - i - 1)) * k;    // (2^(height-l-1) * breadth
        NumericVector v(rows * cols, 1.0);
        if (distr == "norm") {
            v = rnorm(rows * cols, param[0], param[1]);
        } else {
            if (distr == "binom") {
                v = rbinom(rows * cols, param[0], param[1]);
            } else {
                if (distr == "unif") {
                    v = runif(rows * cols, param[0], param[1]);
                }
            }
        }
        v.attr("dim") = Dimension(rows, cols);
        // NumericMatrix v = as<NumericMatrix>(v);// If one wants v to be the
        // matrix within C++
        EE[i] =
            v;  // Attribute is sufficient if one wants to export a matrix to R
    }
    return (EE);
}
