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

#include <Rcpp.h>
// #include <math.h> // Included to get `pow(,)' function (as it turns out it is
// not needed) #include <stdio.h>
using namespace Rcpp;

// [[Rcpp::export]]
List rcpp_constr(int height, int breadth, String distr, NumericVector param) {
    List EE(height);
    // Building 'entries'n
    for (int l = 0; l < height; l++) {
        int rows = (pow(2, l + 1) - 1) * breadth;  // (2^(l+1)-1) * breadth
        int cols =
            pow(2, height - l - 1) * breadth;  // (2^(height-l-1) * breadth
        NumericVector v(rows * cols, 1.0);
        if (distr == "norm") {
            v = rnorm(rows * cols, param[0], param[1]);
        } else {
            if (distr == "binom") {
                v = rbinom(rows * cols, param[0], param[1]);
                ;
            } else {
                if (distr == "unif") {
                    v = runif(rows * cols, param[0], param[1]);
                }
            }
        }
        v.attr("dim") = Dimension(rows, cols);
        // NumericMatrix v = as<NumericMatrix>(v);// If one wants v to be the
        // matrix within C++
        EE[l] =
            v;  // Attribute is sufficient if one wants to export a matrix to R
    }
    return (EE);
}
