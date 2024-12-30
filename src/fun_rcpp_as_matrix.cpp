#include "fun_aux.h"

// [[Rcpp::export]]
arma::mat rcpp_as_matrix(List entries, List aentries, int N, int k,
                         std::string type) {
    int d = k * ((1 << N) - 1);
    mat result = zeros(d, d);

    vector<mat> matrices = read_mats(entries, N);

    if (type == "vert") {
        as_matrix_helper(matrices, result, N, k, 'v');
    }

    if (type == "horiz") {
        as_matrix_helper(matrices, result, N, k, 'h');
    }

    if (type == "symm") {
        as_matrix_helper(matrices, result, N, k, 'v');
        as_matrix_helper(matrices, result, N, k, 'h');
        result.diag() /= 2;
    }

    if (type == "asymm") {
        as_matrix_helper(matrices, result, N, k, 'v');
        vector<mat> amatrices = read_mats(aentries, N);
        as_matrix_helper(amatrices, result, N, k, 'h');
    }

    return result;
}