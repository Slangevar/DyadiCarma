#include "aux.h"

// [[Rcpp::export]]
arma::mat rcpp_dyad2matrix(List entries, List aentries, int N, int k,
                           std::string type) {
    int d = k * ((1 << N) - 1);
    mat result = zeros(d, d);

    vector<mat> matrices(N);
    for (int i = 0; i < N; i++) {
        matrices[i] = as<mat>(entries[i]);
    }

    if (type == "vert") {
        dyad2mat_helper(matrices, result, N, k, 'v');
    }

    if (type == "horiz") {
        dyad2mat_helper(matrices, result, N, k, 'h');
    }

    if (type == "symm") {
        dyad2mat_helper(matrices, result, N, k, 'v');
        dyad2mat_helper(matrices, result, N, k, 'h');
        result.diag() /= 2;
    }

    if (type == "asymm") {
        dyad2mat_helper(matrices, result, N, k, 'v');
        vector<mat> amatrices(N);
        for (int i = 0; i < N; i++) {
            amatrices[i] = as<mat>(aentries[i]);
        }
        dyad2mat_helper(amatrices, result, N, k, 'h');
    }

    return result;
}