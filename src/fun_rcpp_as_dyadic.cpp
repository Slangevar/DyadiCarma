#include "fun_aux.h"

// [[Rcpp::export]]
List rcpp_as_dyadic(arma::mat matrix, int N, int k, std::string type) {
    vector<mat> matrices(N);
    for (int i = 0; i < N; i++) {
        matrices[i] = zeros(((1 << (i + 1)) - 1) * k, (1 << (N - i - 1)) * k);
    }

    if (type == "vert") {
        as_dyadic_helper(matrices, matrix, N, k, 'v');
    }
    if (type == "horiz") {
        as_dyadic_helper(matrices, matrix, N, k, 'h');
    }
    if (type == "symm") {
        as_dyadic_helper(matrices, matrix, N, k, 'v');
        symm_read_helper(matrices, N, k);
    }
    if (type == "asymm") {
        as_dyadic_helper(matrices, matrix, N, k, 'v');
        vector<mat> amatrices(N);
        for (int i = 0; i < N; i++) {
            amatrices[i] =
                zeros(((1 << (i + 1)) - 1) * k, (1 << (N - i - 1)) * k);
        }
        as_dyadic_helper(amatrices, matrix, N, k, 'h');
        asymm_read_helper(matrices, amatrices, N, k);

        List result_list =
            List::create(Named("entries") = wrap_mats(matrices),
                         Named("aentries") = wrap_mats(amatrices));

        return result_list;
    }
    return wrap_mats(matrices);
}