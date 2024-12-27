#include "fun_aux.h"

vector<mat> read_mats(List entries, int N) {
    vector<mat> matrices(N);
    for (int i = 0; i < N; i++) {
        matrices[i] = as<mat>(entries[i]);
    }

    return matrices;
}

vector<mat> init_mats(vector<mat> matrices, int N) {
    vector<mat> new_matrices(N);
    for (int i = 0; i < N; i++) {
        new_matrices[i] = zeros(size(matrices[i]));
    }

    return new_matrices;
}

List wrap_mats(vector<mat> matrices) {
    List wrap_list;
    for (const auto& mat : matrices) {
        wrap_list.push_back(as<NumericMatrix>(wrap(mat)));
    }

    return wrap_list;
}

void multiply_hv_core(vector<mat> l_matrices, vector<mat> r_matrices,
                      vector<mat>& symm_matrices, vector<mat>& asymm_matrices,
                      int N, int k) {
    // The divide and conquer for the right-hand side matrix will lead to the
    // symm part. For the left-hand side, it will be the asymm part.

    // It utilizes the fact that A B = (B^T A^T)^T and (A B)_{m j} = \Sum A_{m
    // .} B_{. j}

    for (int i = N - 1; i >= 0; i--) {
        // i is the level, j is the number of width-k columns
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            // j is the column id for this specific column in level i
            queue<int> cen_queue;
            queue<int> col_queue;
            // This is the center id of that column
            cen_queue.push((1 << i) - 1);
            col_queue.push(j);
            for (int m = i; m >= 0; m--) {
                // The number of columns of A to deal with at level m.
                for (int s = 0; s < (1 << (i - m)); s++) {
                    int cen_id = cen_queue.front();
                    cen_queue.pop();
                    int col_id = col_queue.front();
                    col_queue.pop();
                    if (m != 0) {
                        cen_queue.push(cen_id - (1 << (m - 1)));
                        cen_queue.push(cen_id + (1 << (m - 1)));
                        col_queue.push(
                            col_id *
                            2);  // Once the level goes down, the corresponding
                                 // index for the col/row will be doubled.
                        col_queue.push(col_id * 2 + 1);
                    }

                    symm_matrices[i].submat(cen_id * k, j * k,
                                            (cen_id + 1) * k - 1,
                                            (j + 1) * k - 1) +=
                        l_matrices[m]
                            .cols(col_id * k, (col_id + 1) * k - 1)
                            .t() *  // It is actually the rows in the HD matrix
                        r_matrices[i].submat((cen_id - (1 << m) + 1) * k, j * k,
                                             (cen_id + (1 << m)) * k - 1,
                                             (j + 1) * k - 1);

                    if (m != i) {
                        asymm_matrices[i].submat(cen_id * k, j * k,
                                                 (cen_id + 1) * k - 1,
                                                 (j + 1) * k - 1) +=
                            r_matrices[m]
                                .cols(col_id * k, (col_id + 1) * k - 1)
                                .t() *
                            l_matrices[i].submat(
                                (cen_id - (1 << m) + 1) * k, j * k,
                                (cen_id + (1 << m)) * k - 1, (j + 1) * k - 1);
                    }
                }
            }
        }
    }
}
void multiply_vv_core(vector<mat> l_matrices, vector<mat> r_matrices,
                      vector<mat>& result_matrices, int N, int k) {
    // The algorithm uses the property that
    // (A B)_{. j} = \Sum_{m} A_{. m} * B_{m j}
    for (int i = N - 1; i >= 0; i--) {
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            // j is the column id for this specific column in level i
            queue<int> cen_queue;
            queue<int> col_queue;
            // This is the center id of that column
            cen_queue.push((1 << i) - 1);
            col_queue.push(j);
            for (int m = i; m >= 0; m--) {
                // The number of columns of A to deal with at level m.
                for (int s = 0; s < (1 << (i - m)); s++) {
                    int cen_id = cen_queue.front();
                    cen_queue.pop();
                    int col_id = col_queue.front();
                    col_queue.pop();
                    if (m != 0) {
                        cen_queue.push(cen_id - (1 << (m - 1)));
                        cen_queue.push(cen_id + (1 << (m - 1)));
                        col_queue.push(col_id * 2);
                        col_queue.push(col_id * 2 + 1);
                    }

                    result_matrices[i].submat(
                        (cen_id - (1 << m) + 1) * k, j * k,
                        (cen_id + (1 << m)) * k - 1, (j + 1) * k - 1) +=
                        l_matrices[m].cols(col_id * k, (col_id + 1) * k - 1) *
                        r_matrices[i].submat(cen_id * k, j * k,
                                             (cen_id + 1) * k - 1,
                                             (j + 1) * k - 1);
                }
            }
        }
    }
}

void multiply_vh_core(vector<mat> l_matrices, vector<mat> r_matrices,
                      arma::mat& result, int N, int k) {
    queue<vector<int>>
        cen_queue;  // This is for the center index of each row in HD.
    vector<int> cur_cen;
    cur_cen.push_back((1 << (N - 1)) - 1);
    cen_queue.push(cur_cen);
    queue<vector<int>> col_queue;
    vector<int> cur_col;
    cur_col.push_back(0);
    col_queue.push(cur_col);  // This is for the column index for the given
                              // submatrix of certain levels. For example, for
                              // the first item in one of the vectors in the
                              // queue, it should always be 0 since the matrix
                              // for the highest level of the dyadic structure
                              // only has one column (or row). Notice that this
                              // is also highly related to the row index
    // Since for HD matrix, we have more than 1

    for (int i = N - 1; i >= 0; i--) {
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            vector<int> cur_cen = cen_queue.front();
            cen_queue.pop();
            vector<int> cur_col = col_queue.front();
            col_queue.pop();

            if (i != 0) {
                vector<int> next_cen_1;
                vector<int> next_cen_2;
                for (int m = 0; m < N - i; m++) {
                    next_cen_1.push_back(cur_cen[m] - (1 << (i - 1)));
                    next_cen_2.push_back(cur_cen[m] + (1 << (i - 1)));
                };
                next_cen_1.push_back((1 << (i - 1)) - 1);
                next_cen_2.push_back((1 << (i - 1)) - 1);
                cen_queue.push(next_cen_1);
                cen_queue.push(next_cen_2);

                vector<int> next_col_1(cur_col);
                vector<int> next_col_2(cur_col);
                next_col_1.push_back(cur_col[N - i - 1] * 2);
                next_col_2.push_back(cur_col[N - i - 1] * 2 + 1);
                col_queue.push(next_col_1);
                col_queue.push(next_col_2);
            }
            int res_col = cur_cen[0];
            for (int m = 0; m < N - i; m++) {
                int cen_id = cur_cen[m];
                int col_id = cur_col[m];
                int res_cen_row = (1 << (N - m)) * col_id + (1 << (N - m - 1)) -
                                  1;  // This should be related to the
                                      // corresponding rows of the VD matrix.
                int res_upp_row = res_cen_row - (1 << (N - m - 1)) + 1;
                int res_low_row = res_cen_row + (1 << (N - m - 1));
                result.submat(res_upp_row * k, res_col * k, res_low_row * k - 1,
                              (res_col + 1) * k - 1) +=
                    l_matrices[N - m - 1].cols(col_id * k,
                                               (col_id + 1) * k - 1) *
                    r_matrices[N - m - 1]
                        .submat(cen_id * k, col_id * k, (cen_id + 1) * k - 1,
                                (col_id + 1) * k - 1)
                        .t();
            }
        }
    }
}

void symm_arith_helper(vector<mat>& matrices, int N, int k) {
    // Help arithmetic operations like multiplications and additions.
    for (int i = 0; i < N; i++) {
        int row_id = ((1 << i) - 1) * k;
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            matrices[i]
                .submat(row_id, j * k, row_id + k - 1, (j + 1) * k - 1)
                .diag() /= 2;
        }
    }
}

void symm_convert(std::vector<arma::mat>& matrices, int N, int k) {
    // Complete the whole symmetric central block
    for (int i = 0; i < N; i++) {
        int row_id = ((1 << i) - 1) * k;
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            matrices[i].submat(row_id, j * k, row_id + k - 1,
                               (j + 1) * k - 1) +=
                trimatu(matrices[i].submat(row_id, j * k, row_id + k - 1,
                                           (j + 1) * k - 1),
                        1)
                    .t();
        }
    }
}

void symm_read_helper(std::vector<arma::mat>& matrices, int N, int k) {
    // Extract only the upper diagonal terms from the central block of a
    // symmetrically dyadic matrix.
    for (int i = 0; i < N; i++) {
        int row_id = ((1 << i) - 1) * k;
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            matrices[i].submat(row_id, j * k, row_id + k - 1, (j + 1) * k - 1) =
                trimatu(matrices[i].submat(row_id, j * k, row_id + k - 1,
                                           (j + 1) * k - 1));
        }
    }
}

void asymm_convert(vector<mat>& matrices, vector<mat>& amatrices, int N,
                   int k) {
    // Clean up the result of multiplication to fit in the format of the asymm
    // Dyadic.
    for (int i = 0; i < N; i++) {
        int row_id = ((1 << i) - 1) * k;
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            matrices[i].submat(row_id, j * k, row_id + k - 1,
                               (j + 1) * k - 1) +=
                amatrices[i]
                    .submat(row_id, j * k, row_id + k - 1, (j + 1) * k - 1)
                    .t();
            amatrices[i].submat(row_id, j * k, row_id + k - 1,
                                (j + 1) * k - 1) =
                trimatl(matrices[i].submat(row_id, j * k, row_id + k - 1,
                                           (j + 1) * k - 1),
                        -1)
                    .t();
            matrices[i].submat(row_id, j * k, row_id + k - 1, (j + 1) * k - 1) =
                trimatu(matrices[i].submat(row_id, j * k, row_id + k - 1,
                                           (j + 1) * k - 1));
            // trimatl(A, -1)
        }
    }
}

void asymm_read_helper(std::vector<arma::mat>& matrices,
                       std::vector<arma::mat>& amatrices, int N, int k) {
    for (int i = 0; i < N; i++) {
        int row_id = ((1 << i) - 1) * k;
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            matrices[i].submat(row_id, j * k, row_id + k - 1, (j + 1) * k - 1) =
                trimatu(matrices[i].submat(row_id, j * k, row_id + k - 1,
                                           (j + 1) * k - 1));
            amatrices[i].submat(row_id, j * k, row_id + k - 1,
                                (j + 1) * k - 1) =
                trimatu(amatrices[i].submat(row_id, j * k, row_id + k - 1,
                                            (j + 1) * k - 1),
                        1);
        }
    }
}

void as_matrix_helper(vector<mat> matrices, mat& result, int N, int k,
                      char fmt = 'v') {
    for (int i = 0; i < N; i++) {
        int col_length = ((1 << (i + 1)) - 1) * k;
        int col_id = ((1 << i) - 1) * k;
        int row_id = 0;
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            if (fmt == 'v') {
                result.submat(row_id, col_id, row_id + col_length - 1,
                              col_id + k - 1) +=
                    matrices[i].cols(j * k, (j + 1) * k - 1);
            } else if (fmt == 'h') {
                result.submat(col_id, row_id, col_id + k - 1,
                              row_id + col_length - 1) +=
                    matrices[i].cols(j * k, (j + 1) * k - 1).t();
            }
            col_id += (1 << (i + 1)) * k;
            row_id += (1 << (i + 1)) * k;

            // For the first level, it is just every two
            // column, for the second, every four column, etc.
        }
    }
}

void as_dyadic_helper(vector<mat>& matrices, mat matrix, int N, int k,
                      char fmt = 'v') {
    // Notice that I assume zero input matrices here! This is quite different
    // from the as_matrix_helper since the latter is used in other
    // multiplication algorithms.
    // #pragma omp parallel for
    for (int i = 0; i < N; i++) {
        int col_length = ((1 << (i + 1)) - 1) * k;
        int col_id = ((1 << i) - 1) * k;
        int row_id = 0;
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            if (fmt == 'v') {
                matrices[i].cols(j * k, (j + 1) * k - 1) = matrix.submat(
                    row_id, col_id, row_id + col_length - 1, col_id + k - 1);
            } else if (fmt == 'h') {
                matrices[i].cols(j * k, (j + 1) * k - 1) =
                    matrix
                        .submat(col_id, row_id, col_id + k - 1,
                                row_id + col_length - 1)
                        .t();
            }
            col_id += (1 << (i + 1)) * k;
            row_id += (1 << (i + 1)) * k;
        }
    }
}

// [[Rcpp::export]]
List add_helper(List l_entries, List l_aentries, List r_entries,
                List r_aentries, std::string type1, std::string type2, int N,
                int k) {
    vector<mat> l_matrices = read_mats(l_entries, N);
    vector<mat> r_matrices = read_mats(r_entries, N);
    vector<mat> matrices;
    vector<mat> amatrices;

    if (type1 == type2) {
        matrices = l_matrices;
        for (int i = 0; i < N; i++) {
            matrices[i] += r_matrices[i];
        }

        if (type1 == "asymm") {
            vector<mat> l_amatrices = read_mats(l_aentries, N);
            vector<mat> r_amatrices = read_mats(r_aentries, N);
            amatrices = l_amatrices;
            for (int i = 0; i < N; i++) {
                amatrices[i] += r_amatrices[i];
            }
        }
    } else {
        if (type1 == "vert" && type2 == "horiz") {
            matrices = l_matrices;
            amatrices = r_matrices;
        } else if (type1 == "vert" && type2 == "symm") {
            symm_arith_helper(r_matrices, N, k);
            matrices = l_matrices;
            for (int i = 0; i < N; i++) {
                matrices[i] += r_matrices[i];
            }
            amatrices = r_matrices;
        } else if (type1 == "vert" && type2 == "asymm") {
            matrices = l_matrices;
            amatrices = read_mats(r_aentries, N);
            for (int i = 0; i < N; i++) {
                matrices[i] += r_matrices[i];
            }
        } else if (type1 == "horiz" && type2 == "symm") {
            symm_arith_helper(r_matrices, N, k);
            matrices = r_matrices;
            amatrices = l_matrices;
            for (int i = 0; i < N; i++) {
                amatrices[i] += r_matrices[i];
            }
        } else if (type1 == "horiz" && type2 == "asymm") {
            matrices = r_matrices;
            amatrices = read_mats(r_aentries, N);
            for (int i = 0; i < N; i++) {
                amatrices[i] += l_matrices[i];
            }
        } else if (type1 == "symm" && type2 == "asymm") {
            symm_arith_helper(l_matrices, N, k);
            matrices = l_matrices;
            amatrices = read_mats(r_aentries, N);
            for (int i = 0; i < N; i++) {
                matrices[i] += r_matrices[i];
                amatrices[i] += l_matrices[i];
            }
        } else {
            return add_helper(r_entries, r_aentries, l_entries, l_aentries,
                              type2, type1, N, k);
        }

        asymm_convert(matrices, amatrices, N, k);
    }
    List result_list = List::create(Named("entries") = wrap_mats(matrices),
                                    Named("aentries") = wrap_mats(amatrices));

    return result_list;
}

// [[Rcpp::export]]
List multiply_hv(List l_entries, List r_entries, int N, int k) {
    // HD on the left and VD on the right
    vector<mat> l_matrices = read_mats(l_entries, N);
    vector<mat> r_matrices = read_mats(r_entries, N);

    vector<mat> symm_matrices = init_mats(l_matrices, N);
    vector<mat> asymm_matrices = init_mats(r_matrices, N);

    multiply_hv_core(l_matrices, r_matrices, symm_matrices, asymm_matrices, N,
                     k);

    asymm_convert(symm_matrices, asymm_matrices, N, k);

    List result_list =
        List::create(Named("entries") = wrap_mats(symm_matrices),
                     Named("aentries") = wrap_mats(asymm_matrices));

    return result_list;
}

// [[Rcpp::export]]
List multiply_vv(List l_entries, List r_entries, int N, int k) {
    vector<mat> l_matrices = read_mats(l_entries, N);
    vector<mat> r_matrices = read_mats(r_entries, N);

    vector<mat> result_matrices = init_mats(l_matrices, N);

    multiply_vv_core(l_matrices, r_matrices, result_matrices, N, k);

    return wrap_mats(result_matrices);
}

// [[Rcpp::export]]
List multiply_hasv(List l_entries, List l_aentries, List r_entries, int N,
                   int k, char type) {
    // Only two types, 'v' and 'h'
    vector<mat> l_matrices = read_mats(l_entries, N);
    vector<mat> l_amatrices = read_mats(l_aentries, N);
    vector<mat> r_matrices = read_mats(r_entries, N);

    vector<mat> symm_matrices = init_mats(l_matrices, N);
    vector<mat> asymm_matrices = init_mats(r_matrices, N);

    if (type == 'v') {
        multiply_vv_core(l_matrices, r_matrices, symm_matrices, N, k);
        multiply_hv_core(l_amatrices, r_matrices, symm_matrices, asymm_matrices,
                         N, k);
    } else {
        multiply_hv_core(r_matrices, l_matrices, symm_matrices, asymm_matrices,
                         N, k);
        multiply_vv_core(l_amatrices, r_matrices, asymm_matrices, N, k);
    }

    asymm_convert(symm_matrices, asymm_matrices, N, k);

    // Needed. Otherwise the matrices in the list will be of wrong shape after
    // returned to R.
    List result_list =
        List::create(Named("entries") = wrap_mats(symm_matrices),
                     Named("aentries") = wrap_mats(asymm_matrices));

    return result_list;
}

// [[Rcpp::export]]
List multiply_hsv(List l_entries, List r_entries, int N, int k, char type) {
    // Only two types, 'v' and 'h'.
    vector<mat> l_matrices = read_mats(l_entries, N);
    vector<mat> r_matrices = read_mats(r_entries, N);

    vector<mat> symm_matrices = init_mats(l_matrices, N);
    vector<mat> asymm_matrices = init_mats(r_matrices, N);

    if (type == 'v') {
        symm_arith_helper(l_matrices, N, k);
        multiply_vv_core(l_matrices, r_matrices, symm_matrices, N, k);
        multiply_hv_core(l_matrices, r_matrices, symm_matrices, asymm_matrices,
                         N, k);
    } else {
        symm_arith_helper(r_matrices, N, k);
        multiply_hv_core(l_matrices, r_matrices, symm_matrices, asymm_matrices,
                         N, k);
        multiply_vv_core(r_matrices, l_matrices, asymm_matrices, N,
                         k);  // The transpose!
    }

    asymm_convert(symm_matrices, asymm_matrices, N, k);

    // Needed. Otherwise the matrices in the list will be of wrong shape after
    // returned to R.

    List result_list =
        List::create(Named("entries") = wrap_mats(symm_matrices),
                     Named("aentries") = wrap_mats(asymm_matrices));

    return result_list;
}

// [[Rcpp::export]]
arma::mat multiply_vh(List l_entries, List r_entries, int N, int k) {
    int d = k * ((1 << N) - 1);
    mat result = zeros(d, d);

    vector<mat> l_matrices = read_mats(l_entries, N);
    vector<mat> r_matrices = read_mats(r_entries, N);

    multiply_vh_core(l_matrices, r_matrices, result, N, k);

    return result;
}

// [[Rcpp::export]]
arma::mat multiply_vsh(List l_entries, List r_entries, int N, int k,
                       char type) {
    // Change the diagonal to zero here since it is easier to manipulate. So the
    // vertical part of the SD matrix only has the upper-triangular terms. Only
    // two types, 'v' and 'h'
    vector<mat> l_matrices = read_mats(l_entries, N);
    vector<mat> r_matrices = read_mats(r_entries, N);

    vector<mat> result_matrices = init_mats(l_matrices, N);

    int d = k * ((1 << N) - 1);
    mat result = zeros(d, d);

    if (type == 'v') {
        symm_arith_helper(r_matrices, N, k);
        multiply_vh_core(l_matrices, r_matrices, result, N, k);
        multiply_vv_core(l_matrices, r_matrices, result_matrices, N, k);
        as_matrix_helper(result_matrices, result, N, k, 'v');
    } else {
        symm_arith_helper(l_matrices, N, k);
        multiply_vh_core(l_matrices, r_matrices, result, N, k);
        multiply_vv_core(r_matrices, l_matrices, result_matrices, N, k);
        as_matrix_helper(result_matrices, result, N, k, 'h');
    }

    return result;
}

// [[Rcpp::export]]
arma::mat multiply_vash(List l_entries, List r_entries, List r_aentries, int N,
                        int k, char type) {
    // Change the diagonal to zero here since it is easier to manipulate. So the
    // vertical part of the SD matrix only has the upper-triangular terms. Only
    // two types, 'v' and 'h'
    vector<mat> l_matrices = read_mats(l_entries, N);
    vector<mat> r_matrices = read_mats(r_entries, N);
    vector<mat> r_amatrices = read_mats(r_aentries, N);

    vector<mat> result_matrices = init_mats(l_matrices, N);

    int d = k * ((1 << N) - 1);
    mat result = zeros(d, d);

    if (type == 'v') {
        multiply_vh_core(l_matrices, r_amatrices, result, N, k);
        multiply_vv_core(l_matrices, r_matrices, result_matrices, N, k);
        as_matrix_helper(result_matrices, result, N, k, 'v');
    } else {
        multiply_vh_core(r_matrices, l_matrices, result, N, k);
        multiply_vv_core(l_matrices, r_amatrices, result_matrices, N, k);
        as_matrix_helper(result_matrices, result, N, k, 'h');
    }

    return result;
}

// [[Rcpp::export]]
arma::mat multiply_sas(List l_entries, List l_aentries, List r_entries,
                       List r_aentries, int N, int k) {
    vector<mat> l_matrices = read_mats(l_entries, N);
    vector<mat> r_matrices = read_mats(r_entries, N);
    vector<mat> symm_matrices = init_mats(l_matrices, N);
    vector<mat> asymm_matrices = init_mats(r_matrices, N);

    int d = k * ((1 << N) - 1);
    mat result = zeros(d, d);
    if (l_aentries.length() == 0 && r_aentries.length() == 0) {
        symm_arith_helper(l_matrices, N, k);
        symm_arith_helper(r_matrices, N, k);
        multiply_vh_core(l_matrices, r_matrices, result, N, k);
        multiply_vv_core(l_matrices, r_matrices, symm_matrices, N, k);
        multiply_hv_core(l_matrices, r_matrices, symm_matrices, asymm_matrices,
                         N, k);
        multiply_vv_core(r_matrices, l_matrices, asymm_matrices, N, k);

        as_matrix_helper(symm_matrices, result, N, k, 'v');
        as_matrix_helper(asymm_matrices, result, N, k, 'h');
    } else if (l_aentries.length() == 0) {
        vector<mat> r_amatrices = read_mats(r_aentries, N);
        symm_arith_helper(l_matrices, N, k);
        multiply_vh_core(l_matrices, r_amatrices, result, N, k);
        multiply_vv_core(l_matrices, r_matrices, symm_matrices, N, k);
        multiply_hv_core(l_matrices, r_matrices, symm_matrices, asymm_matrices,
                         N, k);
        multiply_vv_core(r_amatrices, l_matrices, asymm_matrices, N, k);

        as_matrix_helper(symm_matrices, result, N, k, 'v');
        as_matrix_helper(asymm_matrices, result, N, k, 'h');
    } else if (r_aentries.length() == 0) {
        vector<mat> l_amatrices = read_mats(l_aentries, N);
        symm_arith_helper(r_matrices, N, k);
        multiply_vh_core(l_matrices, r_matrices, result, N, k);
        multiply_vv_core(l_matrices, r_matrices, symm_matrices, N, k);
        multiply_hv_core(l_amatrices, r_matrices, symm_matrices, asymm_matrices,
                         N, k);
        multiply_vv_core(r_matrices, l_amatrices, asymm_matrices, N, k);

        as_matrix_helper(symm_matrices, result, N, k, 'v');
        as_matrix_helper(asymm_matrices, result, N, k, 'h');
    } else {
        vector<mat> l_amatrices = read_mats(l_aentries, N);
        vector<mat> r_amatrices = read_mats(r_aentries, N);
        multiply_vh_core(l_matrices, r_amatrices, result, N, k);
        multiply_vv_core(l_matrices, r_matrices, symm_matrices, N, k);
        multiply_hv_core(l_amatrices, r_matrices, symm_matrices, asymm_matrices,
                         N, k);
        multiply_vv_core(r_amatrices, l_amatrices, asymm_matrices, N, k);

        as_matrix_helper(symm_matrices, result, N, k, 'v');
        as_matrix_helper(asymm_matrices, result, N, k, 'h');
    }
    return result;
}

// [[Rcpp::export]]
List asymm_trans(List entries, List aentries, int N, int k) {
    // Transpose of the asymmetrically dyadic matrix.
    vector<mat> matrices(N);
    vector<mat> amatrices(N);

    for (int i = 0; i < N; i++) {
        matrices[i] = as<mat>(entries[i]);
        amatrices[i] = as<mat>(aentries[i]);
    }

    for (int i = 0; i < N; i++) {
        int row_id = ((1 << i) - 1) * k;
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            amatrices[i].submat(row_id, j * k, row_id + k - 1,
                                (j + 1) * k - 1) +=
                diagmat(matrices[i].submat(row_id, j * k, row_id + k - 1,
                                           (j + 1) * k - 1));
            matrices[i]
                .submat(row_id, j * k, row_id + k - 1, (j + 1) * k - 1)
                .diag()
                .zeros();
        }
    }

    vector<NumericMatrix> symm_list;
    vector<NumericMatrix> asymm_list;

    for (const auto& mat : amatrices) {
        symm_list.push_back(as<NumericMatrix>(wrap(mat)));
    }
    for (const auto& mat : matrices) {
        asymm_list.push_back(as<NumericMatrix>(wrap(mat)));
    }

    List result_list = List::create(Named("entries") = wrap(symm_list),
                                    Named("aentries") = wrap(asymm_list));

    return result_list;
}

void block_gram_schmidt(mat h, subview<double> p) {
    if (!(h.n_rows == p.n_rows && h.n_cols == p.n_cols)) {
        throw invalid_argument("The two matrices must be of the same shape!");
    }
    if (h.n_rows != h.n_cols) {
        throw invalid_argument("Only square matrices are supported!");
    }

    try {
        p.diag() += 1;
        int d = h.n_rows;
        for (int i = 0; i < d - 1; i++) {
            double sq = sqrt(h(i, i));
            p.col(i).subvec(0, i) /= sq;
            h.row(i).subvec(i + 1, d - 1) /= sq;
            p.submat(0, i + 1, i, d - 1) -=
                p.col(i).subvec(0, i) * h.row(i).subvec(i + 1, d - 1);
            h.submat(i + 1, i + 1, d - 1, d - 1) -=
                h.row(i).subvec(i + 1, d - 1).t() *
                h.row(i).subvec(i + 1, d - 1);
        }
        p.col(d - 1) /= sqrt(h(d - 1, d - 1));
    } catch (const std::exception& e) {
        Rcpp::Rcout << e.what() << std::endl;
    }
}
