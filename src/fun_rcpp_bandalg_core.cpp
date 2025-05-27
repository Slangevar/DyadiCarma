#include "fun_aux.h"

// [[Rcpp::export]]
List rcpp_bandalg_core(List entries, int N, int k) {
    // The input is a symmetric band matrix H in a dyadic representation, and
    // the output is a vertically dyadic matrix P where P^T H P = I.

    vector<mat> matrices = read_mats(entries, N);
    vector<mat> p_matrices = init_mats(matrices, N);
    vector<mat> tmp_matrices = init_mats(matrices, N);

    // To complete the diagonal block.
    symm_convert(matrices, N, k);

    // GS for the first layer
    for (int i = 0; i < (1 << (N - 1)); i++) {
        block_gram_schmidt(
            matrices[0].submat(0, i * k, k - 1, (i + 1) * k - 1),
            p_matrices[0].submat(0, i * k, k - 1, (i + 1) * k - 1));
    }

    // The main part of the dyadic algorithm.
    for (int i = 1; i < N; i++) {
        // For calculating P^T \check \Sigma, we can still make use of the
        // property that (A B)_{. j} = \sum_m A_{. m} B_{m j}
        // This is for the number of columns in that layer

        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            // For band matrices, we don't need queues for cen_id.
            // Creating \check \Sigma^\prime. For band matrices we only
            // care about the central block (the closest neighbor).

            int cen_st_id = (1 << i) - 1;  // The starting id, the real center.
            int left_cen = cen_st_id - 1;
            int right_cen = cen_st_id + 1;
            int left_col = j * 2;
            int right_col = j * 2 + 1;

            for (int m = i - 1; m >= 0; m--) {
                int p_left_id = (1 << (m + 1)) - 2;
                int p_right_id = 0;
                int tmp_left_id = cen_st_id - (1 << m);
                int tmp_right_id = cen_st_id + (1 << m);

                tmp_matrices[i].submat(tmp_left_id * k, j * k,
                                       (tmp_left_id + 1) * k - 1,
                                       (j + 1) * k - 1) =
                    p_matrices[m]
                        .submat(p_left_id * k, left_col * k,
                                (p_left_id + 1) * k - 1, (left_col + 1) * k - 1)
                        .t() *
                    matrices[i].submat(left_cen * k, j * k,
                                       (left_cen + 1) * k - 1, (j + 1) * k - 1);

                tmp_matrices[i].submat(tmp_right_id * k, j * k,
                                       (tmp_right_id + 1) * k - 1,
                                       (j + 1) * k - 1) =
                    p_matrices[m]
                        .submat(p_right_id * k, right_col * k,
                                (p_right_id + 1) * k - 1,
                                (right_col + 1) * k - 1)
                        .t() *
                    matrices[i].submat(right_cen * k, j * k,
                                       (right_cen + 1) * k - 1,
                                       (j + 1) * k - 1);

                left_col = 2 * left_col + 1;
                right_col = 2 * right_col;
            }
        }

        // Creating A = P \check \Sigma^\prime, save it to the next
        // level of P
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            int cen_st_id = (1 << i) - 1;  // The starting id, the real center.
            int left_cen = cen_st_id - (1 << (i - 1));
            int right_cen = cen_st_id + (1 << (i - 1));
            int left_col = j * 2;
            int right_col = j * 2 + 1;

            for (int m = i - 1; m >= 0; m--) {
                p_matrices[i].submat((left_cen - (1 << m) + 1) * k, j * k,
                                     (left_cen + (1 << m)) * k - 1,
                                     (j + 1) * k - 1) +=
                    p_matrices[m].cols(left_col * k, (left_col + 1) * k - 1) *
                    tmp_matrices[i].submat(left_cen * k, j * k,
                                           (left_cen + 1) * k - 1,
                                           (j + 1) * k - 1);

                p_matrices[i].submat((right_cen - (1 << m) + 1) * k, j * k,
                                     (right_cen + (1 << m)) * k - 1,
                                     (j + 1) * k - 1) +=
                    p_matrices[m].cols(right_col * k, (right_col + 1) * k - 1) *
                    tmp_matrices[i].submat(right_cen * k, j * k,
                                           (right_cen + 1) * k - 1,
                                           (j + 1) * k - 1);
                // Update the center.
                if (m > 0) {
                    left_cen += (1 << (m - 1));
                    right_cen -= (1 << (m - 1));
                    left_col = 2 * left_col + 1;
                    right_col = 2 * right_col;
                }
            }
        }

        // Obtain \tilde \Sigma from \check \Sigma^\prime
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            int cen_id = (1 << i) - 1;
            tmp_matrices[i].submat(cen_id * k, j * k, (cen_id + 1) * k - 1,
                                   (j + 1) * k - 1) =
                matrices[i].submat(cen_id * k, j * k, (cen_id + 1) * k - 1,
                                   (j + 1) * k - 1);
            int cen_st_id = (1 << i) - 1;  // The starting id, the real center.
            int left_cen = cen_st_id - (1 << (i - 1));
            int right_cen = cen_st_id + (1 << (i - 1));
            for (int m = i - 1; m >= 0; m--) {
                tmp_matrices[i].submat(cen_id * k, j * k, (cen_id + 1) * k - 1,
                                       (j + 1) * k - 1) -=

                    tmp_matrices[i]
                            .submat(left_cen * k, j * k, (left_cen + 1) * k - 1,
                                    (j + 1) * k - 1)
                            .t() *
                        tmp_matrices[i].submat(left_cen * k, j * k,
                                               (left_cen + 1) * k - 1,
                                               (j + 1) * k - 1) +
                    tmp_matrices[i]
                            .submat(right_cen * k, j * k,
                                    (right_cen + 1) * k - 1, (j + 1) * k - 1)
                            .t() *
                        tmp_matrices[i].submat(right_cen * k, j * k,
                                               (right_cen + 1) * k - 1,
                                               (j + 1) * k - 1);
                // Update the center.
                if (m > 0) {
                    left_cen += (1 << (m - 1));
                    right_cen -= (1 << (m - 1));
                }
            }
        }

        // Obtain AG from \tilde\Sigma
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            int cen_id = (1 << i) - 1;
            block_gram_schmidt(
                tmp_matrices[i].submat(cen_id * k, j * k, (cen_id + 1) * k - 1,
                                       (j + 1) * k - 1),
                p_matrices[i].submat(cen_id * k, j * k, (cen_id + 1) * k - 1,
                                     (j + 1) * k - 1));

            // Update P's -AG part.
            p_matrices[i].submat(0, j * k, cen_id * k - 1, (j + 1) * k - 1) =
                -p_matrices[i].submat(0, j * k, cen_id * k - 1,
                                      (j + 1) * k - 1) *
                p_matrices[i].submat(cen_id * k, j * k, (cen_id + 1) * k - 1,
                                     (j + 1) * k - 1);
            p_matrices[i].submat((cen_id + 1) * k, j * k,
                                 (2 * cen_id + 1) * k - 1, (j + 1) * k - 1) =
                -p_matrices[i].submat((cen_id + 1) * k, j * k,
                                      (2 * cen_id + 1) * k - 1,
                                      (j + 1) * k - 1) *
                p_matrices[i].submat(cen_id * k, j * k, (cen_id + 1) * k - 1,
                                     (j + 1) * k - 1);
        }
    }

    return wrap_mats(p_matrices);
}
