#include "fun_aux.h"

// [[Rcpp::export]]
List rcpp_dyadFac_core(List entries, int N, int k) {
    // The input is a symmetrically dyadic matrix H, and the output is a
    // vertically dyadic matrix P where P^T H P = I.

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
            queue<int> cen_queue;          // The dynamic center to update.
            int cen_st_id = (1 << i) - 1;  // The starting id, the real center.
            cen_queue.push(cen_st_id - (1 << (i - 1)));
            cen_queue.push(cen_st_id + (1 << (i - 1)));
            queue<int> col_queue;
            col_queue.push(j * 2);
            col_queue.push(j * 2 + 1);

            // This is for the hierarchy in P
            for (int m = i - 1; m >= 0; m--) {
                // This is for the number of columns in certain level.
                for (int s = 0; s < 1 << (i - m); s++) {
                    int cen_id = cen_queue.front();
                    int col_id = col_queue.front();
                    cen_queue.pop();
                    col_queue.pop();
                    if (m != 0) {
                        cen_queue.push(cen_id - (1 << (m - 1)));
                        cen_queue.push(cen_id + (1 << (m - 1)));
                        col_queue.push(col_id * 2);
                        col_queue.push(col_id * 2 + 1);
                    }
                    // Creating \check \Sigma^\prime

                    tmp_matrices[i].submat(cen_id * k, j * k,
                                           (cen_id + 1) * k - 1,
                                           (j + 1) * k - 1) +=
                        p_matrices[m]
                            .cols(col_id * k, (col_id + 1) * k - 1)
                            .t() *
                        matrices[i].submat((cen_id - (1 << m) + 1) * k, j * k,
                                           (cen_id + (1 << m)) * k - 1,
                                           (j + 1) * k - 1);
                }
            }
        }

        // Creating A = P \check \Sigma^\prime, save it to the next
        // level of P
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            queue<int> cen_queue;          // The dynamic center to update.
            int cen_st_id = (1 << i) - 1;  // The starting id, the real center.
            cen_queue.push(cen_st_id - (1 << (i - 1)));
            cen_queue.push(cen_st_id + (1 << (i - 1)));
            queue<int> col_queue;
            col_queue.push(j * 2);
            col_queue.push(j * 2 + 1);
            // This is for the hierarchy in P
            for (int m = i - 1; m >= 0; m--) {
                // This is for the number of columns in certain level.
                for (int s = 0; s < 1 << (i - m); s++) {
                    int cen_id = cen_queue.front();
                    int col_id = col_queue.front();
                    cen_queue.pop();
                    col_queue.pop();
                    if (m != 0) {
                        cen_queue.push(cen_id - (1 << (m - 1)));
                        cen_queue.push(cen_id + (1 << (m - 1)));
                        col_queue.push(col_id * 2);
                        col_queue.push(col_id * 2 + 1);
                    }

                    p_matrices[i].submat((cen_id - (1 << m) + 1) * k, j * k,
                                         (cen_id + (1 << m)) * k - 1,
                                         (j + 1) * k - 1) +=
                        p_matrices[m].cols(col_id * k, (col_id + 1) * k - 1) *
                        tmp_matrices[i].submat(cen_id * k, j * k,
                                               (cen_id + 1) * k - 1,
                                               (j + 1) * k - 1);
                }
            }
        }

        // Obtain \tilde \Sigma from \check \Sigma^\prime
        for (int j = 0; j < (1 << (N - i - 1)); j++) {
            int cen_id = (1 << i) - 1;
            tmp_matrices[i].submat(cen_id * k, j * k, (cen_id + 1) * k - 1,
                                   (j + 1) * k - 1) =
                matrices[i].submat(cen_id * k, j * k, (cen_id + 1) * k - 1,
                                   (j + 1) * k - 1) -
                tmp_matrices[i]
                        .submat(0, j * k, cen_id * k - 1, (j + 1) * k - 1)
                        .t() *
                    tmp_matrices[i].submat(0, j * k, cen_id * k - 1,
                                           (j + 1) * k - 1) -
                tmp_matrices[i]
                        .submat((cen_id + 1) * k, j * k,
                                (2 * cen_id + 1) * k - 1, (j + 1) * k - 1)
                        .t() *
                    tmp_matrices[i].submat((cen_id + 1) * k, j * k,
                                           (2 * cen_id + 1) * k - 1,
                                           (j + 1) * k - 1);
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
