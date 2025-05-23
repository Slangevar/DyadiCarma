# Generated by using Rcpp::compileAttributes() -> do not edit by hand
# Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

add_helper <- function(l_entries, l_aentries, r_entries, r_aentries, type1, type2, N, k) {
    .Call(`_DyadiCarma_add_helper`, l_entries, l_aentries, r_entries, r_aentries, type1, type2, N, k)
}

multiply_hv <- function(l_entries, r_entries, N, k) {
    .Call(`_DyadiCarma_multiply_hv`, l_entries, r_entries, N, k)
}

multiply_vv <- function(l_entries, r_entries, N, k) {
    .Call(`_DyadiCarma_multiply_vv`, l_entries, r_entries, N, k)
}

multiply_hasv <- function(l_entries, l_aentries, r_entries, N, k, type) {
    .Call(`_DyadiCarma_multiply_hasv`, l_entries, l_aentries, r_entries, N, k, type)
}

multiply_hsv <- function(l_entries, r_entries, N, k, type) {
    .Call(`_DyadiCarma_multiply_hsv`, l_entries, r_entries, N, k, type)
}

multiply_vh <- function(l_entries, r_entries, N, k) {
    .Call(`_DyadiCarma_multiply_vh`, l_entries, r_entries, N, k)
}

multiply_vsh <- function(l_entries, r_entries, N, k, type) {
    .Call(`_DyadiCarma_multiply_vsh`, l_entries, r_entries, N, k, type)
}

multiply_vash <- function(l_entries, r_entries, r_aentries, N, k, type) {
    .Call(`_DyadiCarma_multiply_vash`, l_entries, r_entries, r_aentries, N, k, type)
}

multiply_sas <- function(l_entries, l_aentries, r_entries, r_aentries, N, k) {
    .Call(`_DyadiCarma_multiply_sas`, l_entries, l_aentries, r_entries, r_aentries, N, k)
}

asymm_trans <- function(entries, aentries, N, k) {
    .Call(`_DyadiCarma_asymm_trans`, entries, aentries, N, k)
}

rcpp_as_dyadic <- function(matrix, N, k, type) {
    .Call(`_DyadiCarma_rcpp_as_dyadic`, matrix, N, k, type)
}

rcpp_as_matrix <- function(entries, aentries, N, k, type) {
    .Call(`_DyadiCarma_rcpp_as_matrix`, entries, aentries, N, k, type)
}

rcpp_bandalg_core <- function(entries, N, k) {
    .Call(`_DyadiCarma_rcpp_bandalg_core`, entries, N, k)
}

rcpp_constr <- function(N, k, distr, param) {
    .Call(`_DyadiCarma_rcpp_constr`, N, k, distr, param)
}

rcpp_dyadFac_core <- function(entries, N, k) {
    .Call(`_DyadiCarma_rcpp_dyadFac_core`, entries, N, k)
}

