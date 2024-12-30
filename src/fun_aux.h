#ifndef FUN_AUX_H
#define FUN_AUX_H

#include <RcppArmadillo.h>

#include <cmath>
#include <queue>

using arma::distr_param;
using arma::mat;
using arma::ones;
using arma::subview;
using arma::zeros;
using Rcpp::as;
using Rcpp::Dimension;
using Rcpp::List;
using Rcpp::Named;
using Rcpp::NumericMatrix;
using Rcpp::NumericVector;
using Rcpp::rbinom;
using Rcpp::rnorm;
using Rcpp::runif;
using Rcpp::String;
using Rcpp::wrap;
using std::invalid_argument;
using std::make_pair;
using std::map;
using std::pair;
using std::queue;
using std::sqrt;
using std::string;
using std::unordered_map;
using std::vector;

vector<mat> read_mats(List entries, int N);
vector<mat> init_mats(vector<mat> matrices, int N);
List wrap_mats(vector<mat> matrices);
void multiply_hv_core(vector<mat> l_matrices, vector<mat> r_matrices,
                      vector<mat>& symm_matrices, vector<mat>& asymm_matrices,
                      int N, int k);
void multiply_vv_core(vector<mat> l_matrices, vector<mat> r_matrices,
                      vector<mat>& result_matrices, int N, int k);
void symm_arith_helper(vector<mat>& matrices, int N, int k);
void symm_convert(vector<mat>& matrices, int N, int k);
void symm_read_helper(vector<mat>& matrices, int N, int k);
void asymm_convert(vector<mat>& matrices, vector<mat>& amatrices, int N, int k);
void asymm_read_helper(vector<mat>& matrices, vector<mat>& amatrices, int N,
                       int k);
void as_matrix_helper(vector<mat> matrices, mat& result, int N, int k,
                      char fmt);
void as_dyadic_helper(vector<mat>& matrices, mat matrix, int N, int k,
                      char fmt);

List add_helper(List l_entries, List l_aentries, List r_entries,
                List r_aentries, std::string type1, std::string type2, int N,
                int k);

List multiply_hv(List l_entries, List r_entries, int N, int k);
List multiply_vv(List l_entries, List r_entries, int N, int k);
arma::mat multiply_vh(List l_entries, List r_entries, int N, int k);
List multiply_hasv(List l_entries, List l_aentries, List r_entries, int N,
                   int k, char type);
List multiply_hsv(List l_entries, List r_entries, int N, int k, char type);
arma::mat multiply_vsh(List l_entries, List r_entries, int N, int k, char type);
arma::mat multiply_vash(List l_entries, List l_aentries, List r_entries, int N,
                        int k, char type);

arma::mat multiply_sas(List l_entries, List l_aentries, List r_entries,
                       List r_aentries, int N, int k);

List asymm_trans(List entries, List aentries, int N, int k);

void block_gram_schmidt(mat h, subview<double> p);

#endif
