#ifndef AUX_H
#define AUX_H

#include <RcppArmadillo.h>

#include <cmath>
#include <queue>

using arma::mat;
using arma::subview;
using arma::zeros;
using Rcpp::as;
using Rcpp::List;
using Rcpp::Named;
using Rcpp::NumericMatrix;
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
void symm_multiply_helper(vector<mat>& matrices, int N, int k);
void symm_convert(vector<mat>& matrices, int N, int k);
void asymm_convert(vector<mat>& matrices, vector<mat>& amatrices, int N, int k);
void dyad2mat_helper(vector<mat> matrices, mat& result, int N, int k, char fmt);

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
