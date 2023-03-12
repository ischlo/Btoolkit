// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// distance_mat_cpp
arma::mat distance_mat_cpp(const arma::mat& coord1, const arma::mat& coord2);
RcppExport SEXP _Btoolkit_distance_mat_cpp(SEXP coord1SEXP, SEXP coord2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type coord1(coord1SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type coord2(coord2SEXP);
    rcpp_result_gen = Rcpp::wrap(distance_mat_cpp(coord1, coord2));
    return rcpp_result_gen;
END_RCPP
}
// distance_pair_cpp
arma::mat distance_pair_cpp(const arma::mat& coord1, const arma::mat& coord2);
RcppExport SEXP _Btoolkit_distance_pair_cpp(SEXP coord1SEXP, SEXP coord2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type coord1(coord1SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type coord2(coord2SEXP);
    rcpp_result_gen = Rcpp::wrap(distance_pair_cpp(coord1, coord2));
    return rcpp_result_gen;
END_RCPP
}
// gc_distance_mat_cpp
arma::mat gc_distance_mat_cpp(const arma::mat& coord1, const arma::mat& coord2);
RcppExport SEXP _Btoolkit_gc_distance_mat_cpp(SEXP coord1SEXP, SEXP coord2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type coord1(coord1SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type coord2(coord2SEXP);
    rcpp_result_gen = Rcpp::wrap(gc_distance_mat_cpp(coord1, coord2));
    return rcpp_result_gen;
END_RCPP
}
// gc_distance_pair_cpp
arma::vec gc_distance_pair_cpp(const arma::mat& coord1, const arma::mat& coord2);
RcppExport SEXP _Btoolkit_gc_distance_pair_cpp(SEXP coord1SEXP, SEXP coord2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type coord1(coord1SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type coord2(coord2SEXP);
    rcpp_result_gen = Rcpp::wrap(gc_distance_pair_cpp(coord1, coord2));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Btoolkit_distance_mat_cpp", (DL_FUNC) &_Btoolkit_distance_mat_cpp, 2},
    {"_Btoolkit_distance_pair_cpp", (DL_FUNC) &_Btoolkit_distance_pair_cpp, 2},
    {"_Btoolkit_gc_distance_mat_cpp", (DL_FUNC) &_Btoolkit_gc_distance_mat_cpp, 2},
    {"_Btoolkit_gc_distance_pair_cpp", (DL_FUNC) &_Btoolkit_gc_distance_pair_cpp, 2},
    {NULL, NULL, 0}
};

RcppExport void R_init_Btoolkit(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}