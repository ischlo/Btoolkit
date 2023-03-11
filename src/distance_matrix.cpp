#include <RcppArmadillo.h>
#include <strings.h>
#include <cstdlib>
#include <iterator>

// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins("cpp11")]]

// https://en.wikipedia.org/wiki/Great-circle_distance check here for details on the subject.

const double radius = 6371009;
const double pi = 3.141593;

// [[Rcpp::export]]
arma::mat distance_mat_cpp(const arma::mat& coord1,const arma::mat& coord2){

  arma::mat res(coord1.n_rows,coord2.n_rows);
  arma::uword n = coord2.n_rows;

  for(arma::uword i = 0; i < coord1.n_rows;++i) {

    res.row(i) = arma::sqrt(arma::square(arma::rowvec(n, arma::fill::value(coord1(i,0))) - coord2.col(0).as_row()) + arma::square(arma::rowvec(n, arma::fill::value(coord1(i,1))) - coord2.col(1).as_row()));
  }

  return res;
}


// [[Rcpp::export]]
arma::mat distance_pair_cpp(const arma::mat& coord1,const arma::mat& coord2){

  return arma::sqrt(arma::square(coord1.col(0)-coord2.col(0)) + arma::square(coord1.col(1)-coord2.col(1)));
}


// [[Rcpp::export]]
arma::mat gc_distance_mat_cpp(const arma::mat& coord1,const arma::mat& coord2) {

  arma::mat res(coord1.n_rows,coord2.n_rows);
  arma::uword n = coord2.n_rows;

  arma::rowvec x1(n);
  arma::rowvec y1(n);

  for(arma::uword i = 0; i < coord1.n_rows;++i) {

    x1 = arma::rowvec(n,arma::fill::value(coord1(i,0)))*pi/180;
    y1 = arma::rowvec(n,arma::fill::value(coord1(i,1)))*pi/180;

    arma::rowvec num = arma::sqrt(arma::square(arma::cos(coord2.col(1).as_row()*pi/180)%arma::sin(x1-coord2.col(0).as_row()*pi/180)) + arma::square(cos(y1)%sin(coord2.col(1).as_row()*pi/180) - arma::sin(y1)%arma::cos(coord2.col(1).as_row()*pi/180)%arma::cos(x1-coord2.col(0).as_row()*pi/180)));
    arma::rowvec den = arma::sin(y1)%arma::sin(coord2.col(1).as_row()*pi/180) + arma::cos(y1)%arma::cos(coord2.col(1).as_row()*pi/180)%cos(x1-coord2.col(0).as_row()*pi/180);
    res.row(i) = radius*arma::atan(num/den);
  }

  return res;

}


// [[Rcpp::export]]
arma::vec gc_distance_pair_cpp(const arma::mat& coord1,const arma::mat& coord2) {

  return radius*arma::acos(arma::cos(coord2.col(0)*pi/180 - coord1.col(0)*pi/180)%arma::cos(coord1.col(1)*pi/180)%arma::cos(coord2.col(1)*pi/180)+arma::sin(coord1.col(1)*pi/180)%arma::sin(coord2.col(1)*pi/180));

}

