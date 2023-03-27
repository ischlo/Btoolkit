#include <RcppArmadillo.h>
#include <cstdlib>

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

  arma::rowvec x2(n);
  arma::rowvec y2(n);

  x2 = coord2.col(0).as_row()*pi/180;
  y2 = coord2.col(1).as_row()*pi/180;

  for(arma::uword i = 0; i < coord1.n_rows;++i) {

    x1 = arma::rowvec(n,arma::fill::value(coord1(i,0)))*pi/180;
    y1 = arma::rowvec(n,arma::fill::value(coord1(i,1)))*pi/180;

    arma::rowvec num = arma::sqrt(arma::square(arma::cos(y2)%arma::sin(arma::abs(x1-x2))) + arma::square(arma::cos(y1)%sin(y2) - arma::sin(y1)%arma::cos(y2)%arma::cos(arma::abs(x1-x2))));
    arma::rowvec den = arma::sin(y1)%arma::sin(y2) + arma::cos(y1)%arma::cos(y2)%arma::cos(arma::abs(x1-x2));
    res.row(i) = radius*arma::atan(num/den);
  }

  return res;

}


// [[Rcpp::export]]
arma::vec gc_distance_pair_cpp(const arma::mat& coord1,const arma::mat& coord2) {

  arma::uword n = coord2.n_rows;
 
  arma::vec x1(n);
  arma::vec y1(n);

  arma::vec x2(n);
  arma::vec y2(n);

  x2 = coord2.col(0)*pi/180;
  y2 = coord2.col(1)*pi/180;

  x1 = coord1.col(0)*pi/180;
  y1 = coord1.col(1)*pi/180;

  arma::vec num = arma::sqrt(arma::square(arma::cos(y2)%arma::sin(arma::abs(x1-x2))) + arma::square(arma::cos(y1)%sin(y2) - arma::sin(y1)%arma::cos(y2)%arma::cos(arma::abs(x1-x2))));
  arma::vec den = arma::sin(y1)%arma::sin(y2) + arma::cos(y1)%arma::cos(y2)%arma::cos(arma::abs(x1-x2));

  return radius*arma::atan(num/den);

  // return radius*acos(cos(coord2.col(0)*pi/180 - coord1.col(0)*pi/180)%cos(coord1.col(1)*pi/180)%cos(coord2.col(1)*pi/180)+sin(coord1.col(1)*pi/180)%sin(coord2.col(1)*pi/180));

}

