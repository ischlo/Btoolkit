#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// This is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp
// function (or via the Source button on the editor toolbar). Learn
// more about Rcpp at:
//
//   http://www.rcpp.org/
//   http://adv-r.had.co.nz/Rcpp.html
//   http://gallery.rcpp.org/
//
// Vectorized

// [[Rcpp::export]]
NumericVector distance_cpp_vec(NumericVector x1,NumericVector y1,NumericVector x2,NumericVector y2) {
  double radius = 6371010;
  double pi = 3.141593;

      return radius*acos(cos(x2*pi/180 - x1*pi/180)*cos(y1*pi/180)*cos(y2*pi/180)+sin(y1*pi/180)*sin(y2*pi/180));
};




