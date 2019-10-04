#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector refractory(IntegerVector predictions, int period) {

  int counter = 0;

  while (counter < predictions.size()) {
    if (predictions[counter] == 1) {
      for (int i = counter + 1; i < (counter + 1 + period); ++i) {
        if (i < predictions.size()) {
          predictions[i] = 0;
        }
      }
      counter += period;
    }
    counter++;
  }

  return predictions;

}
