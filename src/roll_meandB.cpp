#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix roll_meandB(NumericMatrix x, int windowSize) {
  int rows = x.nrow();
  int cols = x.ncol();
  NumericMatrix out(rows, cols);

  for(int k = 0; k < cols; k++) {
    NumericVector column = x(_, k);

    for(int i = 0; i < rows; i++) {
      // Center alignment: windowSize should be centered around the current point
      int start = std::max(0, i - windowSize / 2);
      int end = std::min(i + windowSize / 2, rows - 1); // ensure the end index does not exceed the length of the column
      NumericVector window = column[Range(start, end)];

      // Handling NA values
      if (std::any_of(window.begin(), window.end(), [](double v) { return NumericVector::is_na(v); })) {
        out(i, k) = NA_REAL;
        continue;
      }

      double sum = 0.0;
      for(int j = 0; j < window.size(); j++) {
        sum += pow(10.0, window[j] / 10.0);
      }

      double meanDB = 10 * log10(sum / window.size());
      out(i, k) = meanDB;
    }
  }

  return out;
}