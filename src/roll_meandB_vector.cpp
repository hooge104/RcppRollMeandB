#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector roll_meandB_vector(NumericVector x, int windowSize) {
  int n = x.size();
  NumericVector out(n);

  for(int i = 0; i < n; i++) {
    // Center alignment: windowSize should be centered around the current point
    int start = std::max(0, i - windowSize / 2);
    int end = std::min(i + windowSize / 2, n - 1);
    NumericVector window = x[Range(start, end)];

    // Handling NA values
    if (std::any_of(window.begin(), window.end(), [](double v) { return NumericVector::is_na(v); })) {
      out[i] = NA_REAL;
      continue;
    }

    double sum = 0.0;
    for(int j = 0; j < window.size(); j++) {
      sum += pow(10.0, window[j] / 10.0);
    }

    double meanDB = 10 * log10(sum / window.size());
    out[i] = meanDB;
  }

  return out;
}
