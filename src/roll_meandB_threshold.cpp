#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix roll_meandB_threshold(NumericMatrix x, int windowRowSize, int windowColSize, double threshold) {
  int rows = x.nrow();
  int cols = x.ncol();
  NumericMatrix out(rows, cols);
  
  // Initialize output matrix with NA values
  std::fill(out.begin(), out.end(), NumericVector::get_na());
  
  // Create padded matrix
  NumericMatrix padded(rows + windowRowSize - 1, cols + windowColSize - 1);
  int rowPad = (windowRowSize - 1) / 2;
  int colPad = (windowColSize - 1) / 2;
  for(int i = 0; i < rows; i++) {
    for(int j = 0; j < cols; j++) {
      padded(i + rowPad, j + colPad) = x(i, j);
    }
  }

  // Allocate window vector once
  NumericVector window(windowRowSize * windowColSize);
  
  for(int i = 0; i < rows; i++) {
    for(int j = 0; j < cols; j++) {
      int windowIndex = 0;
      for(int k = 0; k < windowRowSize; k++) {
        for(int l = 0; l < windowColSize; l++) {
          double value = padded(i + k, j + l);
          // Assume no NA values
          window[windowIndex++] = value;
        }
      }
      
      if(windowIndex > 0) {
        // Calculate meanDB
        double sum = std::accumulate(window.begin(), window.begin() + windowIndex, 0.0, [](double a, double b) {
          return a + std::pow(10.0, b / 10.0);
        });
        double meanDB = 10.0 * std::log10(sum / windowIndex);
        
        // Apply threshold
        out(i, j) = meanDB > threshold ? x(i, j) : *std::min_element(window.begin(), window.begin() + windowIndex);
      }
    }
  }
  
  return out;
}

