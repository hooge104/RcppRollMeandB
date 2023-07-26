#' @title Rolling Mean dB
#' @description This function calculates the rolling mean dB
#' @param x A numeric matrix.
#' @param windowSize An integer.
#' @return A numeric matrix with the rolling mean dB
#' @export
roll_meandB_wrap <- function(x, windowSize = 3) {
  result <- roll_meandB(x, windowSize)
  return(result)
}

#' @title Rolling Mean dB Vector
#' @description This function calculates the rolling mean dB for a vector.
#' @param x A numeric vector.
#' @param windowSize An integer.
#' @return A numeric vector with the rolling mean dB
#' @export
roll_meandB_vector_wrap <- function(x, windowSize = 3) {
  result <- roll_meandB_vector(x, windowSize)
  return(result)
}

#' @title Roll Min and Threshold
#' @description This function performs some operation.
#' @param x A numeric vector or matrix.
#' @param windowRowSize An integer.
#' @param windowColSize An integer.
#' @param threshold An integer.
#' @return A numeric vector or matrix after some operation.
#' @export
roll_meandB_threshold_wrap <- function(x, windowRowSize = 9, windowColSize = 3, threshold = 3) {
  result <- roll_meandB_threshold(x, windowRowSize, windowColSize, threshold)
  return(result)
}

#' @title dB mode per row
#' @description This function performs some operation.
#' @param x A numeric vector
#' @param windowSize An integer. Default = 5
#' @return A numeric vector after some operation.
#' @export
dB.mode.per.row <- function(x, windowSize = 5){
  x <- as.numeric(x)
  seq.100 <- seq(from = min(x), to = max(x), length.out = 100)
  sound.hist <- graphics::hist(as.numeric(x), breaks = seq.100, plot = FALSE)
  
  # Smooth counts with a moving window
  sound.hist$counts <- roll_meandB_vector(sound.hist$counts, windowSize)
  
  # Find bin with the largest number of counts.
  mode <- sound.hist$mids[which.max(sound.hist$counts)]
  
  return(mode)
}