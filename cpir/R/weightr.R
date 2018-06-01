#' Calculate Relative Importance Weights
#'
#' This function calculates the relative importance weights for a collection of subindices, based on changes in the subindices and the aggregate index.
#' @param w0 the reference set of weights.
#' @param sub0 the reference set of subindices.
#' @param index0 the reference value of the index.
#' @param sub1 the current set of subindices.
#' @param index1 the currewnt value of the index
#'
#' @return A vector of weights
#' @export

weightr <- function(w0, sub0, index0, sub1,  index1){
  w0 * (sub1 / sub0) * (index0 / index1)
}
