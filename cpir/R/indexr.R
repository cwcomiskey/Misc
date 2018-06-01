#' Calculate an Aggregate Index
#'
#' This function calculates an aggregate index, such as CPI, from its reference and current subindices, their reference and current relative importance weights, and a reference value of the aggregate index.
#' @param w0 the reference set of weights.
#' @param sub0 the reference set of subindices.
#' @param index0 the reference value of the index.
#' @param sub1 the current set of subindices.
#'
#' @return The aggregate index value, or vector of values, depending on the arguments supplied.
#' @export

indexr <- function(w0, sub0, index0, sub1){
  index0 * sum(w0 * (sub1 / sub0) )/sum(w0)
}
