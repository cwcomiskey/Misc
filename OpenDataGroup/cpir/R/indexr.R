#' Calculate an Aggregate Index
#'
#' This function calculates an aggregate index, such as CPI, from (i) its reference and current subindices, (ii) its reference and current relative importance weights, and (iii) a reference value of the aggregate index.
#' @param w0 the reference set of weights.
#' @param sub0 the reference set of subindices.
#' @param index0 the reference value of the index.
#' @param sub1 the current set of subindices.
#'
#' @return The aggregate index value, or vector of values, depending on the arguments supplied (vector/scalar).
#' @examples
#' \dontrun{
#' # Calculate Month 2 (2010-01-01) index using:
#' (i) Month 2 subindices; and
#' (ii) Month 1 (2009-12-01) weights, subindices,
#' and aggregate index
#'
#' indexr(
#'   w0 = weights[1,-1],
#'   sub0 = subindices[1,-1],
#'   index0 = index[1,2],
#'   sub1 = subindices[2,-1]
#'   )
#'
#' # Make sure the date for reference weight/sub/index match;
#' i.e.:
#' weights[1,'Date'] == subindices[1,'date'] &
#' subindices[1,'date'] == index[1,'date']
#' }
#' @export

indexr <- function(w0, sub0, index0, sub1){
  index0 * sum(w0 * (sub1 / sub0) )/sum(w0)
}
