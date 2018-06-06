#' Calculate Relative Importance Weights
#'
#' This function calculates the relative importance weights for a collection of subindices, based on changes in the subindices and the aggregate index.
#' @param w0 the reference set of weights.
#' @param sub0 the reference set of subindices.
#' @param index0 the reference value of the index.
#' @param sub1 the current set of subindices.
#' @param index1 the current value of the index
#'
#' @return A vector of weights
#' @export
#' @examples
#' \dontrun{
#' # Calculate Month 2 (2010-01-01) weights using
#' (i) Month 1 (2009-12-01) weights, subindices, and
#' aggregate index
#' (ii) Month 2 subindices and aggregate index
#'
#' weightr(
#'   w0 = weights[1,-1],
#'   sub0 = subindices[1,-1],
#'   index0 = index[1,2],
#'   sub1 = subindices[2,-1],
#'   index1 = index[2,2]
#'   )
#'
#' # Make sure the date for reference weight/sub/index
#' match (as well as for current month);
#' i.e.
#' weights[1,'Date'] == subindices[1,'date'] &
#' subindices[1,'date'] == index[1,'date'] &
#' subindices[2,'date'] == index[2,'date']
#' }

weightr <- function(w0, sub0, index0, sub1,  index1){
  w0 * (sub1 / sub0) * (index0 / index1)
}
