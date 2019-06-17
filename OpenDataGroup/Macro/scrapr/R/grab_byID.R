#' Retrieve item information from Walmart API
#'
#' \code{grab_byID} retrieves item information from the Walmart API, by Item ID, and trims
#' the results to six helpful columns.
#'
#' @param IDs The Walmart ID(s) of item(s) to collect information on.
#'
#' @return A data frame of i rows, where i is the number of IDs provided;
#' and six basically self explanatory columns: \code{itemId}, \code{name},
#' \code{salePrice}, \code{categoryPath}, \code{productUrl}, \code{offerType}.
#' @export
grab_byID <- function(IDs){
  walmartAPI::lookup(id = IDs, key = scrapr::key) %>%
    select(itemId,
           name,
           salePrice,
           categoryPath,
           productUrl,
           offerType)
}

