#' Collect motorcycle prices with scrapr::zoom()
#'
#' \code{zoom} works with \code{collect} to retrieve motorcycle prices
#'
#' @return A data frame of five rows (types), 4 columns: 
#' type of motocycle \code{cycle}, \code{price},
#' the url \code{site}, and \code{date}.
#' @export

collect <- function(){
  
  types <- c("2018-honda-xr650l", "2014-husaberg-fe-501", 
             "2018-aprilia-rsv4-rf", "2018-beta-evo-250-factory", 
             "2018-bmw-r-1200-gs-adventure")
  
  bikes <- data.frame()
  
  for(i in types) {
    bikes_i <- zoom(i)
    bikes <- rbind.data.frame(bikes, bikes_i)
  }
  
  return(bikes)
}
