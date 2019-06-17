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

#' Retrieve Data for 884 Items from Walmart API
#'
#' \code{collect_byID} retrieves data for 884 preselected items from the Walmart API.
#'
#' The items were selected based on 'relevant' results for approximately 170 Entry Level
#' Items that Walmart sells. The initial 'search' queried by item name rather than ID.
#' The 884 item IDs and names are included as package data, named \code{dat_itemIDs} and \code{ELI} respectively.
#'
#' @return Returns 884 by 8 data frame, including the six columns \code{grab_byID} returns,
#' plus the date of retrieval and the item name.
#' @export
collect_byID <- function(){
  dat_itemIDs <- unique(dat$itemId) %>% .[!is.na(.)]

  for(i in 1:length(dat_itemIDs)){
    if(i == 1) {
      datr <- data.frame()
    }

    datr_i <- tryCatch(
      grab_byID(dat_itemIDs[i]),
      error = function(c){
        data.frame(itemId = dat_itemIDs[i],
                   name = i,
                   salePrice = paste(c),
                   categoryPath = NA,
                   productUrl = NA,
                   offerType = NA)
      }
    )

    datr_i <- cbind.data.frame(today(), 'items[i]' = dat_itemIDs[i], datr_i)
    datr <- rbind.data.frame(datr, datr_i)

    if(i %% 25 == 0) print(i)

  }

  count <- 0
  while(count < 15 & sum(is.na(datr$categoryPath)) > 0){
    count = count + 1

    datrNAs <- datr %>% filter(is.na(categoryPath))
    datr <- datr %>% filter(!(is.na(categoryPath)))
    cat("NAs remaining: ", dim(datrNAs)[1], "\n")

    for(i in 1:dim(datrNAs)[1]){

      datrNAs_i <- tryCatch(
        grab_byID(datrNAs$itemId[i]),
        error = function(c){
          data.frame(itemId = datrNAs$itemId[i],
                     name = i,
                     salePrice = paste(c),
                     categoryPath = NA,
                     productUrl = NA,
                     offerType = NA)
        }
      )


      datrNAs_i <- cbind.data.frame(today(), 'items[i]' = datrNAs$itemId[i], datrNAs_i)
      datr <- rbind.data.frame(datr, datrNAs_i)

      if(i %% 25 == 0) cat("NAs Progress: Count = ", count, "Iteration = ", i, "\n")
    }
  }
  return(datr)
}
