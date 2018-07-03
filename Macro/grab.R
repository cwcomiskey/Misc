grab <- function(item, catId = NULL){
  t <- walmartAPI::searching(
    query = item, 
    key = scrapr::key, 
    categoryId = catId, 
    numItems = 5, 
    sort = "relevance") %>%
    select(itemId, 
           name, 
           salePrice, 
           categoryPath,
           productUrl, 
           offerType) %>%
    .[1:5,]
  return(t)
} 

grab("flour")
