# Grocery Scraping w/ Selector Gadget =====

install.packages("rvest")
library(rvest)

# Breakfast cereal (selector gadget) ======
wal_cereal <- 
  read_html("https://www.walmart.com/browse/976759_976783_1231208") # breakfast cereals

wal_cereal %>% 
  html_nodes(".search-result-product-title , .enable-2price-2") %>%
  html_text()


# Canned goods
wal_canned <- 
  read_html("https://www.walmart.com/browse/food/canned-goods-soups/976759_976794_976785")

wal_canned %>% 
  html_nodes(".product-title-link span , .price-main-block div") %>%
  html_text()

# Use Walmart API, known item id ======

vignette("httr")

?GET
b <- httr::GET(
  url = "http://api.walmartlabs.com/v1/items/12417832?format=json&apiKey=m9vevaewz6a6fawujxs2amnx"
  )

baseball2 <- httr::content(b)

itemID = "12417832"
baseball <- httr::GET(
  url = paste0("http://api.walmartlabs.com/v1/items/", itemID), 
  query = list(
    format = "json",
    apiKey = "m9vevaewz6a6fawujxs2amnx"
    ))

baseball2 <- httr::content(baseball)
httr::content(baseball)[1:4]

# Raw stuff ======
resp <- httr::GET(
  url = "http://api.walmartlabs.com/v1/items/12417832?format=json&apiKey=m9vevaewz6a6fawujxs2amnx"
  )

resp_content <- resp %>% httr::content()

resp_item <- resp %>%
  httr::content() 

# walmartAPI =======
key = "m9vevaewz6a6fawujxs2amnx"
categories <- walmartAPI::taxonomy(key = key)
orange <- walmartAPI::searching(
  query = "orange", key = key, categoryId = "976759", 
  numItems = 5, sort = "relevance") %>%
  select(itemId, name, salePrice, categoryPath,
         productUrl, offerType)

bread <- walmartAPI::searching(
  query = "bread", key = key, categoryId = "976759", 
  numItems = 10, sort = "relevance")

grab <- function(item, catId, n){
  t <- walmartAPI::searching(
    query = item, key = key, categoryId = catId, 
    numItems = 5, sort = "relevance") %>%
    select(itemId, name, salePrice, categoryPath,
           productUrl, offerType) %>%
    .[1:5,]
  
  return(t)
}

t <- grab("shirt", "5438")

# Appendix5 <- read.csv("~/Desktop/ODG/Macro-models/Appendix5.txt", sep="")

# keep ELIs for searching Walmart
# App5_ELIs <- Appendix5[nchar(as.character(Appendix5$Code)) == 5,]

