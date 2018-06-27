# ELIs: walmart API loop

# https://developer.walmartlabs.com/API_Terms_of_Use
# c) API calls are subject to a daily rate limit of 5000 calls per day.
# ---> change to more lookups per call??

# walmartAPI =======
key = "m9vevaewz6a6fawujxs2amnx"
categories <- walmartAPI::taxonomy(key = key)

# e.g.
orange <- walmartAPI::searching(
  query = "orange", key = key, categoryId = "976759", 
  numItems = 5, sort = "relevance") %>%
  select(itemId, name, salePrice, categoryPath,
         productUrl, offerType)

# e.g.
walmartAPI::searching(
  query = "ground+beef", key = key)[,1:2]

# Automate =====
macro::depends() # load packages
library(scrapr) # data

grab <- function(item, catId = NULL){
  t <- walmartAPI::searching(
    query = item, 
    key = key, 
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
grab_byID <- function(IDs){
  walmartAPI::lookup(id = IDs, key = key) %>%
    select(itemId, 
           name, 
           salePrice, 
           categoryPath,
           productUrl, 
           offerType)
}

# dat = package data
# datr = environment data


collect_byID <- function(){
  dat_itemIDs <- unique(dat$itemId)
  
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

datr <- collect_byID()

# Reading and writing ======
data.table::fwrite(dat, "dat.csv")
dat <- data.table::fread("dat.csv")



