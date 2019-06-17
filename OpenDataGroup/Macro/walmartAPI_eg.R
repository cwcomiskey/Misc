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
walmartAPI::searching(query = "hammer", key = key)[,1:2]


