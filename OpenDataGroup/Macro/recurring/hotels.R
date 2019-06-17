# hotels

macro::depends() # load packages
library(scrapr) # data

# e.g. ===== 
GET_one <- function(l = "BOS"){
  GET("https://api.sandbox.amadeus.com/v1.2/hotels/search-airport", 
    query = list(
      apikey = "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34", 
      check_in = as.character(today()), 
      check_out = as.character(today() + 1),
      location = l,
      number_of_results = 10)) %>%
  content()
}

# Parse hotels around one airport
parse_GET_one <- function(loc = "BOS"){
  
  dat <- data.frame("Property" = NA, 
                    "Property_Code" = NA, 
                    "Longitude" = NA,
                    "Latitude" = NA,
                    "Price" = NA)
  
  response <- GET_one(l = loc)
  
  for(i in 1:length(response[["results"]])){
    
    dat[i, "Property"] <- response[["results"]][[i]][["property_name"]]
    dat[i, "Property_Code"] <- response[["results"]][[i]][["property_code"]]
    
    dat[i, "Longitude"] <- response[["results"]][[i]][["location"]][["longitude"]]
    dat[i, "Latitude"] <- response[["results"]][[i]][["location"]][["latitude"]]
    
    dat[i, "Price"] <- response[["results"]][[i]][["total_price"]][["amount"]]
  
  }
  
  return(dat)
}

test <- parse_GET_one()
