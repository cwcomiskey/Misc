# car rental

macro::depends()

# www.amadeus.com 
# API
# username: chriscomiskey
# password: Koufax32
# Cosumer key: lGMBxB2n5cXwHnJUrMGVp1AIID47uP34

# E.g. =====
url <- "https://api.sandbox.amadeus.com/v1.2/cars/search-airport?apikey=lGMBxB2n5cXwHnJUrMGVp1AIID47uP34&location=DIA&pick_up=2018-12-07&drop_off=2018-12-08"

httr::GET(url)

# E.g. expanded =====

base <- "https://api.sandbox.amadeus.com/v1.2/cars/search-airport"
key <- "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34"
pu <- as.character(today())
do <- as.character(today() + 1)
loc <- "SFO"

req <- GET(base, query = list(
  apikey = key, 
  location = loc, 
  pick_up = pu, 
  drop_off = do)
  ) %>%
  content()

lengths(req$results)

dat <- data.frame("Provider" = NA, "Provider_Code" = NA,
                  "Airport" = NA, "Car_Code" = NA, 
                  "Rate_Type" = NA, "Price" = NA)

# Company
dat[1,c("Provider", "Provider_Code")] <- req[["results"]][[1]][["provider"]]

# Airport
dat[1, "Airport"] <- req[["results"]][[1]][["airport"]]

# Vehicle code
dat[1, "Car_Code"] <- 
  req[["results"]][[1]][["cars"]][[1]][["vehicle_info"]][["acriss_code"]]

# Rate type; e.g. daily
dat[1, "Rate_Type"] <- 
  req[["results"]][[1]][["cars"]][[1]][["rates"]][[1]][["type"]]

# Price
dat[1, "Price"] <- 
  req[["results"]][[1]][["cars"]][[1]][["rates"]][[1]][["price"]][["amount"]]

# Dimension confusion ====
req$results[[1]]$cars[[1]]$rates[[1]]$price$amount

# req$results[[.]]$cars[[.]]$rates[[.]]$price$amount

length(req$results) # [1] 6
lengths(req$results) # [1] 6 6 6 6 6 6

lengths(req$results[[1]])
# provider branch_id  location   airport   address      cars 
#        2         1         2         1         5         6 

lengths(req$results[[2]])
# provider branch_id  location   airport   address      cars 
#        2         1         2         1         4         7 

length(req$results[[1]]$cars) # [1] 6
length(req$results[[2]]$cars) # [1] 7

lengths(req$results[[1]]$cars) # [1] 3 4 4 4 4 4
lengths(req$results[[2]]$cars) # [1] 4 4 4 4 4 4 4




# Automate extraction =====

GET_amadeus <- function(loc = "SFO"){
  
  base <- "https://api.sandbox.amadeus.com/v1.2/cars/search-airport"
  key <- "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34"
  pu <- as.character(today())
  do <- as.character(today() + 1)
  
  request <- GET(base, query = list(
    apikey = key, 
    location = loc, 
    pick_up = pu, 
    drop_off = do)
    ) %>%
    content()
  
  return(request)
}

req <- GET_amadeus()


# Add date
# get rid of NA row

for(i in 1:length(req[["results"]])){ # **COMPANIES**
  
  if(i == 1){
    dat <- data.frame("Provider" = NA, "Provider_Code" = NA,
                      "Airport" = NA, "Car_Code" = NA, 
                      "Rate_Type" = NA, "Price" = NA)
  }

  for(j in 1:length(req[["results"]][[i]][["cars"]])){ # **CARS**
    
    dat_ij <- data.frame("Provider" = NA, "Provider_Code" = NA,
                         "Airport" = NA, "Car_Code" = NA, 
                         "Rate_Type" = NA, "Price" = NA)
    
    # Company
    dat_ij[1,c("Provider_Code", "Provider")] <- req[["results"]][[i]][["provider"]]
    
    # Airport
    dat_ij[1, "Airport"] <- req[["results"]][[i]][["airport"]]
    
    # Vehicle code
    dat_ij[1, "Car_Code"] <- 
      req[["results"]][[i]][["cars"]][[j]][["vehicle_info"]][["acriss_code"]]
    
    # Rate type; e.g. daily
    dat_ij[1, "Rate_Type"] <- 
      req[["results"]][[i]][["cars"]][[j]][["rates"]][[1]][["type"]]
    
    # Price
    dat_ij[1, "Price"] <- 
      req[["results"]][[i]][["cars"]][[j]][["rates"]][[1]][["price"]][["amount"]]
    
    dat <- rbind.data.frame(dat, dat_ij)
      
  }
  if(i == length(req[["results"]])){rm(i, j, dat_ij)}
}




