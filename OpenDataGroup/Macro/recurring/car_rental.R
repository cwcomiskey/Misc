# car rentals

macro::depends()

# www.amadeus.com 
# API
# username: chriscomiskey
# password: Koufax32
# Consumer key: lGMBxB2n5cXwHnJUrMGVp1AIID47uP34

# E.g. =====

base <- "https://api.sandbox.amadeus.com/v1.2/cars/search-airport"
key <- "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34"
pu <- as.character(today())
do <- as.character(today() + 1)
loc <- "SFO"

resp <- GET(base, query = list(
  apikey = key, 
  location = loc, 
  pick_up = pu, 
  drop_off = do)
  ) %>%
  content()

lengths(resp$results)

dat <- data.frame("Provider" = NA, "Provider_Code" = NA,
                  "Airport" = NA, "Car_Code" = NA, 
                  "Rate_Type" = NA, "Price" = NA)

# Company
dat[1,c("Provider", "Provider_Code")] <- resp[["results"]][[1]][["provider"]]

# Airport
dat[1, "Airport"] <- resp[["results"]][[1]][["airport"]]

# Vehicle code
dat[1, "Car_Code"] <- 
  resp[["results"]][[1]][["cars"]][[1]][["vehicle_info"]][["acriss_code"]]

# Rate type; e.g. daily
dat[1, "Rate_Type"] <- 
  resp[["results"]][[1]][["cars"]][[1]][["rates"]][[1]][["type"]]

# Price
dat[1, "Price"] <- 
  resp[["results"]][[1]][["cars"]][[1]][["rates"]][[1]][["price"]][["amount"]]

# Automate extraction =====
collect_amadeus <- function(loc = "SFO"){
  
  base <- "https://api.sandbox.amadeus.com/v1.2/cars/search-airport"
  key <- "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34"
  pu <- as.character(today())
  do <- as.character(today() + 1)
  
  resp <- GET(base, query = list(
    apikey = key, 
    location = loc, 
    pick_up = pu, 
    drop_off = do)
    ) %>%
    content()

  for(i in 1:length(resp[["results"]])){ 
  
  # ****COMPANIES****
  if(i == 1){
    dat <- data.frame("Provider" = NA, "Provider_Code" = NA,
                      "Latitude" = NA, "Longitude" = NA,
                      "Airport" = NA, "Car_Code" = NA, 
                      "Rate_Type" = NA, "Price" = NA)
  }

  # ****CARS****
  for(j in 1:length(resp[["results"]][[i]][["cars"]])){ 
    
    if(j == 1){
      dat_j <- data.frame("Provider" = NA, "Provider_Code" = NA,
                          "Airport" = NA, "Car_Code" = NA, 
                          "Rate_Type" = NA, "Price" = NA)
    }
    
    # Company**
    dat_j[j,c("Provider_Code", "Provider")] <- resp[["results"]][[i]][["provider"]]
    
    dat_j[j, "Latitude"] <- resp[["results"]][[i]][["location"]][["latitude"]]
    
    dat_j[j, "Longitude"] <- resp[["results"]][[i]][["location"]][["longitude"]]

    # Airport**
    dat_j[j, "Airport"] <- resp[["results"]][[i]][["airport"]]
    
    # Vehicle code** (acriss code)
    dat_j[j, "Car_Code"] <- 
      resp[["results"]][[i]][["cars"]][[j]][["vehicle_info"]][["acriss_code"]]
    
    # Rate type; e.g. daily**
    dat_j[j, "Rate_Type"] <- 
      resp[["results"]][[i]][["cars"]][[j]][["rates"]][[1]][["type"]]
    
    # Price**
    dat_j[j, "Price"] <- 
      resp[["results"]][[i]][["cars"]][[j]][["rates"]][[1]][["price"]][["amount"]]
  }
  
  dat <- rbind.data.frame(dat, dat_j)

  if(i == length(resp[["results"]])){
    rm(i, j, dat_j)
    dat <- dat %>% 
      drop_na() %>%
      mutate(date = today())
  }
  
  }
  return(dat)
} # one airport

amadeus_car_rental_byIATA <- function(){
  
  for(i in 1:46){
    
    if(i == 1){
      IATA <- c("ATL", "LAX", "ORD", "DFW", "JFK", "DEN", "SFO", "LAS", "SEA",
                "MIA", "CLT", "PHX", "MCO", "IAH", "EWR", "MSP", "BOS", "DTW",
                "PHL", "LGA", "FLL", "BWI", "DCA", "SLC", "MDW", "IAD", "SAN",
                "HNL", "TPA", "PDX", "DAL", "STL", "BNA", "HOU", "AUS", "OAK",
                "MSY", "RDU", "MCI", "SJC", "SNA", "SMF", "SAT", "RSW", "IND",
                "CLE")
      dat <- data.frame("Provider" = NA, "Provider_Code" = NA,
                        "Latitude" = NA, "Longitude" = NA,
                        "Airport" = NA, "Car_Code" = NA, 
                        "Rate_Type" = NA, "Price" = NA,
                        "date" = NA)
    }
    
    if(i %% 5 == 0){
      print(i)
      print(dim(dat))
    }
    print(IATA[i])
    
    try({
      dat_IATA <- collect_amadeus(loc = IATA[i])
      dat <- rbind.data.frame(dat, dat_IATA)
    }, silent = TRUE)
  }
  dat <- dat %>%
    drop_na()
  return(dat)
} # 46 airports

# the four steps ======

# (1) load
car_rental_dat <- data.table::fread("car_rental_dat.csv") %>%
  mutate("date" = lubridate::ymd(date))

# (2) collect

dat <- amadeus_car_rental_byIATA()

# (3) combine
car_rental_dat <- rbind.data.frame(car_rental_dat, dat)

# (4) re-save
data.table::fwrite(car_rental_dat, "car_rental_dat.csv")

# Plot =====
# car_rental_dat_by_IATA <- dat
# data.table::fwrite(car_rental_dat_by_IATA, "car_rental_dat_by_IATA.csv")

# Maps

# some standard map packages.
# install.packages(c("maps", "mapdata"))
# devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)

usa <- map_data("usa")
dim(usa); head(usa)

plot_dat <- car_rental_dat_by_IATA %>%
  filter(Longitude > -130, Price < 300)

ggplot() +   
  geom_point(data = plot_dat, 
             aes(x = Longitude, y = Latitude, 
                 color=Price),
             # position = position_jitter(w = 2, h = 2),
             alpha = 0.7) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_polygon(data = usa, 
               aes(x=long, y = lat, group = group), 
               fill = NA, color = "black")

summary(plot_dat$Price)
