# flights
# base: "https://api.sandbox.amadeus.com/v1.2/flights/low-fare-search"
# api key: "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34"

# e.g. ===== 
GET("https://api.sandbox.amadeus.com/v1.2/flights/low-fare-search", 
    query = list(
      apikey = "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34", 
      departure_date = as.character(today()), 
      origin = "BOS",
      destination = "DEN")) %>%
  content()

# Origin-destination combos ==== 
IATA <- c("ATL", "LAX", "ORD", "DFW", "JFK", "DEN", "SFO", "LAS", "SEA",
          "MIA", "CLT", "PHX", "MCO", "IAH", "EWR", "MSP", "BOS", "DTW",
          "PHL", "LGA", "FLL", "BWI", "DCA", "SLC", "MDW", "IAD", "SAN",
          "HNL", "TPA", "PDX", "DAL", "STL", "BNA", "HOU", "AUS", "OAK",
          "MSY", "RDU", "MCI", "SJC", "SNA", "SMF", "SAT", "RSW", "IND",
          "CLE")

orig_dest <- combn(IATA, 2) %>% 
  t() %>%
  as.data.frame() %>%
  mutate(Origin = as.character(V1), Dest = as.character(V2)) %>%
  select(Origin, Dest)

# Airports: long/lat ======
a <- read.csv("~/Desktop/ODG/Macro-models/scraping/airports_dat_LongLat.txt", 
              header=FALSE) %>%
  select(V3, V5, V7, V8) %>%
  setnames(c("City", "iata", "Latitude", "Longitude")) %>%
  mutate(City = as.character(City), iata = as.character(iata)) %>%
  filter(iata %in% IATA)

orig <- orig_dest %>% 
  select(Origin) %>%
  left_join(., a, by = c("Origin" = "iata")) %>%
  setnames(c("Origin", "O_city", "O_Latitude", "O_Longitude")) %>%
  unique()

dest <- orig_dest %>% 
  select(Dest) %>%
  left_join(., a, by = c("Dest" = "iata")) %>%
  setnames(c("Dest", "D_city", "D_Latitude", "D_Longitude")) %>%
  unique()

orig_dest <- left_join(orig_dest, orig) %>%
  left_join(., dest)

# Orig-Dest numeric summary =====
OD_dat <- flights_dat %>%
  group_by(Origin, Dest) %>%
  summarise(Min = min(Price, na.rm = TRUE), 
            Q1 = summary(Price)[2],
            Avg = mean(Price, na.rm = TRUE), 
            Q3 = summary(Price)[5],
            Max = max(Price, na.rm = TRUE))

orig_dest <- inner_join(orig_dest, OD_dat); rm(OD_dat)

# One GET ===== 
GET_one_flight <- function(orig = "BOS", dest = "DEN"){
  GET("https://api.sandbox.amadeus.com/v1.2/flights/low-fare-search", 
      query = list(
        apikey = "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34", 
        departure_date = as.character(today()), 
        origin = orig,
        destination = dest)) %>%
    content()
  }

response <- GET_one_flight(orig = "BOS", dest = "DEN")

# Loop: 1035 routes, variables flights per route =====

gather <- function(){
  for(i in 1:1035){ # ROUTES
    
  if(i %% 10 == 0){
    print(i)
    print(dim(dat))
    }
  
  # Create container
  if(i == 1){
    dat <- data.frame("Origin" = NA, "Dest" = NA, "Price" = NA)
  }
  
  # Retrieve ROUTE i FLIGHTS 
  response <- 
    GET_one_flight(orig = orig_dest[i,"Origin"], dest = orig_dest[i,"Dest"])

  # record ROUTE i FLIGHT prices 
  for(j in 1:length(response[["results"]])){ # FLIGHTS
    
    # create iterative route-flights container
    if(j == 1){
      dat_j <- data.frame("Origin" = NA, "Dest" = NA, "Price" = NA)
    }
    
    dat_j[j,c("Origin", "Dest")] <- orig_dest[i,]
    
    # price of flight j, route i
    try({
      dat_j[j, "Price"] <- 
          response[["results"]][[j]]$fare$price_per_adult$total_fare
      })
  } # FLIGHTS 
  
  # Add prices to big container
  try(dat <- rbind.data.frame(dat, dat_j))
  
  # finishing touches
  if(i == 1035){
    rm(i, j, dat_j)
    dat <- dat %>% 
      drop_na() %>%
      mutate(date = today())
  }
  }
  dat <- dat %>% 
    mutate(Price = as.numeric(Price))
  return(dat)
}

# the four steps ======

# (1) load
flights_dat <- data.table::fread("flights_dat.csv") %>%
  mutate("date" = lubridate::ymd(date))

# (2) collect

dat <- gather()

# (3) combine
flights_dat <- rbind.data.frame(flights_dat, dat)

# (4) re-save
data.table::fwrite(flights_dat, "flights_dat.csv")

# Plot =====
# flights_dat <- data.table::fread("flights_dat.csv") %>%
#   mutate("date" = lubridate::ymd(date))

# install.packages(c("maps", "mapdata"))
# devtools::install_github("dkahle/ggmap")

library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)

usa <- map_data("usa")
dim(usa); head(usa)

plot_dat <- orig_dest %>% filter(D_Longitude > -125)

ggplot() +   
  geom_curve(data = plot_dat, 
    aes(x = O_Longitude, y = O_Latitude, xend = D_Longitude, 
        yend = D_Latitude, color = Avg), alpha = 0.3) +
  scale_colour_gradientn(colours=rainbow(4)) +
  geom_polygon(data = usa, aes(x=long, y = lat, group = group), 
               fill = NA, color = "black") +
  geom_text(data = plot_dat, size = 4, color = "blue",
            aes(x = O_Longitude, y = O_Latitude, label = Origin))

  


