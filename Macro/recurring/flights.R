# flights

GET(base, query = list(
  apikey = key, 
  departure_date = as.character(today()), 
  origin = "BOS",
  destination = "DEN")) %>%
  content()

# Origin-destination combos ==== #
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

# GET === #
GET_one_flight <- function(orig = "BOS", dest = "DEN"){
  GET("https://api.sandbox.amadeus.com/v1.2/flights/low-fare-search", 
      query = list(
        apikey = "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34", 
        departure_date = as.character(today()), 
        origin = orig,
        destination = dest)) %>%
    content()
  }

response <- GET_one_flight()

# Parse one GET =====

dat <- data.frame("Origin" = NA, "Dest" = NA, "Price" = NA)

dat[1,c("Origin", "Dest")] <- orig_dest[1,]

length(response[["results"]]) # number of flights/route

response[["results"]][[30]]$fare$price_per_adult$total_fare # price of flight 30

# Need double loop:
# (1) Routes (1035)
# (2) Flights (Variable number per route) 

for(i in 1:1035){ # ROUTES
  
  # query FLIGHTS for that ROUTE

  for(j in 1:length(response[["results"]])){ # FLIGHTS
    
    # retrieve FLIGHTS and store
    
  }
}







