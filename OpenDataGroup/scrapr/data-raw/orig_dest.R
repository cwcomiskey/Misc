# Origin-destination combos + long/lat ==== 

# ADD THIS PROCESSING CODE TO THE HIDDEN PACKAGE CODE (WILL HAVE TO FIND THAT IN HAD'S BOOK)

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