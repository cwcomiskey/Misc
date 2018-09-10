# flights =====

macro::depends() # load packages
library(scrapr) # data

# base: 
#   "https://api.sandbox.amadeus.com/v1.2/flights/low-fare-search"
# api key: "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34"

# e.g. ===== 
GET("https://api.sandbox.amadeus.com/v1.2/flights/low-fare-search", 
    query = list(
      apikey = "lGMBxB2n5cXwHnJUrMGVp1AIID47uP34", 
      departure_date = as.character(today()), 
      origin = "BOS",
      destination = "DEN")) %>%
  content()

# Orig-Dest numeric summary =====
OD_dat <- flights_dat %>%
  group_by(Origin, Dest) %>%
  summarise(Min = min(Price, na.rm = TRUE), 
            Q1 = summary(Price)[2],
            Avg = mean(Price, na.rm = TRUE), 
            Q3 = summary(Price)[5],
            Max = max(Price, na.rm = TRUE))

orig_dest <- inner_join(orig_dest, OD_dat); rm(OD_dat)


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

  


