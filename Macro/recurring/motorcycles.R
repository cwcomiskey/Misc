load_packages <- function(){
  library(jsonlite)
  library(stringr)
  library(httr)
  macro::depends()
  library(rvest)
}

# Building blocks =====
bike <- xml2::read_html("https://www.motorcyclistonline.com/2018-honda-xr650l") 

bike2 <- bike %>% rvest::html_nodes("span")

bike3 <- bike2 %>% rvest::html_text()

bike4 <- as.data.frame(bike3) %>%
  mutate(bike3 = as.character(bike3)) %>%
  filter(grepl("\\$", bike3))

# Year-manufacturer-model (or whatever)

# (1) load
bike_dat <- data.table::fread("bike_dat.csv") %>%
  mutate("date" = lubridate::ymd(`date`))

# (2) collect
bikes <- scrapr::collect()

# (3) combine
bike_dat <- rbind.data.frame(bike_dat, bikes)

# (4) re-save
data.table::fwrite(bike_dat, "bike_dat.csv")
