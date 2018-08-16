library(rvest)

library(jsonlite)
library(stringr)
library(httr)
macro::depends()

collect_car_lease_dat <- function(){
  lease_url <- "https://www.carsdirect.com/deals-articles/cheapest-lease-deals"
  
  l <- xml2::read_html(lease_url) %>%
    html_nodes("#article-body .left-column") %>%
    html_text()   %>%
    strsplit(split = "\n") %>%
    unlist() %>% 
    stringr::str_trim() %>%
    .[. != ""] %>%
    .[. != "Back to top of page"] %>%
    .[-grep("Cheapest", .)]
  
  nam <- l[1:4]
  l <- l[-(1:4)]
  
  for(i in 1:(length(l)/4)){
    if(i == 1){
      dat <- data.frame(matrix(ncol = 4))
      colnames(dat) <- nam
  }
    
    dat[i,] <- l[(4*(i-1) + 1):(4*(i-1) + 4)]
    
    if(i == (length(l)/4)){
      dat <- filter(dat, Vehicle != "Vehicle")
      dat <- cbind.data.frame(dat, Date = today())
      dat <- mutate(dat, Price = as.numeric(gsub("\\$", "", `Effective Cost`)))
      rm(i, nam, l, lease_url)}
  }
  return(dat)
}

# (1) load

car_lease_dat <- data.table::fread("car_lease_dat.csv") %>%
  mutate("date" = lubridate::ymd(`date`))

# (2) collect

dat <- collect_car_lease_dat()

# (3) combine

car_lease_dat <- rbind.data.frame(car_lease_dat, dat)

# (4) resave

data.table::fwrite(car_lease_dat, "car_lease_dat.csv")

