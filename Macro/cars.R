load_packages <- function(){
  library(jsonlite)
  library(stringr)
  library(httr)
  macro::depends()
}; load_packages(); rm(load_packages)

# cars ======
# https://www.cargurus.com/Cars/price-trends/priceIndexJson.action?entityIds=lb40

# (1) load
car_dat <- data.table::fread("car_dat.csv") %>%
  mutate("today()" = lubridate::ymd(`today()`))

# (2) collect
for(i in 1:40){
  if(i == 1){
    car_dat_today <- data.frame()
    entityIds <- data.frame(Ids = c("lb33", "lb32", "lb37", "lb35", "lb34", "lb40", "lb39", "lb44", "lb43", "lb42", "Index", "m4", "m124", "m110", "m19", "m20", "m3", "m21", "m22", "m23", "m24", "m25", "m98", "m203", "m26", "m6", "m27", "m28", "m84", "m31", "m32", "m33", "m34", "m35", "m37", "m38", "m40", "m42", "m55", "m49"))
    base <- "https://www.cargurus.com/Cars/price-trends/priceIndexJson.action?entityIds="
  }
  
  url <- paste0(base, entityIds[i,1])
  
  car <- GET(url = url) %>%
    content() %>%
    fromJSON()
  
  car_price <- car %>%
    .$data %>%
    as.data.frame() %>%
    tail(., 1) %>%
    .[1,2]
  
  car_dat_today <- rbind.data.frame(
    car_dat_today, cbind.data.frame(car[2:3], car_price, today())
  )
  
  if(i == 40){
    rm(i, url, car_price, base, entityIds, car)
  }
}

# (3) combine
car_dat <- rbind.data.frame(car_dat, car_dat_today)

# (4) re-save
data.table::fwrite(car_dat, "car_dat.csv")

