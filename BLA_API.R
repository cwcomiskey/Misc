# BLS API

library(data.table)
library(devtools)
library(readr)
library(dplyr)
library(stringr)
# install_github("mikeasilva/blsAPI")
# library(blsAPI)
library(jsonlite)
library(httr)
library(ggplot2)
library(lubridate)

# My BLS v2 API key: 968f207631b149269ac5603c9424c35c

# CU = consumer price index

# base_code
# A = Alternate	
# S = Current
# 
# periodicity_code	
# R = Monthly	
# S = Semi-Annual
# 
# seasonal_code     
# S = Seasonally-adjusted
# U  = Unadjusted

# blsAPI package ======
# Ex 1
response <- blsAPI('LAUCN040010000000005', api_version = 2)
fromJSON(response)

# Ex 2 (works)
payload <- list(
  'seriesid'=c('LAUCN040010000000005','LAUCN040010000000006'),
  'startyear'=2010,
  'endyear'=2012,
  'catalog'=FALSE,
  'calculations'=TRUE,
  'annualaverage'=TRUE,
  'registrationKey'='968f207631b149269ac5603c9424c35c') # KEY WORKS
response <- blsAPI(payload, 2)
json <- fromJSON(response)

# httr, e.g. 1; deprecated? ======

body <- list(
  'seriesid'=c('LAUCN040010000000005', 'LAUCN040010000000006'), 
  'startyear' = 2000, 'endyear' = 2010,
  'registrationKey' = '968f207631b149269ac5603c9424c35c'
             )

resp <- httr::POST(
  url = "https://api.bls.gov/publicAPI/v2/timeseries/data/",
  body = body, encode = "json", 
  verbose())

fromJSON(content(resp, "text"))

# httr -- BLS v2 API=========

# one series: GET request
# multiple series: POST request

body <- list(
  'seriesid' = c('APU0000701111', 'APU0000701312'),
  'startyear' = 1999, 'endyear' = 2018,
  'registrationKey' = '968f207631b149269ac5603c9424c35c'
)

resp <- httr::POST(
  url = "https://api.bls.gov/publicAPI/v2/timeseries/data/",
  body = body, encode = "json") # verbose() 

dat <- fromJSON(content(resp, "text"))
str(dat)

dat2 <- dat[["Results"]][["series"]][["data"]][[1]]

dat2 <- select(dat2, year, periodName, value)
dat2$year <- year(parse_date_time(dat2$year, "%y"))
dat2$month <- month(parse_date_time(dat2$periodName, "%m"))

for(i in 1:dim(dat2)[1]){ # for lubridate formatting
  if(nchar(dat2$month[i]) == 1) {
    dat2$month[i] <- paste0(0, dat2$month[i])}
  }

dat2 <- dat2 %>% mutate(date = ymd(paste0(year, month, "01")),
                        value = as.numeric(value))

ggplot(data = dat2) + 
  geom_line(aes(x = date, y = value))

# httr -- .txt from BLS, example =====

# https://download.bls.gov/pub/time.series/cu/
# https://download.bls.gov/pub/time.series/ap/

# e.g.
dat <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.data.11.USFoodBeverage") 
dat <- content(dat, "text")
dat <- data.table::fread(dat)

month_convert <- function(m){
    if (m == "M01") {
      return("January")
    } else if (m == "M02") {
      return("February")
    } else if (m == "M03") {
      return("March")
    } else if (m == "M04") {
      return("April")
    } else if (m == "M05") {
      return("May")
    } else if (m == "M06") {
      return("June")
    } else if (m == "M07") {
      return("July")
    } else if (m == "M08") {
      return("August")
    } else if (m == "M09") {
      return("September")
    } else if (m == "M10") {
      return("October")
    } else if (m == "M11") {
      return("November")
    } else if (m == "M12") {
      return("December")
    } else {
      return(m)
    }
}
m <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12")
dat <- filter(dat, period %in% m) 

dat <- mutate(dat, 
              month = substring(period, 2),
              date = ymd(paste0(year, month, "01"))
              ) %>% select(series_id, date, value)

dat2 <- filter(dat, series_id %in% unique(dat$series_id)[1:12])

ggplot(data = dat2) + 
  geom_line(aes(x = date, y = value)) + 
  facet_wrap(~ series_id, scales = "free")

# area_codes: cu.area  =====
area_dat <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.area") %>% content("text") %>% data.table::fread() %>% select(V1, V2, V3) 
colnames(area_dat) <- c("area_code", "area_name", "display_level")

# item_codes: cu.item =========================================
# https://download.bls.gov/pub/time.series/cu/cu.item
item_codes <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.item") %>% 
  content("text") %>% 
  data.table::fread() %>% 
  select(V1, V2, V3) 
colnames(item_codes) <- c("item_code", "item_name",	"display_level")

# table(item_codes$display_level)
#  0   1   2   3   4   5   6   7   8 
# 12  66  70 113  43  37  37  18   3 

filter(item_codes, display_level == "0")

# meta-data: cu.series =================================

# https://download.bls.gov/pub/time.series/cu/cu.series

meta_dat <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.series") %>% 
  content("text") %>% 
  data.table::fread()

names(meta_dat)
length(unique(meta_dat$area_code)) # [1] 58 areas
length(unique(meta_dat$item_code)) # [1] 399 items

# all-in-one series: cu.data.1.AllItems =================================
all <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems") %>% 
  content("text") %>% 
  data.table::fread()

length(unique(all$series_id)) # [1] 190
table(all$series_id)




# All category-item series: cu.data.xx.category =====================
# https://download.bls.gov/pub/time.series/cu/ + ...
ends <- c("cu.data.1.AllItems", "cu.data.11.USFoodBeverage", "cu.data.12.USHousing", 
          "cu.data.13.USApparel", "cu.data.14.USTransportation", 
          "cu.data.15.USMedical", "cu.data.16.USRecreation", 
          "cu.data.17.USEducationAndCommunication", 
          "cu.data.18.USOtherGoodsAndServices")
d <- list()
for(i in 1:length(ends)){
  d[[i]] <- httr::GET(
    url = paste0("https://download.bls.gov/pub/time.series/cu/", ends[i])) %>% 
    content("text") %>% 
    data.table::fread() 
}

# Primary categories df =================

# Primary categories item codes
items0 <- filter(item_codes, display_level == 0) %>% .[c(3,5:12),]

# Meta-data for all primary categories
series0_meta_data <- filter(meta_dat, 
                            item_code %in% items0$item_code, # "Major Group"
                            area_code == "0000",             # Nat'l avg.
                            seasonal == "U",
                            periodicity_code == "R")

# Food and beverage test run ====== 
month_convert <- function(m){
  if (m == "M01") {
    return("January")
  } else if (m == "M02") {
    return("February")
  } else if (m == "M03") {
    return("March")
  } else if (m == "M04") {
    return("April")
  } else if (m == "M05") {
    return("May")
  } else if (m == "M06") {
    return("June")
  } else if (m == "M07") {
    return("July")
  } else if (m == "M08") {
    return("August")
  } else if (m == "M09") {
    return("September")
  } else if (m == "M10") {
    return("October")
  } else if (m == "M11") {
    return("November")
  } else if (m == "M12") {
    return("December")
  } else {
    return(m)
  }
}
m <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", "M09", "M10", "M11", "M12")

FB <- d[[1]]

FB0 <- FB %>% filter(
  series_id %in% series0_meta_data$series_id, period %in% m) %>%
  mutate(month = substring(period, 2), 
         date = ymd(paste0(year, month, "01"))) %>%
  select(series_id, value, date)

ggplot(data = FB0) + geom_line(aes(x = date, y = value)) + 
  ggtitle("Food & Beverage") + 
  theme(plot.title = element_text(hjust = 0.5))

# Housing test run =====
H <- d[[2]]

H0 <- H %>% filter(
  series_id %in% series0_meta_data$series_id, period %in% m) %>%
  mutate(month = substring(period, 2), 
         date = ymd(paste0(year, month, "01"))) %>%
  select(series_id, value, date)

# All primary categories test run ======
all <- data.frame()
for(i in 1:length(d)){
  series_data <- d[[i]] %>% filter(
    series_id %in% series0_meta_data$series_id, period %in% m) %>%
    mutate(month = substring(period, 2), 
           date = ymd(paste0(year, month, "01")),
           item_code = substring(series_id, 9)) %>%
    select(series_id, value, date, item_code)
  
  all <- rbind.data.frame(all, series_data)
}

all <- left_join(all, items0)


ggplot(data = all) + geom_line(aes(x = date, y = value)) + 
  facet_wrap(~ item_name)

# ggsave("primaryCPIcats_notfree.jpg", height = 8, width = 8)

# correlation test run ======
# cor(Food & Beverage, Housing)
unique(all$item_name)
FB <- filter(all, item_name == "Food and beverages")
Tr <- filter(all, item_name == "Transportation")

date_intersect <- intersect(
  interval(min(Tr$date), max(Tr$date)),
  interval(min(FB$date), max(FB$date))
        )

FB2 <- filter(FB, date %within% date_intersect)
Tr2 <- filter(Tr, date %within% date_intersect)

cor(FB2$value, Tr2$value)

ggplot() + geom_point(aes(x = FB2$value, y = Tr2$value))

# pseudo-algorithm (failing right now) =====
# (1) Pick two categories
# (2) find respective date ranges
# (3) intersect date ranges
# (4) filter respective data sets
# (5) cor(d1, d2)

nam <- unique(all$item_name)
matr <- matrix(nrow = length(nam), ncol = length(nam))
for(i in 1:length(nam)){
  for(j in 1:length(nam)){
    
    A <- filter(all, item_name == nam[i])
    B <- filter(all, item_name == nam[j])
    
    date_intersect <- intersect(
      interval(min(A$date), max(A$date)),
      interval(min(B$date), max(B$date)))
    
    A2 <- filter(A, date %within% date_intersect)
    B2 <- filter(B, date %within% date_intersect)
    
    matr[i,j] <- cor(A2$value, B2$value)
    
  }
}

# Next attempt =====
# What I want:
#     [Date rows] x [series_id columns] matrix filled with index values
# 
# How:
# (1) Modify cu.data.xx.category download to filter down to: date, id, value
# (2) Bunch of 1 x 1 joins preserving all dates (union of dates)
