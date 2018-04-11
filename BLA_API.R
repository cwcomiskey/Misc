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

# httr, BLS v2 API=========

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

# httr, .txt from BLS =====

# https://download.bls.gov/pub/time.series/cu/
# https://download.bls.gov/pub/time.series/ap/

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
