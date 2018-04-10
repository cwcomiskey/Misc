# BLS API

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

# My v2 key: 968f207631b149269ac5603c9424c35c

# Item codes
item_codes <- read.delim("./item_codes", row.names=1)
item_codes$code <- row.names(item_codes)
item_codes <- item_codes[,-2] 
colnames(item_codes) <- c("item", "code")
item_codes <- item_codes %>% mutate(item = as.character(item))

# one series: GET request
# multiple series: POST request

# httr, e.g. 1 ======

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

# httr, e.g. 2 =========

body <- list(
  'seriesid' = c('APU0000701111', 'APU0000701312'),
  'startyear' = 1998, 'endyear' = 2018,
  'registrationKey' = '968f207631b149269ac5603c9424c35c'
)

resp <- httr::POST(
  url = "https://api.bls.gov/publicAPI/v2/timeseries/data/",
  body = body, encode = "json", 
  verbose())

dat <- fromJSON(content(resp, "text"))
str(dat)

dat2 <- dat[["Results"]][["series"]][["data"]][[1]]

dat2 <- select(dat2, year, periodName, value)
dat2$year <- year(parse_date_time(dat2$year, "%y"))
dat2$month <- month(parse_date_time(dat2$periodName, "%m"))

for(i in 1:240){
  if(nchar(dat2$month[i]) == 1) {
    dat2$month[i] <- paste0(0, dat2$month[i])
}}

dat2 <- dat2 %>% mutate(date = ymd(paste0(year, month, "01")),
                        value = as.numeric(value))

ggplot(data = dat2) + 
  geom_line(aes(x = date, y = value))

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

