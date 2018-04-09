# BLS API

library(devtools)
library(readr)
library(dplyr)
library(stringr)
install_github("mikeasilva/blsAPI")
library(blsAPI)
library(jsonlite)
library(httr)

# My v2 key: 968f207631b149269ac5603c9424c35c

# Item codes
item_codes <- read.delim("./item_codes", row.names=1)
item_codes$code <- row.names(item_codes)
item_codes <- item_codes[,-2] 
colnames(item_codes) <- c("item", "code")
item_codes <- item_codes %>% mutate(item = as.character(item))

# one series: GET request
# multiple series: POST request
# I FIGURED IT OUT!! POST REQUEST!!

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

