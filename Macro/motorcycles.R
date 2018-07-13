load_packages <- function(){
  library(jsonlite)
  library(stringr)
  library(httr)
  macro::depends()
  library(rvest)
}; load_packages(); rm(load_packages)


bike <- xml2::read_html("https://www.motorcyclistonline.com/2018-honda-xr650l") 

bike2 <- bike %>% rvest::html_nodes("span")

bike3 <- bike2 %>% rvest::html_text()

bike4 <- as.data.frame(bike3) %>%
  mutate(bike3 = as.character(bike3)) %>%
  filter(grepl("\\$", bike3))

# Tutorial: http://bradleyboehmke.github.io/2015/12/scraping-html-text.html ====

title <- xml2::read_html("https://www.motorcyclistonline.com/2018-honda-xr650l") %>%
  rvest::html_nodes("h1.page-title") %>%
  html_text() 
title

stats <- xml2::read_html("https://www.motorcyclistonline.com/2018-honda-xr650l") %>%
  rvest::html_nodes("div.buyers-guide--intro-stats-item") %>%
  html_text() %>%  
  strsplit(split = "\n") %>%
  unlist() %>% 
  stringr::str_trim() %>%
  .[. != ""] %>%
  grep("\\$", ., value = TRUE)


# function-ify =====
type <- "2018-honda-xr650l"

# Year-manufacturer-model (or whatever)
types <- c("2018-honda-xr650l", "2014-husaberg-fe-501", "2018-aprilia-rsv4-rf", "2018-beta-evo-250-factory", "2018-bmw-r-1200-gs-adventure")

zoom <- function(type){
  
  url <- paste0("https://www.motorcyclistonline.com/", type)
  
  cycle <- xml2::read_html(url) %>%
    rvest::html_nodes("h1.page-title") %>%
    html_text() 
  
  price <- xml2::read_html(url) %>%
    rvest::html_nodes("div.buyers-guide--intro-stats-item") %>%
    html_text() %>%  
    strsplit(split = "\n") %>%
    unlist() %>% 
    stringr::str_trim() %>%
    .[. != ""] %>%
    grep("\\$", ., value = TRUE)
  
  return(data.frame(cycle, price))
  
}

bikes <- data.frame()
for(i in types) {
  bikes_i <- zoom(i)
  bikes <- rbind.data.frame(bikes, bikes_i)
}
bikes

# HUZZAH!!
