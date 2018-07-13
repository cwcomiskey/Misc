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
  html_text() 
stats
cat(stats)

stats <- xml2::read_html("https://www.motorcyclistonline.com/2018-honda-xr650l") %>%
  rvest::html_nodes("div.buyers-guide--intro-stats-item") %>%
  html_text() %>%  
  strsplit(split = "\n") %>%
  unlist() %>% 
  stringr::str_trim() %>%
  .[. != ""]



