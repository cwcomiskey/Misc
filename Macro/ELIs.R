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

grepl("\\$", c("abc", "$21"))
