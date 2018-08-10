library(rvest)

library(jsonlite)
library(stringr)
library(httr)
macro::depends()

# From the jaws of defeat... 

xml2::read_html("https://www.carsdirect.com/deals-articles/cheapest-lease-deals") %>%
  html_nodes("#article-body span") 

# ... to VICTORY!!!

# FUCK. YEAH!!

# ===== Experimentation ======

xml2::read_html("http://www.vw.com/special-offers/") %>% 
  html_nodes("h2.offerTitle") # all <h2> of class "offerTitle"
# HUZZAH!!

xml2::read_html("http://www.vw.com/special-offers/") %>% 
  html_nodes("h2.modelName") # all <h2> of class "modelName"
# HUZZAH!!

xml2::read_html("http://www.vw.com/special-offers/") %>% 
  html_nodes("h2.offerTitle") # all <h2> of class "offerTitle"
# HUZZAH!!
 
xml2::read_html("http://www.vw.com/special-offers/") %>% 
  html_nodes("div.mf-container") %>%
  html_nodes("[data-model=allnewjetta]")



