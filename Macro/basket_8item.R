# mini-basket

library(rvest)
library(walmartAPI)
library(purrr)
library(stringr)
library(readr)
library(dplyr)

basket <- data.frame("Item" = 0, "Price" = 0)
# Food - Walmart - Cheerios =======
categories <- walmartAPI::taxonomy(key = key)
key = "m9vevaewz6a6fawujxs2amnx"

cheerios <- walmartAPI::searching(
  query = "cereal", key = key, categoryId = "976759", # food
  numItems = 15, sort = "relevance")
basket[1,] <- c("Cheerios", cheerios$salePrice)

# Energy - Gas - https://gasprices.aaa.com/ =======
gas <- read_html("https://gasprices.aaa.com/?state=CO")
gas_price <- gas %>%
  html_nodes(".average-price--blue .numb") %>%
  html_text() 

basket[2,] <- c("Gas Price", parse_number(gas_price))

# Commodities - Apparel - Nike shoes =======
shoes <- read_html("https://store.nike.com/us/en_us/pw/mens-shoes/7puZoi3?ipp=120")
shoes_price <- shoes %>%
  html_nodes(".local") %>%
  html_text() %>%
  parse_number()

basket[3,] <- c("Nike Shoes", shoes_price[1])

# Shelter - Corvallis Apartment =======
apartment <- read_html("https://www.apartments.com/corvallis-or/")
apt_price <- apartment %>%
  html_nodes(".altRentDisplay") %>%
  html_text() %>%
  parse_number()

basket[4,] <- c("Apartment", apt_price[1])

# Medical Care - ER visit in Alliance, NE======
er_visit <- read_html("https://healthcarebluebook.com/page_ProcedureDetails.aspx?cftId=242&g=Emergency+Room+Visit+-+Complex+Problem")
er_visit_price <- er_visit %>%
  html_nodes(".fair") %>%
  html_text() %>%
  parse_number()

basket[5,] <- c("ER Visit", er_visit_price[1])

# Transportation - oil change =======
oil_change <- read_html("https://www.pepboys.com/service/preventive_maintenance/oil_changes/")
oil_change_price <- oil_change %>%
  html_nodes(".dis-price:nth-child(5)") %>%
  html_text() %>%
  parse_number()

basket[6,] <- c("Oil Change", oil_change_price[1])

# Change to price column to numeric, add weights ======
basket <- basket %>% dplyr::mutate("Price" = as.numeric(Price))
basket$Expenditure_Category <- c("Food", "Energy", "Commodities", "Shelter", "Medical Care", "Transportation Services")
basket$RIW <- c(13.371, 7.619, 19.848, 32.767, 6.942, 5.933) # add relative importance weights, e.g. https://www.bls.gov/news.release/cpi.t01.htm

# Simulate prices for calculation example =======
basket <- basket %>% mutate("1" = Price + abs(rnorm(n = 1))*Price/10) 
for(i in 2:10){
  basket[,paste(i)] <- basket[,paste(i-1)] + abs(rnorm(n = dim(basket)[1]))*basket[,paste(i-1)]/10
}

# Index computation, three steps =======
# See: "Online and offline price indexes: Measuring Argentina's Inflation", Cavallo, 2012
# (1) Unweighted geometric avg
# (2) Category level index at time t
# (3) Weighted arithmetic avg. of category indices

# (1) R^j_(t, t-1); ratio for time t to t-1, in category j
basket$"R_2,1" <- basket$"2"/basket$"1"
for(i in 2:10){
  basket[,paste0("R_", i, ",", i-1)] <- basket[paste(i)]/basket[paste(i-1)]
}

# (2) I^j_t; index for category j, time t
basket <- mutate(basket, I_10 = `R_2,1` * `R_3,2` * `R_4,3`* `R_5,4` * `R_6,5` *
                                  `R_7,6` * `R_8,7` * `R_9,8` * `R_10,9`)

# (3) : S_t - weighted arithmetic avg of category indices
basket$W <- sum(basket$RIW)
basket <- mutate(basket, S_10 = (RIW/W)*I)
S <- sum(basket$S_10)

