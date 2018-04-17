# BLS API

library(data.table)
# library(devtools)
library(readr)
library(dplyr)
library(stringr)
# install_github("mikeasilva/blsAPI")
# library(blsAPI)
library(jsonlite)
library(httr)
library(ggplot2)
library(lubridate)
library(ggfortify)

#     series_id                series_title
# 1 CUUR0000SA0                   All items 
# 2 CUUR0000SAA                     Apparel 
# 3 CUUR0000SAE Education and communication 
# 4 CUUR0000SAF          Food and beverages 
# 5 CUUR0000SAG    Other goods and services 
# 6 CUUR0000SAH                     Housing 
# 7 CUUR0000SAM                Medical care 
# 8 CUUR0000SAR                  Recreation 
# 9 CUUR0000SAT              Transportation

# My BLS v2 API key: 968f207631b149269ac5603c9424c35c

# CU = consumer price index ("consumer urban")

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
# U = Unadjusted

# https://download.bls.gov/pub/time.series/cu/
# https://download.bls.gov/pub/time.series/ap/

# e.g. ====
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
m <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", 
       "M09", "M10", "M11", "M12")
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
  url = "https://download.bls.gov/pub/time.series/cu/cu.area") %>% 
  content("text") %>% 
  data.table::fread() %>% 
  select(V1, V2, V3) 
colnames(area_dat) <- c("area_code", "area_name", "display_level")

# item_codes: cu.item =========================================
item_dat <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.item") %>% 
  content("text") %>% 
  data.table::fread() %>% 
  select(V1, V2, V3) 
colnames(item_dat) <- c("item_code", "item_name",	"display_level")

# table(item_codes$display_level)
#  0   1   2   3   4   5   6   7   8 
# 12  66  70 113  43  37  37  18   3 

# meta-data: cu.series =================================

meta_dat <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.series") %>% 
  content("text") %>% 
  data.table::fread()

# all-in-one series: cu.data.1.AllItems =================================
# ALL_items/series; **NOT** 1_item/series
all_in_one_dat <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems") %>% 
  content("text") %>% 
  data.table::fread()

# All category-item series: cu.data.xx.category =====================
# https://download.bls.gov/pub/time.series/cu/{...}
end_points <- c("cu.data.1.AllItems", "cu.data.11.USFoodBeverage", 
                "cu.data.12.USHousing", "cu.data.13.USApparel", 
                "cu.data.14.USTransportation", "cu.data.15.USMedical", 
                "cu.data.16.USRecreation", 
                "cu.data.17.USEducationAndCommunication", 
                "cu.data.18.USOtherGoodsAndServices")

for(i in 1:length(end_points)){
  if(i == 1) all_dat <- list()
  all_dat[[i]] <- httr::GET(
    url = paste0("https://download.bls.gov/pub/time.series/cu/", 
                 end_points[i])) %>% 
    content("text") %>% 
    data.table::fread() 
}

# "Major Group" df =================
# Major Group > Expenditure Class > Item Strata > Entry Level Item
# Primary categories item codes
major_group <- filter(item_dat, display_level == 0) %>% .[c(3,5:12),]

# "Major Groups" meta data
major_group_meta_data <- filter(meta_dat, 
                                 item_code %in% major_group$item_code, # "Major Group"
                                 area_code == "0000", # Nat'l avg.
                                 seasonal == "U",
                                 periodicity_code == "R")

# All "Major Group" dfs ======
m <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", 
       "M09", "M10", "M11", "M12")

for(i in 1:length(all_dat)){
  if(i == 1) major_groups_dat <- data.frame()
  
  major_group_i <- all_dat[[i]] %>% filter(
    series_id %in% major_group_meta_data$series_id, 
    period %in% m) %>%
    mutate(month = substring(period, 2), 
           date = ymd(paste0(year, month, "01")),
           item_code = substring(series_id, 9)) %>%
    select(series_id, value, date, item_code)
  
  major_groups_dat <- rbind.data.frame(major_groups_dat, major_group_i)
}

major_groups_dat <- left_join(major_groups_dat, item_dat)


ggplot(data = all_major_groups) + 
  geom_line(aes(x = date, y = value)) + 
  facet_wrap(~ item_name)

# ggsave("primaryCPIcats_notfree.jpg", height = 8, width = 8)

# correlation test run ======
# cor(Food & Beverage, Housing)
unique(all_major_groups$item_name)
Food_and_Bev <- filter(all_major_groups, item_name == "Food and beverages")
Transportation <- filter(all_major_groups, item_name == "Transportation")

date_intersect <- intersect(
  interval(min(Transportation$date), max(Transportation$date)),
  interval(min(Food_and_Bev$date), max(Food_and_Bev$date))
        )

Food_and_Bev2 <- filter(Food_and_Bev, date %within% date_intersect) %>%
  mutate(lag1 = value - lag(value))
Transportation2 <- filter(Transportation, date %within% date_intersect) %>%
  mutate(lag1 = value - lag(value))

cor(Food_and_Bev2$value, Transportation2$value)
cor(Food_and_Bev2$lag1, Transportation2$lag1, use = "complete.obs")

ggplot() + geom_point(aes(x = Food_and_Bev2$lag1, y = Transportation2$lag1))


a <- autoplot(acf(Food_and_Bev2$lag1, na.action = na.pass, plot = FALSE)) + 
  ggtitle("Autocorrelation: Food and Bev. Index Monthly Change") + 
  theme(plot.title = element_text(hjust = 0.5))
b <- autoplot(acf(Transportation2$lag1, na.action = na.pass, plot = FALSE)) +
  ggtitle("Autocorrelation: Transportation Index Monthly Change") + 
  theme(plot.title = element_text(hjust = 0.5))

c <- grid.arrange(a, b)

ggsave("FBandTrans_autocorr.jpg", plot = c, height = 4, width = 6)

# "major group" correlation matrix =====

#     series_id                series_title
# 1 CUUR0000SA0                   All items 
# 2 CUUR0000SAA                     Apparel 
# 3 CUUR0000SAE Education and communication 
# 4 CUUR0000SAF          Food and beverages 
# 5 CUUR0000SAG    Other goods and services 
# 6 CUUR0000SAH                     Housing 
# 7 CUUR0000SAM                Medical care 
# 8 CUUR0000SAR                  Recreation 
# 9 CUUR0000SAT              Transportation 

for(i in 1:length(all_dat)){
  
  if(i == 1){
    corr_matr <- matrix(nrow = length(all_dat), ncol = length(all_dat))
  }
  
  for(j in 1:length(all_dat)){
    
    groupA <- filter(major_groups_dat, 
                     series_id == major_group_meta_data$series_id[i])
    groupB <- filter(major_groups_dat, 
                     series_id == major_group_meta_data$series_id[j])
    
    date_intersect <- intersect(
      interval(min(groupA$date), max(groupA$date)),
      interval(min(groupB$date), max(groupB$date))
      )
    
    groupA2 <- filter(groupA, date %within% date_intersect)
    groupB2 <- filter(groupB, date %within% date_intersect)
    
    joined <- full_join(groupA2[,c("value", "date")], 
                        groupB2[,c("value", "date")], 
                        by = "date")

    corr_matr[i,j] <- with(joined, cor(value.x, value.y, use = "complete.obs"))
  }
}
round(corr_matr, 4)

corr_melt <- melt(corr_matr)
ggplot(data = corr_melt) + 
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "YlOrRd") +
  scale_x_continuous(
    expand=c(0,0), 
    breaks=1:9, 
    labels=c("All", "App", "E&C", "F&B", "OGaS", "H", "MC", "R", "T")) +
  scale_y_continuous(expand=c(0,0), breaks=1:9, labels=major_group$item_name) 

# First diff. correlation matrix =======
major_groups_dat <- major_groups_dat %>% 
  group_by(series_id) %>% 
  mutate(lag1 = value - lag(value))

for(i in 1:length(all_dat)){
  
  if(i == 1){
    corr_matr_lag1 <- matrix(nrow = length(all_dat), ncol = length(all_dat))
  }
  
  for(j in 1:length(all_dat)){
    
    groupA <- filter(major_groups_dat, 
                     series_id == major_group_meta_data$series_id[i])
    groupB <- filter(major_groups_dat, 
                     series_id == major_group_meta_data$series_id[j])
    
    date_intersect <- intersect(
      interval(min(groupA$date), max(groupA$date)),
      interval(min(groupB$date), max(groupB$date))
    )
    
    groupA2 <- filter(groupA, date %within% date_intersect)
    groupB2 <- filter(groupB, date %within% date_intersect)
    
    joined <- full_join(groupA2[,c("lag1", "date")], 
                        groupB2[,c("lag1", "date")], 
                        by = "date")
    
    corr_matr_lag1[i,j] <- with(joined, cor(lag1.x, lag1.y, use = "complete.obs"))
  }
}
round(corr_matr_lag1, 2)
colnames(corr_matr_lag1) <- c("All", "App", "E&C", "F&B", 
                              "OGaS", "H", "MC", "R", "T")
rownames(corr_matr_lag1) <- major_group$item_name

corr_melt_lag1 <- melt(corr_matr_lag1)

ggplot(data = corr_melt_lag1) + 
  geom_tile(aes(x = Var1, y = Var2, fill = value)) +
  scale_fill_distiller(palette = "YlOrRd", trans="reverse") +
  coord_equal() +
  scale_x_continuous(
    expand=c(0,0), 
    breaks=1:9, 
    labels=c("All", "App", "E&C", "F&B", "OGaS", "H", "MC", "R", "T")) +
  scale_y_continuous(expand=c(0,0), breaks=1:9, labels=major_group$item_name)

ggsave("lag1_corr.jpg", height = 8, width = 8)  

# Transportation Major Group deep(er) dive =====

m <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", 
       "M09", "M10", "M11", "M12")

# Get data ==
transportation_dat <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.data.14.USTransportation") %>% 
  content("text") %>% 
  data.table::fread() %>%
  filter(period %in% m) %>%
  mutate(month = substring(period, 2), 
         date = ymd(paste0(year, month, "01"))) %>%
  select(series_id, date, value)

# trim meta_dat for join ==
meta_dat <- meta_dat %>% 
  select(series_id, area_code, item_code, seasonal, 
         periodicity_code, base_period, series_title) 

# Add meta-info to transportation data ==
transportation_dat <- left_join(transportation_dat, meta_dat, by = "series_id") %>%
  filter(area_code == "0000", seasonal == "U", periodicity_code == "R") %>%
  mutate(series_title = str_replace(series_title, " in U.S. city average, all urban consumers, not seasonally adjusted", "")) %>% 
  left_join(., item_dat[,-"item_name"])

# Transportation data "display levels" ==
td_levels <- transportation_dat %>% 
  group_by(series_id) %>% 
  summarise(mean(display_level))
# 0  1  2  3  4 
# 1  3  9 17 10

# Filter to levels 0,1,2 and calculate lag1
trans_dat_012 <- transportation_dat %>% 
  filter(display_level %in% c(0, 1, 2)) %>%
  group_by(series_id) %>%
  mutate(lag1value = value - lag(value)) # ** MONTH-TO-MONTH CHANGES **

# Correlation matrix ==
names012 <- unique(trans_dat_012$series_id)
titles012 <- unique(trans_dat_012$series_title)
for(i in 1:length(names012)){
  if(i==1) {
    corr_matr <- matrix(nrow = length(names012), ncol = length(names012))
  }
  groupA <- filter(trans_dat_012, series_id == names012[i])
  
  for(j in 1:length(names012)){
    groupB <- filter(trans_dat_012, series_id == names012[j])
    
    date_intersect <- intersect(
      interval(min(groupA$date), max(groupA$date)),
      interval(min(groupB$date), max(groupB$date))
    )
    
    groupA2 <- filter(groupA, date %within% date_intersect)
    groupB2 <- filter(groupB, date %within% date_intersect)
    
    joined <- full_join(groupA2[,c("lag1value", "date")], 
                        groupB2[,c("lag1value", "date")], 
                        by = "date")
    
    corr_matr[i,j] <- with(joined, cor(lag1value.x, lag1value.y, use = "complete.obs"))
  }
}

series_id_title <- trans_dat_012 %>% 
  group_by(series_id) %>% 
  summarise(d = unique(series_title))

rownames(corr_matr) <- titles012
round(corr_matr, 2)
round(corr_matr[-c(6,7,9,12,13), -c(6,7,9,12,13)], 2) # Bingo.

colnames(corr_matr_lag1) <- c("All", "App", "E&C", "F&B", "OGaS", "H", "MC", "R", "T")

ggplot(data = trans_dat_012) +
  geom_line(aes(x = date, y = value)) +
  facet_wrap(~ series_title, scales = "free")
