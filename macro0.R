library(data.table)
library(readr); library(stringr)
library(dplyr); library(lubridate)
library(httr); library(jsonlite)
library(ggplot2); library(ggfortify)
library(tidyverse)

# My BLS v2 API key: 968f207631b149269ac5603c9424c35c

# CU = consumer price index ("consumer urban")

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

# CPI ========
m <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", 
       "M09", "M10", "M11", "M12")

CPI <- httr::GET(
  url = "https://download.bls.gov/pub/time.series/cu/cu.data.1.AllItems") %>% 
  content("text") %>% 
  data.table::fread() %>%
  filter(series_id == "CUUR0000SA0",
         period %in% m) %>%
  mutate(month = substring(period, 2), 
         date = ymd(paste0(year, month, "01")),
         lag1 = value - lag(value)) %>%
  select(series_id, date, value, lag1)

# CPI change autocorrelation =====
autoplot(acf(CPI$lag1, na.action = na.pass, plot = FALSE, lag.max = 120)) +
  ggtitle("CPI: Autocorrelation of month-to-month Changes") + 
  theme(plot.title = element_text(hjust = 0.5))

ggsave("CPImtmACF.jpg", height = 4, width = 8)

# Major Groups (list) =====================
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
  if(i == length(end_points)) rm(end_points)
}

# Major Group meta-data =================
# Major Group > Expenditure Class > Item Strata > Entry Level Item
# Primary categories item codes
major_group <- filter(item_dat, display_level == 0) %>% .[c(3,5:12),]

# "Major Groups" meta data
major_group_meta_data <- filter(
  meta_dat, 
  item_code %in% major_group$item_code, 
  area_code == "0000", # Nat'l avg.
  seasonal == "U",
  periodicity_code == "R")

# Major Groups (df) ======
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

# Major Group correlation matrix =====

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
    labels=c("All", "App", "E&C", "F&B", 
             "OGaS", "H", "MC", "R", "T")) +
  scale_y_continuous(expand=c(0,0), 
                     breaks=1:9, 
                     labels=major_group$item_name) 

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
  scale_y_continuous(expand=c(0,0), 
                     breaks=1:9, labels=major_group$item_name)

ggsave("lag1_corr.jpg", height = 8, width = 8)  

# Transportation Major Group deep(er) dive =====

m <- c("M01", "M02", "M03", "M04", "M05", "M06", "M07", "M08", 
       "M09", "M10", "M11", "M12")

# Get data ==
t_dat <- httr::GET(
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

# df: CPI + transportation major group
dat <- rbind.data.frame(CPI, t_dat)

# meta-info for CPI + transportation ==
dat <- 
  left_join(dat, meta_dat, by = "series_id") %>%
  filter(area_code == "0000", seasonal == "U", periodicity_code == "R") %>%
  mutate(series_title = str_replace(series_title, " in U.S. city average, all urban consumers, not seasonally adjusted", "")) %>% 
  left_join(., item_dat[,-"item_name"])

# Transportation data "display levels" ==
tdat_levels <- transportation_dat %>% 
  group_by(series_id) %>% 
  summarise(mean(display_level))
table(tdat_levels$`mean(display_level)`)
# 0  1  2  3  4 
# 1  3  9 17 10

# Filter to levels 0,1,2 and calculate lag1
dat_012 <- dat %>% 
  filter(display_level %in% c(0, 1, 2)) %>%
  group_by(series_id) %>%
  mutate(lag1value = value - lag(value)) # ** MONTH-TO-MONTH CHANGES **

# Correlation matrix ==
titles012 <- unique(dat_012$series_title)

IDs <- unique(dat_012$series_id)
for(i in 1:length(IDs)){
  
  if(i==1) {
    corr_matr <- matrix(nrow = length(IDs), 
                        ncol = length(IDs))
    }

  for(j in 1:length(IDs)){
    
    groupA <- filter(dat_012, series_id == IDs[i])
    groupB <- filter(dat_012, series_id == IDs[j])
    
    date_intersect <- intersect(
      interval(min(groupA$date), max(groupA$date)),
      interval(min(groupB$date), max(groupB$date))
    )
    
    groupA2 <- filter(groupA, date %within% date_intersect)
    groupB2 <- filter(groupB, date %within% date_intersect)
    
    joined <- full_join(groupA2[,c("lag1value", "date")], 
                        groupB2[,c("lag1value", "date")], 
                        by = "date")
    
    corr_matr[i,j] <- with(joined, cor(lag1value.x, lag1value.y, 
                                       use = "complete.obs"))
  }
}

# check series_id and series_title match up
series_id_title <- dat_012 %>% 
  group_by(series_id) %>% 
  summarise(d = unique(series_title))

rownames(corr_matr) <- titles012
corr_matr <- round(corr_matr[-c(3, 4, 11), -c(3, 4, 11)], 3) # Remove inbetween level

colnames(corr_matr) <- c("All", "Tr", "MVs", "Fuel", "P&E", "M&R", "Ins", 
                  "Fees", "Air", "Inter", "Intra")
corr_matr

ggplot(data = trans_dat_012) +
  geom_line(aes(x = date, y = value)) +
  facet_wrap(~ series_title, scales = "free")

# Stratum meta-data df ======

# Strata (level 2) meta-data
strata <- item_dat %>%
  filter(display_level == 2)

# add item levels
meta_dat <- left_join(meta_dat, item_dat) %>%
  filter(area_code == "0000",
         seasonal == "U",
         periodicity_code == "R",
         item_code %in% strata$item_code); rm(item_dat)

strata_meta_dat <- left_join(strata, meta_dat); rm(strata, meta_dat)

# All strata meta + series df ========
for(i in 1:length(all_dat)){
  if(i == 1) dat <- data.frame()
  
  major_group_i <- all_dat[[i]] %>% 
    filter(period %in% m,
           series_id %in% strata_meta_dat$series_id) %>%
    mutate(month = substring(period, 2), 
           date = ymd(paste0(year, month, "01"))) %>%
    select(series_id, value, date)
  
  dat <- rbind.data.frame(dat, major_group_i)
  
  if(i == length(all_dat)) rm(major_group_i, all_dat, i)
}

# Combine data and meta-data ==
dat <- left_join(dat, strata_meta_dat) %>%
  select(series_id, value, date, item_code, item_name,
         display_level, area_code, base_period) %>%
  group_by(series_id) %>% 
  mutate(lag1 = value - lag(value)) # saved as strata_dat


# All stratum correlation matrix, top25 df =====
dat <- strata_dat
IDs <- unique(dat$series_id)
for(i in 1:length(IDs)){
  
  if(i == 1){
    corr_matr <- data.frame(nrow = length(IDs), ncol = 2)
    colnames(corr_matr) <- c("series_id", "Cor")
  }
    groupA <- filter(dat, series_id == IDs[i])
    groupB <- CPI
    
    date_intersect <- intersect(
      interval(min(groupA$date), max(groupA$date)),
      interval(min(groupB$date), max(groupB$date))
    )
    
    groupA2 <- filter(groupA, date %within% date_intersect)
    groupB2 <- filter(groupB, date %within% date_intersect)
    
    joined <- full_join(groupA2[,c("lag1", "date")], 
                        groupB2[,c("lag1", "date")], 
                        by = "date")
    
    corr_matr[i,1] <- IDs[i]
    corr_matr[i,2] <- with(joined, 
                           round(cor(lag1.x, lag1.y, 
                                     use = "complete.obs"), 4)
                           )
    
    if(i == length(IDs)){
      rm(groupA, groupB, groupA2, groupB2, joined, date_intersect, i, IDs)
    }
  }
corr_matr <- left_join(corr_matr, strata_dat) %>%
        select(item_name, series_id, Cor)

top25 <- corr_matr %>% arrange(desc(Cor)) %>% .[1:25,] %>% .[,-2]
top25[21,1] <- "Club membership"

# tidy df: [date] x [top-25], for regression ======
corr_matr <- corr_matr %>% arrange(desc(Cor))

for(i in 1:25){
  if(i == 1) {
    reg_dat <- CPI %>% select(date, lag1) 
    names(reg_dat) <- c("date", "CPI")
  }
  
  strata_i <- dat %>% 
    filter(series_id == corr_matr[i,"series_id"]) %>%
    select(lag1, date)
    
  reg_dat <- full_join(reg_dat, strata_i, by = "date") 
  reg_dat <- within(reg_dat, rm(series_id))
  names(reg_dat)[names(reg_dat) == 'lag1'] <- substring(corr_matr[i,"series_id"], 9)
  
  if(i == 25) rm(strata_i)
}

# Linear regression: lm(CPI ~ Top25, ...) =======

lin_reg <- lm(CPI ~ ., data = select(reg_dat, -date))

s <- summary(lin_reg)
lin_reg <- lm(CPI ~ SETB + SEHB + SAH21 + SETG01 + SEFV + SAF11 +
                SEAC + SEAA + SEMC + SEHA + SEMF - 1, data = reg_dat)
s <- summary(lin_reg)
names(s)
summary(lin_reg)
ggplot() + geom_line(aes(x = residuals(lin_reg)))

# ID hunting ==
IDs <- c("SETB", "SEHB", "SAH21", "SETG01", "SEFV", "SAF11", 
         "SEAC", "SEAA", "SEMC", "SEHA", "SEMF")
keepers <- corr_matr %>% filter(substring(series_id, 9) %in% IDs)
corr_matr <- corr_matr %>% mutate(keeper = as.numeric(substring(series_id, 9) %in% IDs))
# ============= %

# Formatting for plotting regression results ======
# Add fitted values to reg_dat for plotting by date ==
reg_dat <- reg_dat %>% mutate(index = rownames(reg_dat))
fitt <- cbind.data.frame(fitted(lin_reg), residuals(lin_reg))
fitt <- mutate(fitt, index = row.names(fitt))
colnames(fitt) <- c("fit", "residuals", "index")
reg_dat <- left_join(reg_dat, fitt)

# Manual melt for plotting ==
plot_dat <- reg_dat %>% select(date, CPI, fit, residuals) %>% drop_na()
plot_dat_CPI <- data.frame(plot_dat[,1:2], "CPI")
names(plot_dat_CPI) <- c("date", "Change", "Source")
plot_dat_fit <- data.frame(plot_dat[,c(1,3)], "Fit")
names(plot_dat_fit) <- c("date", "Change", "Source")
plot_dat2 <- rbind.data.frame(plot_dat_CPI, plot_dat_fit)

# Model residuals plots; raw, acf, pacf =====
ggplot(data = plot_dat) + 
  geom_line(aes(y = residuals, x = date))
# ggsave("ModelResiduals.jpg", width = 8, height = 4)

autoplot(decompose(plot_dat$residuals))

autoplot(acf(plot_dat$residuals, na.action = na.pass, plot = FALSE, lag.max = 24)) +
  ggtitle("Model Residual Autocorrelation") + 
  theme(plot.title = element_text(hjust = 0.5))
# ggsave("ModResidACF.jpg", width = 8, height = 4)

autoplot(pacf(plot_dat$residuals, na.action = na.pass, plot = FALSE, lag.max = 24)) +
  ggtitle("Model Residual Partial Autocorrelation") + 
  theme(plot.title = element_text(hjust = 0.5))
# ggsave("ModResidPACF.jpg", width = 8, height = 4)


# Changes: CPI vs. Fit (plot) =====
ggplot(data = plot_dat2) +
  geom_line(aes(x = date, y = Change, color = Source), size = 1.25) 
ggsave("CPIvFit.jpg", width = 10, height = 5)

ggplot() + 
  geom_point(aes(x = fitted(lin_reg), 
                 y = residuals.lm(lin_reg))) +
  xlab("Fitted Values") + ylab("Residuals")

autoplot(lin_reg)[1:2]
                      
# Train and test model ====== 
train_dat <- reg_dat[1:1243,]
test_dat <- reg_dat[1244:1263,]
lin_reg2 <- lm(CPI ~ ., data = select(train_dat, -date))

preds <- predict.lm(lin_reg2, test_dat)
1- sum( (preds - test_dat$CPI)^2) / sum( (test_dat$CPI - mean(test_dat$CPI))^2 ) 

test_dat$pred <- predict(lin_reg2, test_dat)

train_dat <- train_dat %>% drop_na()

ggplot() +
  geom_line(data = rbind.data.frame(train_dat[,1:2], test_dat[,1:2]), 
            aes(x = date, y = CPI), color = "red", size = 1.25) + 
  geom_line(data = test_dat, aes(x = date, y = pred), 
            color = "blue", size = 1.25) +
  ylab("CPI Change") + 
  ggtitle(expression(paste("Linear Regression, ", R^{2}, " = 0.91")))

ggsave("CPIvFit_train_test.jpg", width = 10, height = 5)

# Auto ARIMA and plot======
library(forecast)

t <- auto.arima(ts(lin_reg$residuals), max.p = 3, max.d = 3, max.q = 3, max.P = 3, max.Q = 3, max.D = 13 )
autoplot(acf(t$residuals, lag.max = 48)) +
  ggtitle("New Residuals: Regression with AR(1) Errors") + 
  theme(plot.title = element_text(hjust = 0.5))
# ggsave("Mod+AR1Resid.jpg", height = 4, width = 8)

reg_dat2 <- drop_na(reg_dat) %>% select(-date, -index, -fit, -residuals)
arima(reg_dat2$CPI, order = c(1,1,0), xreg = reg_dat2[,-1])


