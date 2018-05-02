# CPI change autocorrelation =====
autoplot(acf(CPI$lag1, na.action = na.pass, plot = FALSE, lag.max = 120)) +
  ggtitle("CPI: Autocorrelation of month-to-month Changes") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("CPImtmACF.jpg", height = 4, width = 8)

# Major Group correlation matrix + plot =====

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
    
    if(i == length(all_dat) & j == length(all_dat)){
      rm(groupA, groupB, groupA2, groupB2, joined, i, j, date_intersect)
    }
    
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

# All stratum correlation matrix, top25 df =====
dat <- strata_dat # that's what I named it in package data
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
corr_matr <- left_join(corr_matr, strata_meta_dat) %>%
  select(item_name, series_id, Cor)

top25 <- corr_matr %>% arrange(desc(Cor)) %>% .[1:25,] %>% .[,-2]
top25[21,1] <- "Club membership"
