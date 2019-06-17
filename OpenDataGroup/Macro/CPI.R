# 2010 CPI with calculated monthly weights =========================
# Create RIW df ====== #
load2009riws <- function(){
  riws2009 <- read_table("2009RIWs_0708wts") %>%
    drop_na() %>%
    mutate(Item_name = gsub("\\.*", "" , `Expenditure category`)) %>% 
    select(Item_name, CPI_U = X2) %>%
    left_join(., item_dat, by = c("Item_name" = "item_name")) %>% 
    # ...riws2009 has __(not sure)__ and item_dat == "Recorded music and music subscriptions"
    filter(display_level == 2 | 
             Item_name %in% c("Airline fare", "Cable and satellite television and radio service")) 
  # mutate(RIW_norm = CPI_U/sum(CPI_U) * 100)
  
  # Correct and add
  riws2009[riws2009$Item_name == "Airline fare", c("Item_name", "item_code")] <- c("Airline fares", "SETG01")
  riws2009[riws2009$Item_name == "Cable and satellite television and radio service", c("Item_name", "item_code")] <- c("Cable and satellite television service", "SERA02")
  riws2009[68,] <- NA 
  riws2009[68,1] <- "Recorded music and music subscriptions"
  riws2009[68,2] <- 0.638
  riws2009[68,3] <- "SERA06"                 
  riws2009[68,4] <- 2
  
  return(riws2009)
  
  # Diagnostics
  # missing <- names(strata70_reg_dat)[!(names(strata70_reg_dat) %in% riws2009$item_code)]
  # item_dat[item_dat$item_code %in% missing,]
  # unique(filter(strata_dat, item_code %in% missing)[,"item_name"])
} # Mess
# riws2009 <- load2009riws()
riws2009 <- riws2009
strata70_reg_dat <- strata70_reg_dat

w <- riws2009[match( 
  names(strata70_reg_dat)[-c(1,2)], 
  riws2009$item_code),]

s70 <- strata70_reg_dat[,-c(1,2)] # shorter name! all strata
cpi.hat <- data.frame(index = strata70_reg_dat$CPI[1]) # container
CPI <- data.frame(index = strata70_reg_dat$CPI) # CPI

for(m in 1:29){
  if(m == 1){
    weights <- as.data.frame(t(w[,"CPI_U"])) # initialize calculated weights cont.
    colnames(weights) <- w$item_code # name weights same as index names
  } else{
    if(year(strata70_reg_dat$date[m]) > 2011){rm(m); break}
    weights[m,] <- weights[1,] * (s70[m,] / s70[1,]) * (CPI[1,1] / CPI[m,1])
  }
} # Calculate weights for CPI 

for(m in 2:100){
  if(year(strata70_reg_dat$date[m]) > 2011){break; rm(m)}
  cpi.hat[m,1] <- CPI[m-1,1] * sum(weights[m-1,] * (s70[m,] / s70[m-1,]))/100
  if(m == 100) rm(m)
} # Calculate CPI

1- sum( (diff(cpi.hat$index) - diff(CPI$index[1:25]))^2) / sum( (diff(CPI$index[1:25]) - mean(diff(CPI$index[1:25])))^2 )

CPIs <- data.frame(date = strata70_reg_dat$date[1:25],
                   value = cpi.hat$index, Cat = "CPI_hat")
CPI.hats <- data.frame(date = strata70_reg_dat$date[1:25],
                       value = CPI$index[1:25], Cat = "CPI")

# CPIs <- data.frame(date = strata70_reg_dat$date[2:25],  
#                    value = diff(cpi.hat$index), Cat = "CPI_hat")
# CPI.hats <- data.frame(date = strata70_reg_dat$date[2:25], 
#                        value = diff(CPI$index[1:25]), Cat = "CPI")

plot_dat <- rbind.data.frame(CPIs, CPI.hats)

ggplot(data = plot_dat) + 
  geom_line(aes(x = date, y = value, color = Cat), size = 1.5) +
  ggtitle("CPI Month-to-month Change Estimates with Calculated Weights") +
  theme(plot.title = element_text(hjust = 0.5))

# ggsave("Weights.jpg", width = 12, height = 5)

