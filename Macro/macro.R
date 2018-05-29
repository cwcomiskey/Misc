devtools::load_all("/Users/cwcomiskey/Desktop/ODG/Macro-models/macro")

macro::depends() # load all dependencies

# f_71 (flawed, weights) =====================================
# Note: 2015 - 2016 weights
# https://www.bls.gov/cpi/tables/relative-importance/home.htm

strata_reg_dat <- CPI %>% select(date = date, CPI = value)

strata_riws_ordered <- strata_riws[match(
  names(strata70_reg_dat)[-c(1,2)],
  strata_riws$item_code),]

riws <- as.vector(t(strata_riws_ordered[,2])); rm(strata_riws_ordered)
CPI <- as.vector(strata70_reg_dat[,2])
strata <- strata70_reg_dat %>% select(-date, -CPI)

for(i in 2:100){
  if(i ==2) CPI.hat <- CPI[1]
  CPI.hat[i] <- CPI[i-1]*(sum(riws*(strata[i,]/strata[i-1,])))/100
}

1- sum( (CPI.hat - CPI)^2) / sum( (CPI - mean(CPI))^2 )

# Plot: 70 ECs ============== #

CPI <- data.frame(date = strata70_reg_dat[,"date"], 
                  value = strata70_reg_dat[,"CPI"], cat = "CPI") 
calc70 <- data.frame("date" = strata70_reg_dat[,c("date")], 
                     "value" = CPI.hat, "cat" = "RIW70")

plot_dat <- rbind.data.frame(CPI, calc70); rm(CPI, calc70)

ggplot(data = plot_dat) +
  geom_line(aes(x = date, y = value, color = cat), size = 1.75)

# ggsave("All.jpg", width = 12, height = 5)

# Plot: Changes  ===========

CPI <- data.frame(date = strata70_reg_dat$`date`[-1],  
                  value = diff(strata70_reg_dat$`CPI`), cat = "CPI") 
calc70 <- data.frame(date = strata70_reg_dat$`date`[-1], 
                     value = diff(CPI.hat), cat = "RIW70")

plot_dat2 <- rbind.data.frame(CPI, calc70); rm(CPI, calc70)

ggplot(data = plot_dat2) +
  geom_line(aes(x = date, y = value, color = cat), size = 1.75) + 
  ggtitle("Month-to-month Changes") +
  theme(plot.title = element_text(hjust = 0.5))

# ggsave("All_changes.jpg", width = 12, height = 5)

1- sum( (diff(CPI.hat) - diff(strata70_reg_dat$`CPI`))^2) / sum( (diff(strata70_reg_dat$`CPI`) - mean(diff(strata70_reg_dat$`CPI`)))^2 )
# RIW70 = 0.9445965

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
  }
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

# 2010 CPI, calculated monthly weights, Top 25 only =================
prep <- function(){
  reg_dat <- reg_dat
  w <- riws2009 %>% filter(item_code %in% names(reg_dat)); rm(reg_dat)
  s25 <- strata70_reg_dat %>% drop_na() %>% 
    filter(year(date) <  2012) %>% 
    select(one_of("date", "CPI", w$item_code)) 
  w <- w[match(names(s25[-c(1,2)]), w$item_code),]

  weights25 <- cbind.data.frame(date = s25$date, CPI = 0, matrix(ncol = 24, nrow = 25))
  colnames(weights25) <- c("date", "CPI.hat", w$item_code) 
  weights25[1,3:26] <- w$CPI_U; rm(w)
}
for(m in 2:25){
    weights25[m,3:26] <- weights25[1,3:26] * (s25[m,3:26] / s25[1,3:26]) * (s25[1,"CPI"] / s25[m,"CPI"])
    if(m == 25) {rm(m); weights25$CPI.hat[1] <- s25$CPI[1]}
    } # Calculate weights25 for CPI 

for(m in 2:25){
  weights25[m,"CPI.hat"] <- s25[m-1,"CPI"] * 
    sum(weights25[m-1,3:26] * (s25[m,3:26] / s25[m-1,3:26]))/sum(weights25[m-1,3:26])
  if(m == 25) rm(m)
} # Calculate CPI

CPIs <- data.frame(date = strata70_reg_dat$date[2:25],
                   value = diff(cpi.hat$index), Cat = "RIW70")
CPI.hats <- data.frame(date = strata70_reg_dat$date[2:25],
                       value = diff(CPI$index[1:25]), Cat = "CPI")
CPI25 <- data.frame(date = weights25$date[2:25], value = diff(weights25$CPI.hat), Cat = "RIW25")

plot_dat <- rbind.data.frame(CPIs, CPI.hats, CPI25)

ggplot(data = plot_dat) + 
  geom_line(aes(x = date, y = value, color = Cat), size = 1.5) +
  ggtitle("CPI Month-to-month Change Estimates with Calculated Weights") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave("CPI_70_25.jpg", width = 12, height = 5)


